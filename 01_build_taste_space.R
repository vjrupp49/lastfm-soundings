# =============================================================================
#  01_build_taste_space.R
#  Builds a "taste space" from Last.fm scrobbles: a vector space in which two
#  artists are close if you listen to them near each other in time.
#
#  Pipeline
#    scrobbles -> sessions -> distance-weighted co-occurrence
#              -> PPMI (with context distribution smoothing)
#              -> truncated SVD  (Levy & Goldberg 2014: SVD of shifted PPMI is
#                 a matrix-factorisation equivalent of skip-gram w/ neg. sampling)
#              -> cosine kNN graph -> Leiden communities
#              -> UMAP for display only (never for the maths)
#
#  Everything downstream (plots, app) reads the single .rds this writes.
#  Runtime: minutes, dominated by the permutation null. That's fine.
# =============================================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(irlba)
  library(igraph)
  library(uwot)
})

# ------------------------------------------------------------- account ----
# Which Last.fm account this run builds a taste space for. Controls where
# the scrobbles are read from and where the output .rds is written, so
# different accounts' outputs never collide.
#   Rscript 01_build_taste_space.R                 -> ACCOUNT = "main" (default)
#   Rscript 01_build_taste_space.R curtisschaefer   -> ACCOUNT = "curtisschaefer"
#   TASTE_ACCOUNT=curtisschaefer Rscript 01_build_taste_space.R
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || !nzchar(a)) b else a
.args <- commandArgs(trailingOnly = TRUE)
ACCOUNT <- (if (length(.args) >= 1) .args[1] else NA_character_) %||%
  Sys.getenv("TASTE_ACCOUNT") %||% "main"

ACCOUNTS <- list(
  main = list(
    data_file = "data/raw_scrobbles.rds",
    out       = "taste_space.rds"
  ),
  curtisschaefer = list(
    data_file = "curtisschaefer_data/curtisschaefer_raw_scrobbles.rds",
    out       = "curtisschaefer_data/taste_space.rds"
  )
)
if (!ACCOUNT %in% names(ACCOUNTS)) {
  stop("Unknown ACCOUNT '", ACCOUNT, "'. Known accounts: ",
       paste(names(ACCOUNTS), collapse = ", "))
}
message("[account] building taste space for: ", ACCOUNT)

# ---------------------------------------------------------------- config ----
CFG <- list(
  # --- input -------------------------------------------------------------
  data_file       = ACCOUNTS[[ACCOUNT]]$data_file,  # cols: artist, track, timestamp
  fetch           = FALSE,                 # TRUE = pull from Last.fm API first
  lastfm_user     = Sys.getenv("LASTFM_USER"),
  lastfm_key      = Sys.getenv("LASTFM_API_KEY"),
  fetch_tags      = FALSE,                 # optional: tag-based cluster labels
  
  # --- sessionisation ----------------------------------------------------
  session_gap_min = 30,    # a gap this long ends a session
  window          = 5,     # co-occurrence window, in scrobbles, each side
  
  # --- vocabulary --------------------------------------------------------
  min_plays       = 25,
  max_artists     = 1500,
  
  # --- embedding ---------------------------------------------------------
  ppmi_alpha      = 0.75,  # context distribution smoothing
  ppmi_shift      = 1,     # k in SPPMI; 1 = plain PPMI
  dims            = 128,
  eig_power       = 0.5,   # symmetric eigenvalue weighting U * S^p
  
  # --- graph / communities ----------------------------------------------
  knn             = 15,
  leiden_res      = 1.0,
  min_cluster     = 5,     # smaller communities get absorbed
  n_null          = 10,    # permutation null for modularity (0 to skip)
  
  # --- display -----------------------------------------------------------
  umap_neighbors  = 15,
  umap_min_dist   = 0.12,
  
  seed            = 20260730,
  out             = ACCOUNTS[[ACCOUNT]]$out
)

set.seed(CFG$seed)
msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

# ------------------------------------------------------------ 1. ingest ----

fetch_scrobbles <- function(user, key, cache = "data/scrobbles.csv", pause = 0.2) {
  stopifnot(nzchar(user), nzchar(key))
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("install.packages('jsonlite')")
  dir.create(dirname(cache), showWarnings = FALSE, recursive = TRUE)
  base <- "https://ws.audioscrobbler.com/2.0/"
  
  get_page <- function(p) {
    url <- sprintf(
      "%s?method=user.getrecenttracks&user=%s&api_key=%s&format=json&limit=200&page=%d",
      base, utils::URLencode(user, reserved = TRUE), key, p
    )
    for (attempt in 1:4) {
      r <- try(jsonlite::fromJSON(url), silent = TRUE)
      if (!inherits(r, "try-error")) return(r)
      Sys.sleep(2 ^ attempt)
    }
    stop("Last.fm request failed at page ", p)
  }
  
  parse_page <- function(pg) {
    tr <- pg$recenttracks$track
    if (is.null(tr) || !is.data.frame(tr) || nrow(tr) == 0) return(NULL)
    uts <- if (!is.null(tr$date)) suppressWarnings(as.numeric(tr$date$uts))
    else rep(NA_real_, nrow(tr))
    data.frame(
      artist    = as.character(tr$artist[["#text"]]),
      track     = as.character(tr$name),
      timestamp = uts,
      stringsAsFactors = FALSE
    )
  }
  
  first <- get_page(1)
  total <- as.integer(first$recenttracks$`@attr`$totalPages)
  msg("fetching ", total, " pages of scrobbles")
  out <- vector("list", total)
  out[[1]] <- parse_page(first)
  for (p in seq_len(total)[-1]) {
    out[[p]] <- parse_page(get_page(p))
    if (p %% 25 == 0) msg("  page ", p, "/", total)
    Sys.sleep(pause)
  }
  d <- do.call(rbind, out)
  d <- d[!is.na(d$timestamp), ]                 # drops the "now playing" row
  d <- d[!duplicated(d[c("artist", "track", "timestamp")]), ]
  utils::write.csv(d, cache, row.names = FALSE)
  msg("wrote ", nrow(d), " scrobbles -> ", cache)
  d
}

load_scrobbles <- function(path) {
  ext <- tolower(tools::file_ext(path))
  d <- switch(ext,
              rds = readRDS(path),
              csv = utils::read.csv(path, stringsAsFactors = FALSE),
              tsv = utils::read.delim(path, stringsAsFactors = FALSE),
              utils::read.csv(path, stringsAsFactors = FALSE))
  names(d) <- tolower(names(d))
  
  # be forgiving about column naming across exports
  acol <- intersect(c("artist", "artist_name", "artistname"), names(d))[1]
  tcol <- intersect(c("timestamp", "uts", "date", "utc_time", "time", "played_at"), names(d))[1]
  if (is.na(acol) || is.na(tcol)) stop("need an artist column and a timestamp column")
  
  ts <- d[[tcol]]
  d$ts <- if (inherits(ts, "POSIXct")) ts
  else if (is.numeric(ts)) as.POSIXct(ts, origin = "1970-01-01", tz = "UTC")
  else as.POSIXct(ts, tz = "UTC")
  d$artist <- trimws(as.character(d[[acol]]))
  d <- d[!is.na(d$ts) & nzchar(d$artist), c("artist", "ts")]
  d[order(d$ts), ]
}

d <- if (isTRUE(CFG$fetch)) {
  fetch_scrobbles(CFG$lastfm_user, CFG$lastfm_key, CFG$data_file); load_scrobbles(CFG$data_file)
} else load_scrobbles(CFG$data_file)

msg(nrow(d), " scrobbles, ", format(min(d$ts), "%Y-%m-%d"), " to ", format(max(d$ts), "%Y-%m-%d"))

# ------------------------------------------------------ 2. sessionisation ----
n <- nrow(d)
gap_min <- c(Inf, as.numeric(difftime(d$ts[-1], d$ts[-n], units = "mins")))
d$session <- cumsum(gap_min > CFG$session_gap_min)
msg(length(unique(d$session)), " sessions, median length ",
    round(stats::median(as.numeric(table(d$session)))), " tracks")

# ----------------------------------------------------------- 3. vocabulary ----
d$key <- tolower(d$artist)
tab <- sort(table(d$key), decreasing = TRUE)
vocab_key <- head(names(tab)[tab >= CFG$min_plays], CFG$max_artists)

# canonical display name = most common capitalisation seen
disp <- tapply(d$artist, d$key, function(x) names(sort(table(x), decreasing = TRUE))[1])
vocab <- unname(disp[vocab_key])

d$idx <- match(d$key, vocab_key)
seqd  <- d[!is.na(d$idx), ]
V <- length(vocab_key)
msg("vocabulary: ", V, " artists covering ",
    round(100 * nrow(seqd) / nrow(d), 1), "% of scrobbles")

# ------------------------------------------------- 4. co-occurrence -> PPMI ----
build_cooc <- function(idx, sess, w, V) {
  m <- length(idx); I <- J <- integer(0); X <- numeric(0)
  for (dd in seq_len(w)) {
    a <- seq_len(m - dd); b <- a + dd
    ok <- sess[a] == sess[b]
    if (!any(ok)) next
    ia <- idx[a][ok]; ib <- idx[b][ok]
    I <- c(I, ia, ib); J <- c(J, ib, ia)
    X <- c(X, rep(1 / dd, 2 * length(ia)))     # harmonic distance weighting
  }
  M <- sparseMatrix(i = I, j = J, x = X, dims = c(V, V))
  diag(M) <- 0                                  # self-pairs carry no information
  drop0(M)
}

ppmi <- function(C, alpha = 0.75, k = 1) {
  N  <- sum(C@x)
  pw <- rowSums(C) / N
  pc <- colSums(C) ^ alpha; pc <- pc / sum(pc)  # context distribution smoothing
  Tm <- as(C, "TsparseMatrix")
  v  <- log((Tm@x / N) / (pw[Tm@i + 1] * pc[Tm@j + 1])) - log(k)
  ok <- is.finite(v) & v > 0
  sparseMatrix(i = Tm@i[ok] + 1, j = Tm@j[ok] + 1, x = v[ok], dims = dim(C))
}

embed <- function(M, dims, p) {
  sv <- irlba(M, nv = min(dims, min(dim(M)) - 2))
  E  <- sv$u %*% diag(sv$d ^ p)
  E / pmax(sqrt(rowSums(E ^ 2)), 1e-9)          # L2 -> dot product is cosine
}

msg("building co-occurrence")
C <- build_cooc(seqd$idx, seqd$session, CFG$window, V)

# an artist with no in-session neighbours can't be placed; drop and re-index
alive <- rowSums(C) > 0
if (any(!alive)) {
  msg("dropping ", sum(!alive), " artists with no co-listening context")
  keep <- which(alive)
  C <- C[keep, keep]; vocab <- vocab[keep]; vocab_key <- vocab_key[keep]
  seqd$idx <- match(seqd$idx, keep); seqd <- seqd[!is.na(seqd$idx), ]
  V <- length(vocab)
}

msg("PPMI + SVD")
M <- ppmi(C, CFG$ppmi_alpha, CFG$ppmi_shift)
E <- embed(M, CFG$dims, CFG$eig_power)
rownames(E) <- vocab

S <- tcrossprod(E)                              # cosine similarity, V x V
dimnames(S) <- list(vocab, vocab)

# ------------------------------------------------------ 5. kNN graph, Leiden ----
knn_graph <- function(S, k) {
  V <- nrow(S); Sx <- S; diag(Sx) <- -Inf
  nb <- t(apply(Sx, 1, function(r) order(r, decreasing = TRUE)[seq_len(k)]))
  el <- data.frame(
    from = rep(seq_len(V), each = k),
    to   = as.vector(t(nb)),
    weight = Sx[cbind(rep(seq_len(V), each = k), as.vector(t(nb)))]
  )
  el <- el[el$weight > 0, ]
  el[, c("from", "to")] <- t(apply(el[, c("from", "to")], 1, sort))
  el <- stats::aggregate(weight ~ from + to, el, max)          # union kNN
  g <- graph_from_data_frame(el, directed = FALSE,
                             vertices = data.frame(name = as.character(seq_len(V))))
  E(g)$weight <- el$weight
  g
}

cluster_once <- function(g, res) {
  cl <- cluster_leiden(g, objective_function = "modularity",
                       weights = E(g)$weight, resolution_parameter = res,
                       n_iterations = 5)
  as.integer(membership(cl))
}

msg("kNN graph + Leiden")
g <- knn_graph(S, CFG$knn)
memb <- cluster_once(g, CFG$leiden_res)
Q_obs <- modularity(g, memb, weights = E(g)$weight)

# absorb tiny communities into whichever community they're most similar to
repeat {
  sizes <- table(memb)
  small <- as.integer(names(sizes)[sizes < CFG$min_cluster])
  if (!length(small)) break
  big <- setdiff(unique(memb), small)
  cent <- sapply(big, function(k) colMeans(E[memb == k, , drop = FALSE]))
  for (i in which(memb %in% small)) memb[i] <- big[which.max(E[i, ] %*% cent)]
  if (length(unique(memb)) == length(big)) break
}
memb <- as.integer(factor(memb))                # renumber 1..K
K <- max(memb)
msg(K, " communities, modularity Q = ", round(Q_obs, 3))

# --- permutation null -------------------------------------------------------
# H0: co-listening carries no artist-specific structure. Permuting the artist
# labels across the whole scrobble sequence preserves every play count and
# every session length exactly, and destroys only *who sits next to whom*.
Q_null <- numeric(0)
if (CFG$n_null > 0) {
  msg("permutation null, ", CFG$n_null, " replicates")
  for (b in seq_len(CFG$n_null)) {
    sh <- sample(seqd$idx)
    Cb <- build_cooc(sh, seqd$session, CFG$window, V)
    ab <- rowSums(Cb) > 0
    Cb <- Cb[ab, ab]
    Eb <- embed(ppmi(Cb, CFG$ppmi_alpha, CFG$ppmi_shift), CFG$dims, CFG$eig_power)
    gb <- knn_graph(tcrossprod(Eb), CFG$knn)
    mb <- cluster_once(gb, CFG$leiden_res)
    Q_null[b] <- modularity(gb, mb, weights = E(gb)$weight)
    msg("  null ", b, ": Q = ", round(Q_null[b], 3))
  }
  p_val <- (1 + sum(Q_null >= Q_obs)) / (CFG$n_null + 1)
  msg("Q_obs = ", round(Q_obs, 3), " vs null ", round(mean(Q_null), 3),
      " +/- ", round(stats::sd(Q_null), 3), "   p <= ", signif(p_val, 3))
} else p_val <- NA_real_

# ------------------------------------------------ 6. per-artist statistics ----
plays <- as.integer(table(factor(seqd$idx, levels = seq_len(V))))

# bridges: betweenness on the similarity graph, distances = 1 - cosine
bridge <- betweenness(g, weights = pmax(1 - E(g)$weight, 1e-6), normalized = TRUE)

# context entropy: how many different communities show up around this artist.
# 0 = only ever heard inside one neighbourhood, 1 = heard everywhere.
onehot <- sparseMatrix(i = seq_len(V), j = memb, x = 1, dims = c(V, K))
P <- as.matrix(C %*% onehot)
P <- P / pmax(rowSums(P), 1e-12)
ctx_entropy <- apply(P, 1, function(p) { p <- p[p > 0]; -sum(p * log(p)) }) / log(K)

# how "typical" of its own community an artist is (centrality within cluster)
typicality <- sapply(seq_len(V), function(i) {
  same <- memb == memb[i]; same[i] <- FALSE
  if (!any(same)) return(0)
  mean(S[i, same]) - mean(S[i, !same & seq_len(V) != i])
})

# ------------------------------------------------- 7. naming the communities ----
core_artists <- lapply(seq_len(K), function(k) {
  ix <- which(memb == k)
  vocab[ix[order(-typicality[ix])][seq_len(min(6, length(ix)))]]
})
cluster_name <- sapply(core_artists, function(a) paste(a[1:min(3, length(a))], collapse = " / "))

tag_labels <- NULL
if (isTRUE(CFG$fetch_tags) && nzchar(CFG$lastfm_key) &&
    requireNamespace("jsonlite", quietly = TRUE)) {
  msg("fetching artist tags for cluster labels")
  cache <- "data/tags.rds"
  tags <- if (file.exists(cache)) readRDS(cache) else list()
  for (i in seq_len(V)) {
    a <- vocab[i]
    if (!is.null(tags[[a]])) next
    url <- sprintf("%s?method=artist.gettoptags&artist=%s&api_key=%s&format=json&autocorrect=1",
                   "https://ws.audioscrobbler.com/2.0/",
                   utils::URLencode(a, reserved = TRUE), CFG$lastfm_key)
    r <- try(jsonlite::fromJSON(url), silent = TRUE)
    tg <- if (!inherits(r, "try-error") && is.data.frame(r$toptags$tag)) {
      utils::head(r$toptags$tag, 8)
    } else NULL
    tags[[a]] <- if (is.null(tg)) character(0) else
      stats::setNames(as.numeric(tg$count), tolower(tg$name))
    Sys.sleep(0.2)
    if (i %% 100 == 0) { saveRDS(tags, cache); msg("  ", i, "/", V) }
  }
  saveRDS(tags, cache)
  
  all_tags <- sort(unique(unlist(lapply(tags, names))))
  Tm <- matrix(0, K, length(all_tags), dimnames = list(NULL, all_tags))
  for (i in seq_len(V)) {
    tg <- tags[[vocab[i]]]
    if (length(tg)) Tm[memb[i], names(tg)] <- Tm[memb[i], names(tg)] + tg / max(tg)
  }
  tf  <- Tm / pmax(rowSums(Tm), 1e-9)
  idf <- log(K / pmax(colSums(Tm > 0), 1))       # distinctive, not just frequent
  tfidf <- sweep(tf, 2, idf, "*")
  tag_labels <- apply(tfidf, 1, function(r) paste(names(sort(r, decreasing = TRUE))[1:3], collapse = " · "))
}

# ---------------------------------------------------- 8. UMAP (display only) ----
msg("UMAP")
um <- umap(E, n_neighbors = CFG$umap_neighbors, min_dist = CFG$umap_min_dist,
           metric = "cosine", n_components = 2, ret_model = TRUE, verbose = FALSE)
xy <- um$embedding
colnames(xy) <- c("x", "y")

# --------------------------------------------- 9. time: drift and sparklines ----
seqd$year  <- as.integer(format(seqd$ts, "%Y"))
seqd$month <- format(seqd$ts, "%Y-%m")

months <- format(seq(as.Date(format(min(seqd$ts), "%Y-%m-01")),
                     as.Date(format(max(seqd$ts), "%Y-%m-01")), by = "month"), "%Y-%m")
monthly <- sparseMatrix(
  i = seqd$idx, j = match(seqd$month, months), x = 1,
  dims = c(V, length(months)), dimnames = list(vocab, months)
)

# yearly centre of gravity: play-weighted mean position in the *embedding*,
# then pushed through the fitted UMAP model so it lands on the same picture.
years <- sort(unique(seqd$year))
cent_E <- t(sapply(years, function(y) {
  w <- as.numeric(table(factor(seqd$idx[seqd$year == y], levels = seq_len(V))))
  v <- colSums(E * w) / sum(w)
  v / max(sqrt(sum(v ^ 2)), 1e-9)
}))
drift <- as.data.frame(umap_transform(cent_E, um))
names(drift) <- c("x", "y"); drift$year <- years
drift$scrobbles <- as.integer(table(factor(seqd$year, levels = years)))

# ------------------------------------------- 10. directed flow between regions ----
nx <- seq_len(nrow(seqd) - 1)
ok <- seqd$session[nx] == seqd$session[nx + 1]
from <- memb[seqd$idx[nx][ok]]; to <- memb[seqd$idx[nx + 1][ok]]
Fm <- matrix(0, K, K)
tb <- table(from, to)
Fm[cbind(as.integer(rownames(tb))[row(tb)], as.integer(colnames(tb))[col(tb)])] <- as.vector(tb)
Exp <- outer(rowSums(Fm), colSums(Fm)) / sum(Fm)
flow_lr <- log2((Fm + 0.5) / (Exp + 0.5))       # observed vs. marginal expectation

first_of_session <- tapply(seqd$idx, seqd$session, function(v) v[1])
openers <- as.integer(table(factor(memb[first_of_session], levels = seq_len(K))))

# ------------------------------------------------ 11. nearest neighbour lists ----
Sx <- S; diag(Sx) <- -Inf
nn <- t(apply(Sx, 1, function(r) order(r, decreasing = TRUE)[1:12]))
nn_sim <- t(sapply(seq_len(V), function(i) Sx[i, nn[i, ]]))

# ------------------------------------------------------------------ save ----
artists <- data.frame(
  idx = seq_len(V), artist = vocab, cluster = memb,
  x = xy[, 1], y = xy[, 2], plays = plays,
  bridge = bridge, ctx_entropy = ctx_entropy, typicality = typicality,
  first_seen = as.Date(tapply(seqd$ts, seqd$idx, min), origin = "1970-01-01"),
  last_seen  = as.Date(tapply(seqd$ts, seqd$idx, max), origin = "1970-01-01"),
  stringsAsFactors = FALSE
)

taste <- list(
  cfg = CFG, artists = artists, E = E, S = S, umap_model = um,
  clusters = data.frame(cluster = seq_len(K),
                        name = cluster_name,
                        tags = if (is.null(tag_labels)) NA_character_ else tag_labels,
                        size = as.integer(table(memb)),
                        plays = as.integer(tapply(plays, memb, sum)),
                        openers = openers,
                        stringsAsFactors = FALSE),
  core_artists = core_artists,
  graph = g, cooc = C, monthly = monthly, months = months,
  drift = drift, flow = Fm, flow_lr = flow_lr,
  nn = nn, nn_sim = nn_sim,
  fit = list(Q = Q_obs, Q_null = Q_null, p = p_val,
             span = range(d$ts), n_scrobbles = nrow(d),
             n_sessions = length(unique(d$session)))
)

saveRDS(taste, CFG$out)
msg("wrote ", CFG$out)
msg("next: Rscript 02_atlas_plots.R", if (ACCOUNT != "main") paste0(" ", ACCOUNT) else "")

