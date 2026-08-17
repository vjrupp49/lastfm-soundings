# =============================================================================
#  02_atlas_plots.R
#  Three static pieces drawn from taste_space.rds.
#
#    plate 1  ATLAS   the map itself: regions, isolines, bridge artists
#    plate 2  DRIFT   the same map with your yearly centre of gravity walking
#                     across it
#    plate 3  FLOW    directed movement between regions, observed vs. expected
#
#  Visual grammar is a bathymetric survey chart: dark water, thin isolines,
#  regions named in map-italic serif, everything numeric in mono.
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(ggforce); library(ggrepel); library(grid)
})

# ------------------------------------------------------------- account ----
# Mirrors 01_build_taste_space.R's account switch, so plots for a given
# account read that account's taste_space.rds and land in their own
# plots subfolder rather than overwriting another account's output.
#   Rscript 02_atlas_plots.R                 -> ACCOUNT = "main" (default)
#   Rscript 02_atlas_plots.R curtisschaefer   -> ACCOUNT = "curtisschaefer"
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || !nzchar(a)) b else a
.args <- commandArgs(trailingOnly = TRUE)
ACCOUNT <- (if (length(.args) >= 1) .args[1] else NA_character_) %||%
  Sys.getenv("TASTE_ACCOUNT") %||% "main"

ACCOUNTS <- list(
  main = list(taste_file = "taste_space.rds", plot_dir = "plots"),
  curtisschaefer = list(taste_file = "curtisschaefer_data/taste_space.rds",
                        plot_dir = "plots/curtisschaefer")
)
if (!ACCOUNT %in% names(ACCOUNTS)) {
  stop("Unknown ACCOUNT '", ACCOUNT, "'. Known accounts: ",
       paste(names(ACCOUNTS), collapse = ", "))
}
message("[account] plotting atlas for: ", ACCOUNT)
PLOT_DIR <- ACCOUNTS[[ACCOUNT]]$plot_dir

taste <- readRDS(ACCOUNTS[[ACCOUNT]]$taste_file)
A  <- taste$artists
CL <- taste$clusters
dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------- typography ----
FAM <- c(display = "sans", region = "serif", mono = "mono")
if (requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE)) {
  ok <- try({
    sysfonts::font_add_google("Archivo Narrow", "display")
    sysfonts::font_add_google("EB Garamond",   "region")
    sysfonts::font_add_google("IBM Plex Mono",  "mono")
    showtext::showtext_auto()
    showtext::showtext_opts(dpi = 300)
  }, silent = TRUE)
  if (!inherits(ok, "try-error")) FAM <- c(display = "display", region = "region", mono = "mono")
}

# ------------------------------------------------------------------ palette ----
PAL <- list(
  abyss   = "#08171F",   # deepest water / page
  shelf   = "#0F2C38",
  isoline = "#1F5566",
  sand    = "#E9DFCB",   # ink
  faint   = "#7C8F96",
  beacon  = "#E4572E"    # used for exactly one thing: bridge artists
)
region_cols <- grDevices::colorRampPalette(
  c("#2E7D8F", "#4FA3A5", "#9DC3A4", "#D9C68B", "#C88F6A", "#9A6B7E", "#63698F")
)(nrow(CL))

theme_chart <- function(base = 11) {
  theme_void(base_size = base, base_family = FAM["display"]) +
    theme(
      plot.background  = element_rect(fill = PAL$abyss, colour = NA),
      panel.background = element_rect(fill = PAL$abyss, colour = NA),
      plot.title    = element_text(family = FAM["display"], colour = PAL$sand,
                                   size = base * 2.4, face = "bold", hjust = 0,
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(family = FAM["mono"], colour = PAL$faint,
                                   size = base * 0.85, hjust = 0,
                                   margin = margin(b = 14), lineheight = 1.4),
      plot.caption  = element_text(family = FAM["mono"], colour = PAL$faint,
                                   size = base * 0.7, hjust = 0, lineheight = 1.5,
                                   margin = margin(t = 14)),
      plot.margin   = margin(28, 28, 22, 28),
      legend.position = "none"
    )
}

fit <- taste$fit
stamp <- sprintf(
  "%s scrobbles · %s sessions · %s\n%d artists · %d regions · modularity Q = %.3f%s",
  format(fit$n_scrobbles, big.mark = ","), format(fit$n_sessions, big.mark = ","),
  paste(format(fit$span, "%b %Y"), collapse = " – "),
  nrow(A), nrow(CL), fit$Q,
  if (is.na(fit$p)) "" else sprintf(" (permutation p ≤ %.3f)", fit$p)
)

# =============================================================== PLATE 1 ======
# Regions are found by Leiden on the cosine kNN graph; position is UMAP.
# Isolines are a 2-d kernel density over artist positions — where the library
# is thick, the water is deep.

lab <- merge(
  aggregate(cbind(x, y) ~ cluster, A, function(v) mean(v, trim = 0.15)),
  CL, by = "cluster"
)
lab$label <- ifelse(is.na(lab$tags), lab$name, lab$tags)

# name each region's three most typical artists; label a few more by weight
top_by_cluster <- do.call(rbind, lapply(split(A, A$cluster), function(g)
  g[order(-g$plays), ][seq_len(min(5, nrow(g))), ]))
bridges <- A[order(-A$bridge), ][1:18, ]

p1 <- ggplot(A, aes(x, y)) +
  geom_density_2d(colour = PAL$isoline, linewidth = 0.18, bins = 11,
                  contour_var = "ndensity") +
  geom_mark_hull(aes(group = cluster, fill = factor(cluster)),
                 concavity = 4, expand = unit(3.2, "mm"), radius = unit(3.2, "mm"),
                 alpha = 0.13, colour = NA) +
  geom_point(aes(size = plays, colour = factor(cluster)), alpha = 0.75, stroke = 0) +
  geom_point(data = bridges, shape = 21, size = 3.4, colour = PAL$beacon,
             fill = NA, stroke = 0.75) +
  geom_text(data = lab, aes(label = label), family = FAM["region"],
            fontface = "italic", colour = PAL$sand, size = 4.6, alpha = 0.9) +
  geom_text_repel(data = top_by_cluster, aes(label = artist),
                  family = FAM["display"], colour = PAL$sand, size = 2.5,
                  alpha = 0.8, max.overlaps = 40, segment.colour = PAL$isoline,
                  segment.size = 0.15, box.padding = 0.28, seed = 1) +
  geom_text_repel(data = bridges, aes(label = artist), family = FAM["mono"],
                  colour = PAL$beacon, size = 2.5, max.overlaps = 30,
                  segment.colour = PAL$beacon, segment.size = 0.2,
                  box.padding = 0.45, seed = 2) +
  scale_colour_manual(values = region_cols) +
  scale_fill_manual(values = region_cols) +
  scale_size_area(max_size = 7) +
  coord_equal(clip = "off") +
  labs(
    title = "An atlas of listening",
    subtitle = "Artists placed by who they are heard beside, not by what they are called.\nRegions are communities in the co-listening graph. Isolines are library density.",
    caption = paste0(stamp,
                     "\ncircled in orange — highest betweenness: the artists your sessions travel through to get from one region to another")
  ) +
  theme_chart()

ggsave(file.path(PLOT_DIR, "01_atlas.png"), p1, width = 16, height = 12, dpi = 300, bg = PAL$abyss)

# =============================================================== PLATE 2 ======
# Yearly play-weighted centroid in the embedding, pushed through the fitted
# UMAP model. Step length between years = how far your taste moved.

D <- taste$drift
D$step <- c(NA, sqrt(diff(D$x) ^ 2 + diff(D$y) ^ 2))

p2 <- ggplot(A, aes(x, y)) +
  geom_density_2d(colour = PAL$isoline, linewidth = 0.15, bins = 9,
                  contour_var = "ndensity") +
  geom_point(aes(colour = factor(cluster)), size = 1.5, alpha = 0.3, stroke = 0) +
  geom_text(data = lab, aes(label = label), family = FAM["region"],
            fontface = "italic", colour = PAL$sand, size = 4, alpha = 0.45) +
  geom_path(data = D, aes(x, y), colour = PAL$beacon, linewidth = 0.7,
            lineend = "round",
            arrow = arrow(length = unit(2.4, "mm"), type = "closed", ends = "last")) +
  geom_point(data = D, aes(x, y, size = scrobbles), colour = PAL$beacon) +
  geom_label_repel(data = D, aes(x, y, label = year), family = FAM["mono"],
                   size = 3, colour = PAL$abyss, fill = PAL$sand,
                   label.size = 0, label.padding = unit(1.1, "mm"),
                   box.padding = 0.6, seed = 3) +
  scale_colour_manual(values = region_cols) +
  scale_size_area(max_size = 5.5) +
  coord_equal(clip = "off") +
  labs(
    title = "Where you were standing, by year",
    subtitle = "Each marker is a year's play-weighted centre of gravity in the taste space,\nprojected onto the same map. Long steps are years your listening actually moved.",
    caption = paste0(
      "step length between consecutive years (arbitrary map units): ",
      paste(sprintf("%d→%d %.2f", head(D$year, -1), tail(D$year, -1),
                    stats::na.omit(D$step)), collapse = "   "),
      "\nmarker area ∝ scrobbles that year")
  ) +
  theme_chart()

ggsave(file.path(PLOT_DIR, "02_drift.png"), p2, width = 15, height = 11, dpi = 300, bg = PAL$abyss)

# =============================================================== PLATE 3 ======
# Co-occurrence is symmetric; movement is not. This is the first-order Markov
# chain between regions, scored against the marginal-product null, so what you
# see is "more often than chance", not "more often because both are big".

LR <- taste$flow_lr
diag(LR) <- 0                      # staying put is not a crossing
obs <- taste$flow; diag(obs) <- 0
rownames(LR) <- colnames(LR) <- CL$name

keep <- LR > 0.35 & obs >= max(5, stats::quantile(obs[obs > 0], 0.25))
Mplot <- ifelse(keep, obs, 0)

if (requireNamespace("circlize", quietly = TRUE) && sum(Mplot) > 0) {
  library(circlize)
  grid.col <- setNames(region_cols, rownames(LR))
  png(file.path(PLOT_DIR, "03_flow.png"), width = 3300, height = 3300, res = 300, bg = PAL$abyss)
  circos.clear()
  circos.par(gap.degree = 3, start.degree = 90, track.margin = c(0.01, 0.01))
  chordDiagram(
    Mplot, directional = 1, direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow", diffHeight = -0.04,
    grid.col = grid.col, transparency = 0.28,
    annotationTrack = "grid", preAllocateTracks = list(track.height = 0.14),
    link.sort = TRUE, link.largest.ontop = TRUE
  )
  circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1] + 0.6, CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
                cex = 0.55, col = PAL$sand, family = FAM["display"])
  })
  title(main = "How you cross between regions",
        col.main = PAL$sand, family = FAM["display"], cex.main = 1.6, line = -1.2)
  mtext(paste0("first-order transitions between consecutive tracks in a session.\n",
               "only links occurring at least 2^0.35 times more often than the ",
               "marginal-product null are drawn.\narrow direction = order of play; ",
               "within-region moves omitted."),
        side = 1, col = PAL$faint, family = FAM["mono"], cex = 0.6, line = 0.5)
  circos.clear()
  dev.off()
} else {
  message("skipping plate 3: install.packages('circlize'), or no links passed the threshold")
}

message("wrote ", file.path(PLOT_DIR, "01_atlas.png"), ", ",
        file.path(PLOT_DIR, "02_drift.png"), ", ", file.path(PLOT_DIR, "03_flow.png"))

