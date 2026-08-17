# ==========================================================
# 00_config.R  --  settings shared by every other script
# Run this first in each session (the other scripts source it).
# ==========================================================

# ---- Credentials -----------------------------------------
# Set LASTFM_API_KEY in your .Renviron
# (usethis::edit_r_environ(), then restart R).
API_KEY  <- Sys.getenv("LASTFM_API_KEY")
if (identical(API_KEY, "")) stop("Set LASTFM_API_KEY as an environment variable before running.")
USERNAME <- "curtisschaefer"
USER_AGENT_STRING <- "R-listening-halflife/1.0 (personal analysis)"

# ---- Folders ---------------------------------------------
DATA_DIR    <- "data"
RESULTS_DIR <- "results"
FIG_DIR     <- "figures"

for (d in c(DATA_DIR, RESULTS_DIR, FIG_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

RAW_FILE     <- file.path(DATA_DIR, "raw_scrobbles.rds")
MONTHLY_FILE <- file.path(DATA_DIR, "artist_month.rds")
HL_FILE      <- file.path(RESULTS_DIR, "half_lives.csv")
TAGS_FILE    <- file.path(DATA_DIR, "artist_tags.rds")

# ---- Analysis tuning -------------------------------------
MIN_PLAYS        <- 30   # ignore artists with fewer total scrobbles
MIN_PEAK_RATIO   <- 2    # peak month must be >= 2x that artist's median month
MIN_MONTHS_AFTER <- 6    # need >= 6 months of runway after the peak to judge decay
SMOOTH_WIDTH     <- 3    # centred rolling mean, in months
TAIL_CONFIRM     <- 3    # months it must STAY below half to count as decayed
PLOT_HORIZON     <- 24   # months after peak shown on the decay chart

# Artists drawn in bold colour on the decay chart.
# Leave as NULL to auto-pick your 6 most-played qualifying artists,
# or set e.g. HIGHLIGHT_ARTISTS <- c("Radiohead", "Fleet Foxes")
HIGHLIGHT_ARTISTS <- NULL

# ---- Tiny helper ------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

message("Config loaded. User: ", USERNAME)
