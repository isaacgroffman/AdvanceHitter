# ============================================================================
# HITTER SCOUTING CARDS APP - Streamlined Version
# With MAC (Matchup Analysis using Clustering) Methodology
# Based on: "Using Euclidean Distance and Clustering to quantify matchups"
# by Chap Cunningham and Zack Aisen (Saber Seminar 2025)
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(scales)
library(arrow)
library(httr)
library(xgboost)  # For xRV predictive model

# Suppress jsonlite warnings about named vectors (cosmetic, doesn't affect functionality)
options(shiny.sanitize.errors = FALSE)

PASSWORD <- Sys.getenv("password")

# ---------------------------------------------------------------------------
# HUGGING FACE DATASET CONFIG
# ---------------------------------------------------------------------------

#repo_id <- "CoastalBaseball/AdvanceHitter"
#hf_token <- Sys.getenv("advance_hitter_reader")
#if (identical(hf_token, "")) {
#  stop("HF token not found. Add a Space secret named 'advance_hitter_reader'.")
#}

#hf_cache_dir <- function() {
#  d <- if (dir.exists("/data")) "/data/hf_cache" else "/tmp/hf_cache"
#  dir.create(d, recursive = TRUE, showWarnings = FALSE)
#  d
#}

#hf_download_file_cached <- function(repo_id, filename) {
#  url  <- paste0(
#    "https://huggingface.co/datasets/",
#    repo_id,
#    "/resolve/main/",
#    filename
#  )
#  dest <- file.path(hf_cache_dir(), filename)

 # if (file.exists(dest) && file.info(dest)$size > 0) return(dest)

#  cat("Downloading", filename, "from Hugging Face...\n")
#  resp <- GET(url, add_headers(
#    Authorization = paste("Bearer", hf_token)
#  ))

#  if (status_code(resp) != 200) {
#    stop(
#      paste0(
#        "Failed to download ", filename,
#        " (status ", status_code(resp), ")."
#      )
#    )
#  }

#  writeBin(content(resp, "raw"), dest)
#  dest
#}


download_private_parquet <- function(repo_id, filename) {
  url <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", filename)
  
  api_key <- Sys.getenv("HF_Token")
  
  if (api_key == "") {
    stop("API key is not set.")
  }
  
  response <- GET(url, add_headers(Authorization = paste("Bearer", api_key)))
  
  if (status_code(response) == 200) {
    temp_file <- tempfile(fileext = ".parquet")
    
    writeBin(content(response, "raw"), temp_file)
    
    data <- read_parquet(temp_file)
    
    return(data)
  } else {
    stop(paste("Failed to download dataset. Status code:", status_code(response)))
  }
}





# Small helper: collect arrow queries to a real data.frame when needed
collect_df <- function(x) {
  as.data.frame(dplyr::collect(x))
}

# Debounce helper for optimized note-taking
debounce_value <- function(value, millis = 500) {
  shiny::debounce(value, millis)
}

# ============================================================================
# DATA LOADING - MEMORY OPTIMIZED
# ============================================================================

cat("Loading TrackMan data from Hugging Face (Parquet / Arrow)...\n")

# Force garbage collection before loading
gc(verbose = FALSE, full = TRUE)

# ---------------------------------------------------------------------------
# DOWNLOAD + OPEN PARQUET (LAZY)
# ---------------------------------------------------------------------------

#parquet_path <- hf_download_file_cached(
#  repo_id  = repo_id,
#  filename = "new_college_data25.parquet"
#)

#tm_data <- arrow::open_dataset(
#  parquet_path,
#  format = "parquet"
#)


tm_data <- download_private_parquet("CoastalBaseball/AdvanceHitter", "new_college_data25.parquet")


# SEC Teams for percentile comparison pool
sec_teams <- c("ALA_CRI", "ARK_RAZ", "AUB_TIG", "FLA_GAT", "GEO_BUL", "KEN_WIL", "LSU_TIG", "OLE_REB",
               "MSU_BDG", "MIZ_TIG", "SOU_GAM", "TEN_VOL", "TEX_AGG", "VAN_COM", "OKL_SOO", "TEX_LON")

# Define required columns for the app (minimized for memory efficiency)
required_cols <- c(
  # Core identifiers
  "Pitcher", "Batter", "PitcherThrows", "BatterSide", "BatterTeam",
  # Pitch info
  "TaggedPitchType", "PitchCall", "PlayResult",
  # Location data
  "PlateLocSide", "PlateLocHeight",
  # Batted ball data
  "ExitSpeed", "Angle", "Distance", "Bearing",
  # Count info
  "Balls", "Strikes", "Date",
  # Pre-calculated metrics (from TM2025)
  "mean_DRE_bat", "woba", "wobacon", "slg",
  "is_put_away", "is_walk", "is_ab", "is_hit",
  "in_zone", "in_zone_whiff", "is_whiff", "is_swing", "chase", "is_pa", "is_k",
  # Pitch characteristics (for MAC matchup)
  "RelSpeed", "InducedVertBreak", "HorzBreak", "SpinRate", "RelHeight", "RelSide"
)

# Helper function to select only existing columns
select_existing_cols <- function(df, cols) {
  existing <- cols[cols %in% names(df)]
  df[, existing, drop = FALSE]
}

if (exists("TM2025")) {
  cat("Loading from TM2025...\n")
  tm_data <- select_existing_cols(TM2025, required_cols)
  # Remove original to save memory
  if (!identical(tm_data, TM2025)) rm(TM2025)
} else if (exists("TM25")) {
  cat("Loading from TM25...\n")
  tm_data <- select_existing_cols(TM25, required_cols)
  if (!identical(tm_data, TM25)) rm(TM25)
} else {
  cat("Creating sample data for demonstration.\n")
  set.seed(42)
  n <- 5000
  pitchers <- paste0("Pitcher_", LETTERS[1:10])
  hitters <- paste0("Hitter_", 1:30)
  pitch_types <- c("Fastball", "Sinker", "Slider", "Curveball", "Changeup", "Cutter")
  

cat("Loaded", nrow(tm_data), "rows\n")

if (median(abs(tm_data$PlateLocSide), na.rm = TRUE) > 5) {
  tm_data <- tm_data %>% mutate(PlateLocSide = PlateLocSide / 12, PlateLocHeight = PlateLocHeight / 12)
}

# Benchmarks for new metrics
benchmarks <- list(
  rv100 = 0, woba = 0.320, wobacon = 0.370,
  k_pct = 22.0, bb_pct = 9.0, whiff_pct = 25.0, chase_pct = 28.0,
  z_swing = 68.0, z_contact = 85.0, put_away_pct = 25.0,
  ev_mean = 88.0, ev90 = 100.0, hard_hit_pct = 35.0,
  fp_swing = 30.0, swing_pct = 48.0, pull_pct = 40.0, oppo_pct = 22.0,
  gb_pct = 45.0, ld_pct = 22.0, fb_pct = 33.0
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

classify_pitch_family <- function(pitch_type) {
  case_when(
    pitch_type %in% c("Fastball", "Four-Seam", "FourSeamFastBall", "TwoSeamFastBall", 
                      "Sinker", "FF", "SI", "FB", "FT") ~ "FB",
    pitch_type %in% c("Slider", "Curveball", "Sweeper", "Cutter", "Slurve",
                      "CU", "SL", "FC", "SW", "KC", "CB", "SV") ~ "BB",
    pitch_type %in% c("Changeup", "ChangeUp", "Splitter", "CH", "FS", "SP") ~ "OS",
    TRUE ~ "Other"
  )
}

add_indicators <- function(df) {
  df <- df %>% mutate(PitchFamily = classify_pitch_family(TaggedPitchType))
  
  # Use TM2025 columns if available, otherwise create fallbacks
  if (!"is_swing" %in% names(df)) {
    df <- df %>% mutate(is_swing = as.numeric(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay")))
  }
  if (!"is_whiff" %in% names(df)) {
    df <- df %>% mutate(is_whiff = as.numeric(PitchCall == "StrikeSwinging"))
  }
  if (!"in_zone" %in% names(df)) {
    df <- df %>% mutate(in_zone = as.numeric(PlateLocSide >= -0.83 & PlateLocSide <= 0.83 &
                                              PlateLocHeight >= 1.5 & PlateLocHeight <= 3.5))
  }
  if (!"chase" %in% names(df)) {
    df <- df %>% mutate(chase = as.numeric(is_swing == 1 & in_zone == 0))
  }
  if (!"in_zone_whiff" %in% names(df)) {
    df <- df %>% mutate(in_zone_whiff = as.numeric(is_whiff == 1 & in_zone == 1))
  }
  if (!"is_hit" %in% names(df)) {
    df <- df %>% mutate(is_hit = as.numeric(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")))
  }
  if (!"is_ab" %in% names(df)) {
    df <- df %>% mutate(is_ab = as.numeric(PlayResult %in% c("Out", "FieldersChoice", "Single", "Double", "Triple", "HomeRun")))
  }
  if (!"is_pa" %in% names(df)) {
    df <- df %>% mutate(is_pa = as.numeric(PitchCall == "InPlay" | is_k == 1 | is_walk == 1))
  }
  if (!"is_k" %in% names(df)) {
    df <- df %>% mutate(is_k = as.numeric(is_put_away == 1))
  }
  if (!"is_walk" %in% names(df)) {
    df <- df %>% mutate(is_walk = 0)
  }
  if (!"mean_DRE_bat" %in% names(df)) {
    df <- df %>% mutate(mean_DRE_bat = 0)
  }
  if (!"woba" %in% names(df)) {
    df <- df %>% mutate(woba = NA_real_)
  }
  if (!"wobacon" %in% names(df)) {
    df <- df %>% mutate(wobacon = NA_real_)
  }
  if (!"slg" %in% names(df)) {
    df <- df %>% mutate(slg = NA_real_)
  }
  if (!"Date" %in% names(df)) {
    df <- df %>% mutate(Date = Sys.Date())
  }
  if (!"BatterTeam" %in% names(df)) {
    df <- df %>% mutate(BatterTeam = "UNKNOWN")
  }
  
  df <- df %>% mutate(
    TwoStrikeInd = ifelse(Strikes == 2, 1, 0),
    out_of_zone = 1 - in_zone,
    z_swing = as.numeric(in_zone == 1 & is_swing == 1),
    count_cat = case_when(Balls == 0 & Strikes == 0 ~ "First Pitch", Strikes > Balls ~ "Ahead",
                          Balls > Strikes ~ "Behind", Strikes == 2 ~ "Two Strike", TRUE ~ "Even"),
    two_strike = Strikes == 2
  )
  df
}

tm_data <- add_indicators(tm_data)

# Ensure all required columns exist in tm_data with proper defaults
# This fixes the ifelse() bug where scalar conditions return only the first element
if (!"mean_DRE_bat" %in% names(tm_data)) tm_data$mean_DRE_bat <- 0
if (!"mean_DRE_pit" %in% names(tm_data)) tm_data$mean_DRE_pit <- 0  # Pitcher run value
if (!"RelSpeed" %in% names(tm_data)) tm_data$RelSpeed <- 85
if (!"InducedVertBreak" %in% names(tm_data)) tm_data$InducedVertBreak <- 12
if (!"HorzBreak" %in% names(tm_data)) tm_data$HorzBreak <- 0
if (!"SpinRate" %in% names(tm_data)) tm_data$SpinRate <- 2200
if (!"RelHeight" %in% names(tm_data)) tm_data$RelHeight <- 6
if (!"RelSide" %in% names(tm_data)) tm_data$RelSide <- 0

all_hitters <- sort(unique(tm_data$Batter))

# Create SEC pool for percentile comparisons (optimized - select only needed columns)
sec_pool_cols <- c("Batter", "PitcherThrows", "BatterTeam", "TaggedPitchType", "PitchCall", "PlayResult",
                   "PlateLocSide", "PlateLocHeight", "ExitSpeed", "Angle", "Distance", "Bearing",
                   "Balls", "Strikes", "Date", "mean_DRE_bat", "woba", "wobacon", "slg",
                   "is_put_away", "is_walk", "is_ab", "is_hit", "in_zone", "in_zone_whiff", 
                   "is_whiff", "is_swing", "chase", "is_pa", "is_k", "PitchFamily",
                   "TwoStrikeInd", "out_of_zone", "z_swing", "count_cat", "two_strike")

sec_pool_data <- tm_data %>% 
  filter(BatterTeam %in% sec_teams) %>%
  select(any_of(sec_pool_cols))

# Create Overall D1 pool for percentile comparisons (reference to tm_data to save memory)
# Only create a copy if we need to modify it
d1_pool_data <- tm_data

# Clean up memory
gc(verbose = FALSE)

# ============================================================================
# PITCHER ARSENAL AND BATTER PITCH PERFORMANCE DATA FOR MAC MATCHUP
# ============================================================================

# Create pitcher arsenal summary with run value
# Note: Direct column references used instead of ifelse() to avoid scalar condition bug
pitcher_arsenal <- tm_data %>%
  filter(!is.na(TaggedPitchType)) %>%
  mutate(PitchFamily = classify_pitch_family(TaggedPitchType)) %>%
  group_by(Pitcher, PitcherThrows, PitchFamily) %>%
  summarise(
    n = n(),
    RelSpeed = mean(RelSpeed, na.rm = TRUE),
    InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
    HorzBreak = mean(HorzBreak, na.rm = TRUE),
    SpinRate = mean(SpinRate, na.rm = TRUE),
    RelHeight = mean(RelHeight, na.rm = TRUE),
    RelSide = mean(RelSide, na.rm = TRUE),
    mean_DRE_pit = mean(mean_DRE_pit, na.rm = TRUE),  # Pitcher run value
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  group_by(Pitcher) %>%
  mutate(usage_pct = n / sum(n)) %>%
  ungroup()

# Calculate feature means and standard deviations for standardization
feature_means <- list(
  RelSpeed = mean(pitcher_arsenal$RelSpeed, na.rm = TRUE),
  InducedVertBreak = mean(pitcher_arsenal$InducedVertBreak, na.rm = TRUE),
  HorzBreak = mean(pitcher_arsenal$HorzBreak, na.rm = TRUE),
  SpinRate = mean(pitcher_arsenal$SpinRate, na.rm = TRUE),
  RelHeight = mean(pitcher_arsenal$RelHeight, na.rm = TRUE),
  RelSide = mean(pitcher_arsenal$RelSide, na.rm = TRUE)
)

feature_sds <- list(
  RelSpeed = sd(pitcher_arsenal$RelSpeed, na.rm = TRUE),
  InducedVertBreak = sd(pitcher_arsenal$InducedVertBreak, na.rm = TRUE),
  HorzBreak = sd(pitcher_arsenal$HorzBreak, na.rm = TRUE),
  SpinRate = sd(pitcher_arsenal$SpinRate, na.rm = TRUE),
  RelHeight = sd(pitcher_arsenal$RelHeight, na.rm = TRUE),
  RelSide = sd(pitcher_arsenal$RelSide, na.rm = TRUE)
)

# Replace any NA/zero SDs with 1 to avoid division errors
feature_sds <- lapply(feature_sds, function(x) if(is.na(x) || x == 0) 1 else x)

cat("Memory used after pitcher arsenal:", round(sum(gc()[,2]), 1), "MB\n")

# Create batter pitch performance data with standardized features (optimized)
# Note: Direct column references used instead of ifelse() to avoid scalar condition bug
batter_pitch_performance <- tm_data %>%
  filter(!is.na(TaggedPitchType)) %>%
  mutate(
    PitchFamily = classify_pitch_family(TaggedPitchType),
    hitter_rv = mean_DRE_bat,
    # Standardize features using columns (now guaranteed to exist)
    RelSpeed_z = (RelSpeed - feature_means$RelSpeed) / feature_sds$RelSpeed,
    IVB_z = (InducedVertBreak - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak,
    HB_z = (HorzBreak - feature_means$HorzBreak) / feature_sds$HorzBreak,
    SpinRate_z = (SpinRate - feature_means$SpinRate) / feature_sds$SpinRate,
    RelHeight_z = (RelHeight - feature_means$RelHeight) / feature_sds$RelHeight,
    RelSide_z = (RelSide - feature_means$RelSide) / feature_sds$RelSide,
    # Use indicator columns (already added by add_indicators)
    SwingIndicator = is_swing,
    WhiffIndicator = is_whiff,
    ABindicator = is_ab,
    HitIndicator = is_hit,
    PAindicator = is_pa,
    KorBB = case_when(
      is_k == 1 ~ "Strikeout",
      is_walk == 1 ~ "Walk",
      TRUE ~ NA_character_
    ),
    totalbases = case_when(
      PlayResult == "HomeRun" ~ 4,
      PlayResult == "Triple" ~ 3,
      PlayResult == "Double" ~ 2,
      PlayResult == "Single" ~ 1,
      TRUE ~ 0
    )
  )

# Get list of all pitchers for dropdown
all_pitchers <- sort(unique(tm_data$Pitcher))

# ============================================================================
# PRE-COMPUTE STANDARDIZED BATTER DATA FOR MAC (MEMORY OPTIMIZED)
# ============================================================================
# Pre-compute z-scores and safe values to avoid recalculating during matchup queries

batter_pitch_performance <- batter_pitch_performance %>%
  mutate(
    # Pre-compute safe z-scores for MAC distance calculation (coalesce NA to 0)
    RelSpeed_z_safe = coalesce(RelSpeed_z, 0),
    IVB_z_safe = coalesce(IVB_z, 0),
    HB_z_safe = coalesce(HB_z, 0),
    SpinRate_z_safe = coalesce(SpinRate_z, 0),
    RelHeight_z_safe = coalesce(RelHeight_z, 0),
    RelSide_z_safe = coalesce(RelSide_z, 0)
  )

# Force garbage collection after adding columns
gc(verbose = FALSE)

# ============================================================================
# PRE-BIN BATTER PERFORMANCE AGAINST PITCH PROFILES AT STARTUP
# ============================================================================
# This pre-computation reduces model load times by caching aggregated stats

cat("Pre-binning batter performance against pitch profiles...\n")

# Define pitch type groups for advanced heat maps
classify_detailed_pitch <- function(pitch_type) {
  case_when(
    pitch_type %in% c("Fastball", "Four-Seam", "FourSeamFastBall", "FF") ~ "4S",
    pitch_type %in% c("TwoSeamFastBall", "Sinker", "SI", "FT") ~ "2S/Si",
    pitch_type %in% c("Slider", "Sweeper", "SL", "SW") ~ "SL/SW",
    pitch_type %in% c("Curveball", "Cutter", "CU", "CB", "KC", "FC", "SV") ~ "CB",
    pitch_type %in% c("Changeup", "ChangeUp", "Splitter", "CH", "FS", "SP") ~ "CH/Spl",
    TRUE ~ "Other"
  )
}

# Add detailed pitch classification to batter_pitch_performance
batter_pitch_performance <- batter_pitch_performance %>%
  mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType))

# Pre-bin batter performance by pitch type and pitcher hand
batter_pitch_profile_cache <- batter_pitch_performance %>%
  filter(!is.na(Batter), !is.na(PitcherThrows)) %>%
  group_by(Batter, PitcherThrows, DetailedPitchType) %>%
  summarise(
    n_pitches = n(),
    n_swings = sum(SwingIndicator, na.rm = TRUE),
    n_whiffs = sum(WhiffIndicator, na.rm = TRUE),
    n_in_zone = sum(in_zone, na.rm = TRUE),
    n_out_zone = sum(out_of_zone, na.rm = TRUE),
    n_chase = sum(chase, na.rm = TRUE),
    n_iz_swing = sum(z_swing, na.rm = TRUE),
    n_iz_whiff = sum(in_zone_whiff, na.rm = TRUE),
    n_bip = sum(PitchCall == "InPlay", na.rm = TRUE),
    n_damage = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE),
    woba_sum = sum(woba, na.rm = TRUE),
    woba_n = sum(!is.na(woba)),
    rv_sum = sum(mean_DRE_bat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pitch_freq = n_pitches,
    swing_pct = ifelse(n_pitches > 0, 100 * n_swings / n_pitches, NA),
    whiff_pct = ifelse(n_swings > 0, 100 * n_whiffs / n_swings, NA),
    woba = ifelse(woba_n > 0, woba_sum / woba_n, NA),
    damage_pct = ifelse(n_bip > 0, 100 * n_damage / n_bip, NA),
    chase_pct = ifelse(n_out_zone > 0, 100 * n_chase / n_out_zone, NA),
    iz_whiff_pct = ifelse(n_iz_swing > 0, 100 * n_iz_whiff / n_iz_swing, NA),
    iz_damage_pct = ifelse(n_in_zone > 0, 100 * n_damage / pmax(1, n_bip), NA)
  )

# Pre-bin batter performance by count type and pitcher hand
batter_count_profile_cache <- batter_pitch_performance %>%
  filter(!is.na(Batter), !is.na(PitcherThrows)) %>%
  mutate(
    CountType = case_when(
      Balls == 0 & Strikes == 0 ~ "1P",
      Strikes == 2 ~ "2K",
      Strikes > Balls ~ "Ahead",
      Balls > Strikes ~ "Behind",
      TRUE ~ "Even"
    )
  ) %>%
  group_by(Batter, PitcherThrows, CountType) %>%
  summarise(
    n_pitches = n(),
    n_swings = sum(SwingIndicator, na.rm = TRUE),
    n_whiffs = sum(WhiffIndicator, na.rm = TRUE),
    n_in_zone = sum(in_zone, na.rm = TRUE),
    n_out_zone = sum(out_of_zone, na.rm = TRUE),
    n_chase = sum(chase, na.rm = TRUE),
    n_iz_swing = sum(z_swing, na.rm = TRUE),
    n_iz_whiff = sum(in_zone_whiff, na.rm = TRUE),
    n_bip = sum(PitchCall == "InPlay", na.rm = TRUE),
    n_damage = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE),
    woba_sum = sum(woba, na.rm = TRUE),
    woba_n = sum(!is.na(woba)),
    rv_sum = sum(mean_DRE_bat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pitch_freq = n_pitches,
    swing_pct = ifelse(n_pitches > 0, 100 * n_swings / n_pitches, NA),
    whiff_pct = ifelse(n_swings > 0, 100 * n_whiffs / n_swings, NA),
    woba = ifelse(woba_n > 0, woba_sum / woba_n, NA),
    damage_pct = ifelse(n_bip > 0, 100 * n_damage / n_bip, NA),
    chase_pct = ifelse(n_out_zone > 0, 100 * n_chase / n_out_zone, NA),
    iz_whiff_pct = ifelse(n_iz_swing > 0, 100 * n_iz_whiff / n_iz_swing, NA),
    iz_damage_pct = ifelse(n_in_zone > 0, 100 * n_damage / pmax(1, n_bip), NA)
  )

# Pre-compute last 15 games data for each batter
batter_last15_cache <- batter_pitch_performance %>%
  filter(!is.na(Batter), !is.na(Date)) %>%
  group_by(Batter) %>%
  arrange(desc(as.Date(Date))) %>%
  mutate(game_rank = dense_rank(desc(as.Date(Date)))) %>%
  filter(game_rank <= 15) %>%
  group_by(Batter, PitcherThrows) %>%
  summarise(
    n_pitches = n(),
    n_swings = sum(SwingIndicator, na.rm = TRUE),
    n_whiffs = sum(WhiffIndicator, na.rm = TRUE),
    n_in_zone = sum(in_zone, na.rm = TRUE),
    n_out_zone = sum(out_of_zone, na.rm = TRUE),
    n_chase = sum(chase, na.rm = TRUE),
    n_iz_swing = sum(z_swing, na.rm = TRUE),
    n_iz_whiff = sum(in_zone_whiff, na.rm = TRUE),
    n_bip = sum(PitchCall == "InPlay", na.rm = TRUE),
    n_damage = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE),
    woba_sum = sum(woba, na.rm = TRUE),
    woba_n = sum(!is.na(woba)),
    .groups = "drop"
  ) %>%
  mutate(
    pitch_freq = n_pitches,
    swing_pct = ifelse(n_pitches > 0, 100 * n_swings / n_pitches, NA),
    whiff_pct = ifelse(n_swings > 0, 100 * n_whiffs / n_swings, NA),
    woba = ifelse(woba_n > 0, woba_sum / woba_n, NA),
    damage_pct = ifelse(n_bip > 0, 100 * n_damage / n_bip, NA),
    chase_pct = ifelse(n_out_zone > 0, 100 * n_chase / n_out_zone, NA),
    iz_whiff_pct = ifelse(n_iz_swing > 0, 100 * n_iz_whiff / n_iz_swing, NA),
    iz_damage_pct = ifelse(n_in_zone > 0, 100 * n_damage / pmax(1, n_bip), NA)
  )

# Pre-compute overall batter stats by pitcher hand
batter_overall_cache <- batter_pitch_performance %>%
  filter(!is.na(Batter), !is.na(PitcherThrows)) %>%
  group_by(Batter, PitcherThrows) %>%
  summarise(
    n_pitches = n(),
    n_swings = sum(SwingIndicator, na.rm = TRUE),
    n_whiffs = sum(WhiffIndicator, na.rm = TRUE),
    n_in_zone = sum(in_zone, na.rm = TRUE),
    n_out_zone = sum(out_of_zone, na.rm = TRUE),
    n_chase = sum(chase, na.rm = TRUE),
    n_iz_swing = sum(z_swing, na.rm = TRUE),
    n_iz_whiff = sum(in_zone_whiff, na.rm = TRUE),
    n_bip = sum(PitchCall == "InPlay", na.rm = TRUE),
    n_damage = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE),
    woba_sum = sum(woba, na.rm = TRUE),
    woba_n = sum(!is.na(woba)),
    .groups = "drop"
  ) %>%
  mutate(
    pitch_freq = n_pitches,
    swing_pct = ifelse(n_pitches > 0, 100 * n_swings / n_pitches, NA),
    whiff_pct = ifelse(n_swings > 0, 100 * n_whiffs / n_swings, NA),
    woba = ifelse(woba_n > 0, woba_sum / woba_n, NA),
    damage_pct = ifelse(n_bip > 0, 100 * n_damage / n_bip, NA),
    chase_pct = ifelse(n_out_zone > 0, 100 * n_chase / n_out_zone, NA),
    iz_whiff_pct = ifelse(n_iz_swing > 0, 100 * n_iz_whiff / n_iz_swing, NA),
    iz_damage_pct = ifelse(n_in_zone > 0, 100 * n_damage / pmax(1, n_bip), NA)
  )

cat("Pre-binned performance data for", n_distinct(batter_pitch_profile_cache$Batter), "batters\n")
cat("  - Pitch type profiles:", nrow(batter_pitch_profile_cache), "records\n")
cat("  - Count profiles:", nrow(batter_count_profile_cache), "records\n")
cat("  - Last 15 games:", nrow(batter_last15_cache), "records\n")

# Memory cleanup after initial data processing
cat("Data loading complete. Memory cleanup...\n")
gc(verbose = FALSE)
cat("Final memory usage:", round(sum(gc()[,2]), 1), "MB\n")
cat("Loaded", nrow(tm_data), "pitch records for", length(all_hitters), "hitters and", length(all_pitchers), "pitchers\n")

# ============================================================================
# MAC-STYLE MATCHUP CALCULATION FUNCTION (Euclidean Distance with 0.6 threshold)
# Based on: "Using Euclidean Distance and Clustering to quantify batter vs. pitcher matchups"
# by Chap Cunningham and Zack Aisen (Saber Seminar 2025)
# ============================================================================

# MAC Distance threshold - 0.6 is the "sweet spot" per the article
MAC_DISTANCE_THRESHOLD <- 0.6

# Vectorized Euclidean distance calculation for efficiency
calculate_euclidean_distance_vectorized <- function(batter_df, target) {
  sqrt(
    (batter_df$RelSpeed_z_safe - target$RelSpeed_z)^2 +
    (batter_df$IVB_z_safe - target$IVB_z)^2 +
    (batter_df$HB_z_safe - target$HB_z)^2 +
    (batter_df$SpinRate_z_safe - target$SpinRate_z)^2 +
    (batter_df$RelHeight_z_safe - target$RelHeight_z)^2 +
    (batter_df$RelSide_z_safe - target$RelSide_z)^2
  )
}

# Convert RV/100 to 0-100 MAC score
# 0 = Strong Hitter Advantage, 50 = Neutral, 100 = Strong Pitcher Advantage
rv100_to_mac_score <- function(rv100, n_similar) {
  if (is.na(rv100) || n_similar < 5) return(50)
  
  # Apply confidence adjustment based on sample size
  # More samples = more confident in the score
  confidence <- min(1, n_similar / 100)
  
  # Scale: RV/100 of +3 = 0 (strong hitter), -3 = 100 (strong pitcher)
  # Center at 50, scale by 16.67 per run value
  raw_score <- 50 - (rv100 * 16.67)
  
  # Apply confidence - pull toward 50 if low sample
  adjusted_score <- 50 + (raw_score - 50) * confidence
  
  # Clamp to 0-100
  round(max(0, min(100, adjusted_score)))
}

calculate_mac_matchup <- function(p_name, h_name, distance_threshold = MAC_DISTANCE_THRESHOLD) {
  # Get pitcher's arsenal
  p_arsenal <- pitcher_arsenal %>% filter(Pitcher == p_name)
  
  if (nrow(p_arsenal) == 0) {
    return(list(
      score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL,
      whiff_pct = NA, chase_pct = NA, woba = NA, damage_pct = NA,
      confidence = "Low"
    ))
  }
  
  p_hand <- p_arsenal$PitcherThrows[1]
  
  # Get batter's pitch history - FILTER BY SAME PITCHER HAND
  batter_pitches <- batter_pitch_performance %>%
    filter(Batter == h_name, PitcherThrows == p_hand)
  
  if (nrow(batter_pitches) < 20) {
    return(list(
      score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL,
      whiff_pct = NA, chase_pct = NA, woba = NA, damage_pct = NA,
      confidence = "Low"
    ))
  }
  
  # Use pre-computed safe z-scores (no need to recalculate - already done at startup)
  batter_pitches_safe <- batter_pitches
  
  # For each pitch type in pitcher's arsenal, find similar pitches batter has faced
  results_by_pitch <- list()
  all_similar_pitches <- data.frame()
  total_similar <- 0
  
  for (i in 1:nrow(p_arsenal)) {
    pitch_type <- p_arsenal$PitchFamily[i]
    usage <- p_arsenal$usage_pct[i]
    
    # Target pitch profile (standardized) - from MAC scanning features
    target <- list(
      RelSpeed_z = coalesce((p_arsenal$RelSpeed[i] - feature_means$RelSpeed) / feature_sds$RelSpeed, 0),
      IVB_z = coalesce((p_arsenal$InducedVertBreak[i] - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak, 0),
      HB_z = coalesce((p_arsenal$HorzBreak[i] - feature_means$HorzBreak) / feature_sds$HorzBreak, 0),
      SpinRate_z = coalesce((p_arsenal$SpinRate[i] - feature_means$SpinRate) / feature_sds$SpinRate, 0),
      RelHeight_z = coalesce((p_arsenal$RelHeight[i] - feature_means$RelHeight) / feature_sds$RelHeight, 0),
      RelSide_z = coalesce((p_arsenal$RelSide[i] - feature_means$RelSide) / feature_sds$RelSide, 0)
    )
    
    # Vectorized distance calculation for efficiency
    distances <- calculate_euclidean_distance_vectorized(batter_pitches_safe, target)
    
    # Filter to similar pitches using MAC threshold (0.6)
    similar_mask <- !is.na(distances) & distances <= distance_threshold
    similar_pitches <- batter_pitches_safe[similar_mask, ]
    similar_pitches$distance <- distances[similar_mask]
    similar_pitches$pitcher_pitch_type <- pitch_type
    
    n_similar_pitch <- nrow(similar_pitches)
    
    if (n_similar_pitch >= 5) {
      # Calculate metrics on similar pitches
      rv100_similar <- 100 * mean(similar_pitches$hitter_rv, na.rm = TRUE)
      whiff_pct <- 100 * mean(similar_pitches$WhiffIndicator, na.rm = TRUE)
      chase_pct <- 100 * mean(similar_pitches$chase[similar_pitches$out_of_zone == 1], na.rm = TRUE)
      woba <- mean(similar_pitches$woba, na.rm = TRUE)
      damage_pct <- 100 * mean(similar_pitches$ExitSpeed >= 95, na.rm = TRUE)
      
      # Apply shrinkage based on sample size (Bayesian approach)
      k <- 50  # Shrinkage factor
      shrinkage <- n_similar_pitch / (n_similar_pitch + k)
      rv100_shrunk <- shrinkage * rv100_similar + (1 - shrinkage) * 0
      
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = rv100_shrunk,
        rv100_raw = rv100_similar,
        usage = usage,
        whiff_pct = whiff_pct,
        chase_pct = if(is.nan(chase_pct)) NA else chase_pct,
        woba = woba,
        damage_pct = damage_pct
      )
      total_similar <- total_similar + n_similar_pitch
      all_similar_pitches <- rbind(all_similar_pitches, similar_pitches)
    } else {
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = 0,
        rv100_raw = NA,
        usage = usage,
        whiff_pct = NA,
        chase_pct = NA,
        woba = NA,
        damage_pct = NA
      )
    }
  }
  
  # Calculate usage-weighted RV/100 (MAC methodology)
  weighted_rv100 <- 0
  total_usage_weight <- 0
  
  for (pt in names(results_by_pitch)) {
    pitch_data <- results_by_pitch[[pt]]
    usage <- pitch_data$usage
    
    if (pitch_data$n >= 5 && !is.na(pitch_data$rv100_raw)) {
      # Weight by usage percentage
      weighted_rv100 <- weighted_rv100 + pitch_data$rv100 * usage
      total_usage_weight <- total_usage_weight + usage
    }
  }
  
  if (total_usage_weight > 0) {
    weighted_rv100 <- weighted_rv100 / total_usage_weight
  } else {
    weighted_rv100 <- 0
  }
  
  # Calculate overall stats from all similar pitches
  overall_whiff <- if(nrow(all_similar_pitches) > 0) 100 * mean(all_similar_pitches$WhiffIndicator, na.rm = TRUE) else NA
  overall_chase <- if(nrow(all_similar_pitches) > 0) 100 * mean(all_similar_pitches$chase[all_similar_pitches$out_of_zone == 1], na.rm = TRUE) else NA
  overall_woba <- if(nrow(all_similar_pitches) > 0) mean(all_similar_pitches$woba, na.rm = TRUE) else NA
  overall_damage <- if(nrow(all_similar_pitches) > 0) 100 * mean(all_similar_pitches$ExitSpeed >= 95, na.rm = TRUE) else NA
  
  # Convert to MAC 0-100 score
  final_score <- rv100_to_mac_score(weighted_rv100, total_similar)
  
  # Confidence level based on sample size
  confidence <- if(total_similar < 30) "Low" else if(total_similar < 100) "Medium" else "High"
  
  list(
    score = final_score,
    rv100 = round(weighted_rv100, 2),
    n_similar = total_similar,
    by_pitch = results_by_pitch,
    whiff_pct = round(overall_whiff, 1),
    chase_pct = if(is.nan(overall_chase)) NA else round(overall_chase, 1),
    woba = round(overall_woba, 3),
    damage_pct = round(overall_damage, 1),
    confidence = confidence
  )
}

# Calculate pitch type specific matchup (FB, BB, OS) using MAC methodology
calculate_pitch_type_matchup <- function(p_name, h_name, pitch_family, distance_threshold = MAC_DISTANCE_THRESHOLD) {
  # Get pitcher's arsenal for specific pitch type
  p_arsenal <- pitcher_arsenal %>% filter(Pitcher == p_name, PitchFamily == pitch_family)
  
  if (nrow(p_arsenal) == 0) {
    return(list(score = 50, rv100 = 0, n_similar = 0, whiff_pct = NA, chase_pct = NA, woba = NA))
  }
  
  p_hand <- p_arsenal$PitcherThrows[1]
  
  # Get batter's pitch history - FILTER BY SAME PITCHER HAND
  batter_pitches <- batter_pitch_performance %>%
    filter(Batter == h_name, PitcherThrows == p_hand)
  
  if (nrow(batter_pitches) < 20) {
    return(list(score = 50, rv100 = 0, n_similar = 0, whiff_pct = NA, chase_pct = NA, woba = NA))
  }
  
  # Target pitch profile (standardized) - MAC scanning features
  target <- list(
    RelSpeed_z = coalesce((p_arsenal$RelSpeed[1] - feature_means$RelSpeed) / feature_sds$RelSpeed, 0),
    IVB_z = coalesce((p_arsenal$InducedVertBreak[1] - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak, 0),
    HB_z = coalesce((p_arsenal$HorzBreak[1] - feature_means$HorzBreak) / feature_sds$HorzBreak, 0),
    SpinRate_z = coalesce((p_arsenal$SpinRate[1] - feature_means$SpinRate) / feature_sds$SpinRate, 0),
    RelHeight_z = coalesce((p_arsenal$RelHeight[1] - feature_means$RelHeight) / feature_sds$RelHeight, 0),
    RelSide_z = coalesce((p_arsenal$RelSide[1] - feature_means$RelSide) / feature_sds$RelSide, 0)
  )
  
  # Use pre-computed safe z-scores (already done at startup)
  batter_pitches_safe <- batter_pitches
  
  # Vectorized distance calculation
  distances <- calculate_euclidean_distance_vectorized(batter_pitches_safe, target)
  
  # Filter using MAC threshold (0.6)
  similar_mask <- !is.na(distances) & distances <= distance_threshold
  similar_pitches <- batter_pitches_safe[similar_mask, ]
  
  n_similar <- nrow(similar_pitches)
  
  if (n_similar >= 5) {
    rv100_similar <- 100 * mean(similar_pitches$hitter_rv, na.rm = TRUE)
    whiff_pct <- 100 * mean(similar_pitches$WhiffIndicator, na.rm = TRUE)
    chase_pct <- 100 * mean(similar_pitches$chase[similar_pitches$out_of_zone == 1], na.rm = TRUE)
    woba <- mean(similar_pitches$woba, na.rm = TRUE)
    
    # Shrinkage
    k <- 50
    shrinkage <- n_similar / (n_similar + k)
    rv100_shrunk <- shrinkage * rv100_similar + (1 - shrinkage) * 0
    
    # Convert to MAC score
    final_score <- rv100_to_mac_score(rv100_shrunk, n_similar)
    
    return(list(
      score = final_score, 
      rv100 = round(rv100_shrunk, 2), 
      n_similar = n_similar,
      whiff_pct = round(whiff_pct, 1),
      chase_pct = if(is.nan(chase_pct)) NA else round(chase_pct, 1),
      woba = round(woba, 3)
    ))
  }
  
  list(score = 50, rv100 = 0, n_similar = n_similar, whiff_pct = NA, chase_pct = NA, woba = NA)
}

# Helper function to get score color
get_matchup_score_color <- function(score) {
  if (is.na(score)) return("#E0E0E0")
  if (score >= 70) return("#C8E6C9")  # Pitcher advantage - green
  if (score >= 60) return("#DCEDC8")
  if (score >= 55) return("#F0F4C3")
  if (score >= 45) return("#FFF9C4")  # Neutral - yellow
  if (score >= 40) return("#FFECB3")
  if (score >= 30) return("#FFE0B2")
  return("#FFCDD2")  # Hitter advantage - red
}

# ============================================================================
# xRV PREDICTIVE MATCHUP MODEL - XGBoost with Pitch Similarity Features
# ============================================================================
# Combines pitcher pitch effectiveness with batter performance against similar pitches
# Uses Euclidean distance <= 1.0 to find similar pitches for batter features

cat("Building xRV Predictive Model...\n")

# Calculate league-wide prior distributions (fallback only when no data)
calculate_sec_priors <- function(sec_data) {
  sec_data %>%
    filter(!is.na(PitchFamily), !is.na(mean_DRE_bat)) %>%
    group_by(PitchFamily) %>%
    summarise(
      prior_mean = mean(mean_DRE_bat, na.rm = TRUE),
      prior_sd = sd(mean_DRE_bat, na.rm = TRUE),
      n_total = n(),
      .groups = "drop"
    )
}

sec_priors <- calculate_sec_priors(sec_pool_data)
cat("SEC priors calculated for", nrow(sec_priors), "pitch families (fallback only)\n")

# ============================================================================
# PITCHER-CENTRIC xRV CALCULATION (SIMPLIFIED)
# ============================================================================
# Pre-calculate pitcher xRV vs SEC hitters by batter side and pitch family
# This is much faster than the previous batter-similarity approach

# Calculate pitcher xRV vs SEC hitters by batter hand and pitch family
pitcher_sec_xrv <- tm_data %>%
  filter(
    !is.na(Pitcher),
    !is.na(PitchFamily),
    !is.na(mean_DRE_pit),
    !is.na(BatterSide),
    BatterTeam %in% sec_teams  # Only vs SEC hitters
  ) %>%
  group_by(Pitcher, PitcherThrows, BatterSide, PitchFamily) %>%
  summarise(
    n_pitches = n(),
    xrv_per100 = mean(mean_DRE_pit, na.rm = TRUE) * 100,
    whiff_pct = mean(is_whiff, na.rm = TRUE) * 100,
    chase_pct = mean(chase, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 10)

# Also create overall pitcher xRV by batter side (not split by pitch family)
pitcher_sec_xrv_overall <- tm_data %>%
  filter(
    !is.na(Pitcher),
    !is.na(mean_DRE_pit),
    !is.na(BatterSide),
    BatterTeam %in% sec_teams
  ) %>%
  group_by(Pitcher, PitcherThrows, BatterSide) %>%
  summarise(
    n_pitches = n(),
    xrv_per100 = mean(mean_DRE_pit, na.rm = TRUE) * 100,
    whiff_pct = mean(is_whiff, na.rm = TRUE) * 100,
    chase_pct = mean(chase, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n_pitches >= 20)

cat("Built pitcher SEC xRV data:", nrow(pitcher_sec_xrv), "pitch family rows,", 
    nrow(pitcher_sec_xrv_overall), "overall rows\n")

# ============================================================================
# PITCHER-CENTRIC MATCHUP FUNCTION (SIMPLIFIED - NO HITTER-SPECIFIC CALCS)
# ============================================================================
# Just looks up pre-calculated pitcher xRV vs SEC hitters by batter side
# User can manually input hitter-specific adjustments in the UI

# Simple function to get pitcher xRV data for matchup matrix
get_pitcher_sec_stats <- function(pitcher_name, batter_side) {
  # Get overall stats for this pitcher vs batter side
  overall_data <- pitcher_sec_xrv_overall %>%
    filter(Pitcher == pitcher_name, BatterSide == batter_side)
  
  # Get pitch family breakdown
  family_data <- pitcher_sec_xrv %>%
    filter(Pitcher == pitcher_name, BatterSide == batter_side)
  
  if (nrow(overall_data) == 0) {
    return(list(
      overall_xrv = NA,
      fb_xrv = NA,
      bb_xrv = NA,
      os_xrv = NA,
      n_pitches = 0,
      whiff_pct = NA,
      chase_pct = NA,
      pitcher_hand = NA
    ))
  }
  
  # Get pitch family specific xRV
  fb_row <- family_data %>% filter(PitchFamily == "FB")
  bb_row <- family_data %>% filter(PitchFamily == "BB")
  os_row <- family_data %>% filter(PitchFamily == "OS")
  
  list(
    overall_xrv = overall_data$xrv_per100[1],
    fb_xrv = if(nrow(fb_row) > 0) fb_row$xrv_per100[1] else NA,
    bb_xrv = if(nrow(bb_row) > 0) bb_row$xrv_per100[1] else NA,
    os_xrv = if(nrow(os_row) > 0) os_row$xrv_per100[1] else NA,
    n_pitches = overall_data$n_pitches[1],
    whiff_pct = overall_data$whiff_pct[1],
    chase_pct = overall_data$chase_pct[1],
    pitcher_hand = overall_data$PitcherThrows[1]
  )
}

# Function to get pitcher stats for all batter sides (for matrix display)
get_pitcher_matrix_stats <- function(pitcher_name) {
  vs_lhb <- get_pitcher_sec_stats(pitcher_name, "Left")
  vs_rhb <- get_pitcher_sec_stats(pitcher_name, "Right")
  
  # Get pitcher hand
  p_hand <- pitcher_arsenal %>% 
    filter(Pitcher == pitcher_name) %>% 
    pull(PitcherThrows) %>% 
    unique() %>% 
    first()
  
  list(
    pitcher_hand = if(!is.na(p_hand)) p_hand else "Unknown",
    vs_lhb = vs_lhb,
    vs_rhb = vs_rhb
  )
}

# Helper function for xRV color coding (positive = hitter advantage = red)
get_xrv_color <- function(xrv) {
  if (is.na(xrv)) return("#E0E0E0")
  if (xrv >= 1.5) return("#FFCDD2")    # Strong hitter advantage - red
  if (xrv >= 0.5) return("#FFE0B2")    # Moderate hitter advantage
  if (xrv >= 0) return("#FFF9C4")      # Slight hitter advantage - yellow
  if (xrv >= -0.5) return("#F0F4C3")   # Slight pitcher advantage
  if (xrv >= -1.5) return("#DCEDC8")   # Moderate pitcher advantage
  return("#C8E6C9")                     # Strong pitcher advantage - green
}

# Helper function for xRV text color
get_xrv_text_color <- function(xrv) {
  if (is.na(xrv)) return("#666666")
  if (xrv >= 0.5) return("#C62828")    # Red text for hitter advantage
  if (xrv >= -0.5) return("#F57F17")   # Orange/yellow for neutral
  return("#2E7D32")                     # Green text for pitcher advantage
}

cat("xRV Predictive Model setup complete\n")
gc(verbose = FALSE)

# ============================================================================
# RUN VALUE CALCULATION FUNCTION
# ============================================================================

calculate_rv100 <- function(df) {
  # Calculate hitter run value from existing mean_DRE_bat if available
  # Otherwise estimate from plate outcome events
  if ("mean_DRE_bat" %in% names(df)) {
    df <- df %>% mutate(hitter_rv = mean_DRE_bat)
  } else if ("woba" %in% names(df)) {
    # Estimate RV from wOBA (rough approximation)
    df <- df %>% mutate(hitter_rv = (woba - 0.320) * 0.8)
  } else {
    df <- df %>% mutate(hitter_rv = 0)
  }
  df
}

# ============================================================================
# SWITCH HITTER DETECTION FUNCTION
# ============================================================================

detect_batter_handedness <- function(batter_name, tm_data, min_ab_threshold = 15, min_pct_threshold = 0.10) {
  # Get batting side counts for this batter
  batter_data <- tm_data %>%
    filter(Batter == batter_name, !is.na(BatterSide)) %>%
    group_by(BatterSide) %>%
    summarise(n_ab = sum(is_ab, na.rm = TRUE), .groups = "drop")
  
  left_ab <- sum(batter_data$n_ab[batter_data$BatterSide == "Left"], na.rm = TRUE)
  right_ab <- sum(batter_data$n_ab[batter_data$BatterSide == "Right"], na.rm = TRUE)
  total_ab <- left_ab + right_ab
  
  # If player has significant at-bats from both sides (both >= min ABs AND >= min percentage), they're a switch hitter
  # Require at least 15 ABs from each side AND at least 10% of total ABs from minority side
  if (total_ab > 0) {
    left_pct <- left_ab / total_ab
    right_pct <- right_ab / total_ab
    
    if (left_ab >= min_ab_threshold && right_ab >= min_ab_threshold &&
        left_pct >= min_pct_threshold && right_pct >= min_pct_threshold) {
      return("Switch")
    }
  }
  
  if (left_ab > right_ab) {
    return("Left")
  } else {
    return("Right")
  }
}

# ============================================================================
# ENHANCED GRADE METRICS CALCULATION (PERCENTILE-BASED)
# ============================================================================

# Calculate grade metrics from SEC pool using percentile approach (more robust than z-scores)
calculate_enhanced_grade_metrics <- function(pool_data) {
  # Ensure indicators are present
  if (!"is_pa" %in% names(pool_data)) {
    pool_data <- add_indicators(pool_data)
  }
  pool_data <- calculate_rv100(pool_data)
  
  # Add required indicator columns if missing
  if (!"PAindicator" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(PAindicator = is_pa)
  }
  if (!"ABindicator" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(ABindicator = is_ab)
  }
  if (!"HitIndicator" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(HitIndicator = is_hit)
  }
  if (!"totalbases" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(
      totalbases = case_when(
        PlayResult == "HomeRun" ~ 4,
        PlayResult == "Triple" ~ 3,
        PlayResult == "Double" ~ 2,
        PlayResult == "Single" ~ 1,
        TRUE ~ 0
      )
    )
  }
  if (!"KorBB" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(
      KorBB = case_when(
        is_k == 1 ~ "Strikeout",
        is_walk == 1 ~ "Walk",
        TRUE ~ NA_character_
      )
    )
  }
  if (!"Zwhiffind" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(Zwhiffind = in_zone_whiff)
  }
  if (!"Zswing" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(Zswing = z_swing)
  }
  if (!"WhiffIndicator" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(WhiffIndicator = is_whiff)
  }
  if (!"SwingIndicator" %in% names(pool_data)) {
    pool_data <- pool_data %>% mutate(SwingIndicator = is_swing)
  }
  
  # Calculate per-batter stats with minimum PA threshold
  batter_stats <- pool_data %>%
    filter(!is.na(TaggedPitchType)) %>%
    group_by(Batter) %>%
    summarise(
      n_pitches = n(),
      n_pa = sum(PAindicator, na.rm = TRUE),
      # RV/100 - the core metric
      rv100 = 100 * mean(hitter_rv, na.rm = TRUE),
      # Power metrics
      iso = (sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))) - 
        (sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      # Contact metrics
      ba = sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      zone_con = 100 * (1 - sum(Zwhiffind, na.rm = TRUE) / pmax(1, sum(Zswing, na.rm = TRUE))),
      # Avoid K metrics
      k_pct = 100 * sum(KorBB == "Strikeout", na.rm = TRUE) / pmax(1, sum(PAindicator, na.rm = TRUE)),
      con2k = 100 * (1 - sum(WhiffIndicator[Strikes == 2], na.rm = TRUE) / 
                       pmax(1, sum(SwingIndicator[Strikes == 2], na.rm = TRUE))),
      # Swing Decision metrics: chase rate (lower=better) and zone swing rate (higher=better)
      chase_pct = 100 * sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)),
      z_swing_pct = 100 * sum(Zswing, na.rm = TRUE) / pmax(1, sum(in_zone, na.rm = TRUE)),
      rv_swing = 100 * mean(hitter_rv[SwingIndicator == 1], na.rm = TRUE),
rv_take = 100 * mean(hitter_rv[SwingIndicator == 0], na.rm = TRUE),
swing_dec_score = rv_swing - rv_take,
      .groups = "drop"
    ) %>%
    filter(n_pa >= 20)
  
  cat("Found", nrow(batter_stats), "batters with 20+ PA for grade baseline\n")
  
  # Calculate league percentiles for grading (more robust than z-scores)
  grade_metrics <- list(
    rv100_p20 = quantile(batter_stats$rv100, 0.10, na.rm = TRUE),
    rv100_p50 = quantile(batter_stats$rv100, 0.50, na.rm = TRUE),
    rv100_p80 = quantile(batter_stats$rv100, 0.90, na.rm = TRUE),
    
    iso_p20 = quantile(batter_stats$iso, 0.10, na.rm = TRUE),
    iso_p50 = quantile(batter_stats$iso, 0.50, na.rm = TRUE),
    iso_p80 = quantile(batter_stats$iso, 0.90, na.rm = TRUE),
    
    ev90_p20 = quantile(batter_stats$ev90, 0.10, na.rm = TRUE),
    ev90_p50 = quantile(batter_stats$ev90, 0.50, na.rm = TRUE),
    ev90_p80 = quantile(batter_stats$ev90, 0.90, na.rm = TRUE),
    
    ba_p20 = quantile(batter_stats$ba, 0.10, na.rm = TRUE),
    ba_p50 = quantile(batter_stats$ba, 0.50, na.rm = TRUE),
    ba_p80 = quantile(batter_stats$ba, 0.90, na.rm = TRUE),
    
    zone_con_p20 = quantile(batter_stats$zone_con, 0.10, na.rm = TRUE),
    zone_con_p50 = quantile(batter_stats$zone_con, 0.50, na.rm = TRUE),
    zone_con_p80 = quantile(batter_stats$zone_con, 0.90, na.rm = TRUE),
    
    k_p20 = quantile(batter_stats$k_pct, 0.90, na.rm = TRUE),  # Inverted - lower is better
    k_p50 = quantile(batter_stats$k_pct, 0.50, na.rm = TRUE),
    k_p80 = quantile(batter_stats$k_pct, 0.10, na.rm = TRUE),
    
    con2k_p20 = quantile(batter_stats$con2k, 0.10, na.rm = TRUE),
    con2k_p50 = quantile(batter_stats$con2k, 0.50, na.rm = TRUE),
    con2k_p80 = quantile(batter_stats$con2k, 0.90, na.rm = TRUE),
    
    # Swing Decision Score: z_swing - chase (higher = better)
    swing_dec_p20 = quantile(batter_stats$swing_dec_score, 0.10, na.rm = TRUE),
    swing_dec_p50 = quantile(batter_stats$swing_dec_score, 0.50, na.rm = TRUE),
    swing_dec_p80 = quantile(batter_stats$swing_dec_score, 0.90, na.rm = TRUE)
  )
  
  cat("\nGrade percentile thresholds:\n")
  cat("  RV/100: 20th=", round(grade_metrics$rv100_p20, 2), " 50th=", round(grade_metrics$rv100_p50, 2), " 80th=", round(grade_metrics$rv100_p80, 2), "\n")
  cat("  ISO: 20th=", round(grade_metrics$iso_p20, 3), " 50th=", round(grade_metrics$iso_p50, 3), " 80th=", round(grade_metrics$iso_p80, 3), "\n")
  cat("  EV90: 20th=", round(grade_metrics$ev90_p20, 1), " 50th=", round(grade_metrics$ev90_p50, 1), " 80th=", round(grade_metrics$ev90_p80, 1), "\n")
  cat("  BA: 20th=", round(grade_metrics$ba_p20, 3), " 50th=", round(grade_metrics$ba_p50, 3), " 80th=", round(grade_metrics$ba_p80, 3), "\n")
  cat("  ZCon: 20th=", round(grade_metrics$zone_con_p20, 1), " 50th=", round(grade_metrics$zone_con_p50, 1), " 80th=", round(grade_metrics$zone_con_p80, 1), "\n")
  cat("  2KCon: 20th=", round(grade_metrics$con2k_p20, 1), " 50th=", round(grade_metrics$con2k_p50, 1), " 80th=", round(grade_metrics$con2k_p80, 1), "\n")
  
  grade_metrics
}

# Calculate enhanced grade metrics from pool
grade_metrics_enhanced <- calculate_enhanced_grade_metrics(sec_pool_data)

# ============================================================================
# HITTING GRADES RADAR CHART FUNCTION (PERCENTILE-BASED)
# ============================================================================

create_hitting_grades_radar <- function(batter_name, team_data, grade_metrics = grade_metrics_enhanced) {
  # Calculate hitting grades using percentile-based approach
  grades <- calculate_hitter_grades(batter_name, team_data, grade_metrics)
  
  if (nrow(grades) == 0) {
    return(ggplot() + theme_void() + 
             ggtitle(paste("No data available for", batter_name)) +
             theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
  }
  
  # Create radar chart data
  radar_data <- grades %>%
    mutate(group = ifelse(PitcherThrows == "Right", "vs RHP", "vs LHP")) %>%
    select(group, swing_decisions, game_power, raw_power, avoid_k, contact) %>%
    rename(`Swing Decisions` = swing_decisions, `Game Power` = game_power, 
           `Raw Power` = raw_power, `Avoid K` = avoid_k, Contact = contact) %>%
    pivot_longer(cols = -group, names_to = "Metric", values_to = "Grade")
  
  # Radar chart
  ggplot(radar_data, aes(x = Metric, y = Grade, group = group, color = group, fill = group)) +
    geom_polygon(alpha = 0.2, linewidth = 1) +
    geom_point(size = 2) +
    coord_polar() +
    scale_y_continuous(limits = c(20, 80), breaks = c(20, 35, 50, 65, 80)) +
    scale_color_manual(values = c("vs RHP" = "#1976D2", "vs LHP" = "#D32F2F")) +
    scale_fill_manual(values = c("vs RHP" = "#1976D2", "vs LHP" = "#D32F2F")) +
    labs(title = paste("Hitting Grades:", batter_name), color = NULL, fill = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 7),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray80")
    )
}

# ---- FUNCTION TO CALCULATE GRADES FOR A BATTER (RV-BASED PERCENTILE APPROACH) ----
calculate_hitter_grades <- function(batter_name, team_data, grade_metrics = grade_metrics_enhanced, debug = FALSE) {
  # Add required indicator columns if missing
  if (!"PAindicator" %in% names(team_data)) {
    team_data <- team_data %>% mutate(PAindicator = is_pa)
  }
  if (!"ABindicator" %in% names(team_data)) {
    team_data <- team_data %>% mutate(ABindicator = is_ab)
  }
  if (!"HitIndicator" %in% names(team_data)) {
    team_data <- team_data %>% mutate(HitIndicator = is_hit)
  }
  if (!"totalbases" %in% names(team_data)) {
    team_data <- team_data %>% mutate(
      totalbases = case_when(
        PlayResult == "HomeRun" ~ 4,
        PlayResult == "Triple" ~ 3,
        PlayResult == "Double" ~ 2,
        PlayResult == "Single" ~ 1,
        TRUE ~ 0
      )
    )
  }
  if (!"KorBB" %in% names(team_data)) {
    team_data <- team_data %>% mutate(
      KorBB = case_when(
        is_k == 1 ~ "Strikeout",
        is_walk == 1 ~ "Walk",
        TRUE ~ NA_character_
      )
    )
  }
  if (!"Zwhiffind" %in% names(team_data)) {
    team_data <- team_data %>% mutate(Zwhiffind = in_zone_whiff)
  }
  if (!"Zswing" %in% names(team_data)) {
    team_data <- team_data %>% mutate(Zswing = z_swing)
  }
  if (!"WhiffIndicator" %in% names(team_data)) {
    team_data <- team_data %>% mutate(WhiffIndicator = is_whiff)
  }
  if (!"SwingIndicator" %in% names(team_data)) {
    team_data <- team_data %>% mutate(SwingIndicator = is_swing)
  }
  
  team_data <- calculate_rv100(team_data)
  
  batter_data <- team_data %>%
    filter(!is.na(TaggedPitchType), Batter == batter_name)
  
  if (nrow(batter_data) < 20) {
    return(tibble(
      PitcherThrows = c("Right", "Left"),
      swing_decisions = c(50, 50),
      game_power = c(50, 50),
      raw_power = c(50, 50),
      avoid_k = c(50, 50),
      contact = c(50, 50)
    ))
  }
  
  batter_split <- batter_data %>%
    group_by(PitcherThrows) %>%
    summarise(
      n = n(),
      # RV/100 - core value metric
      rv100 = 100 * mean(hitter_rv, na.rm = TRUE),
      # Power metrics
      iso = (sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))) - 
        (sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE))),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      # Contact metrics
      ba = sum(HitIndicator, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      zone_con = 100 * (1 - sum(Zwhiffind, na.rm = TRUE) / pmax(1, sum(Zswing, na.rm = TRUE))),
      # Avoid K metrics
      k_pct = 100 * sum(KorBB == "Strikeout", na.rm = TRUE) / pmax(1, sum(PAindicator, na.rm = TRUE)),
      con2k = 100 * (1 - sum(WhiffIndicator[Strikes == 2], na.rm = TRUE) / 
                       pmax(1, sum(SwingIndicator[Strikes == 2], na.rm = TRUE))),
      # Swing Decision Score: z_swing - chase (higher = better decisions)
      z_swing_pct = 100 * sum(Zswing, na.rm = TRUE) / pmax(1, sum(in_zone, na.rm = TRUE)),
      chase_pct = 100 * sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)),
rv_swing = 100 * mean(hitter_rv[SwingIndicator == 1], na.rm = TRUE),
rv_take = 100 * mean(hitter_rv[SwingIndicator == 0], na.rm = TRUE),
swing_dec_score = rv_swing - rv_take,
      .groups = "drop"
    )
  
  if (debug) {
    cat("\n--- Debug for", batter_name, "---\n")
    cat("Raw stats:\n")
    print(batter_split)
  }
  
  # Calculate grades using percentile-based approach
  batter_split <- batter_split %>%
    rowwise() %>%
    mutate(
      # Swing Decisions: Based on swing decision score (z_swing - chase), higher = better
      # Uses swing_dec_score which combines zone swing rate and chase rate
      swing_decisions = value_to_grade(swing_dec_score, grade_metrics$swing_dec_p20, grade_metrics$swing_dec_p50, grade_metrics$swing_dec_p80),
      
      # Game Power: ISO + EV90 combined (actual in-game power production)
      iso_grade = value_to_grade(iso, grade_metrics$iso_p20, grade_metrics$iso_p50, grade_metrics$iso_p80),
      ev90_grade = value_to_grade(ev90, grade_metrics$ev90_p20, grade_metrics$ev90_p50, grade_metrics$ev90_p80),
      game_power = round((iso_grade + ev90_grade) / 2),
      
      # Raw Power: EV90 only
      raw_power = value_to_grade(ev90, grade_metrics$ev90_p20, grade_metrics$ev90_p50, grade_metrics$ev90_p80),
      
      # Avoid K: K% (inverted) + 2K Contact Rate
      k_grade = value_to_grade(k_pct, grade_metrics$k_p20, grade_metrics$k_p50, grade_metrics$k_p80),
      con2k_grade = value_to_grade(con2k, grade_metrics$con2k_p20, grade_metrics$con2k_p50, grade_metrics$con2k_p80),
      avoid_k = round((k_grade + con2k_grade) / 2),
      
      # Contact: Zone Contact + Batting Average
      zcon_grade = value_to_grade(zone_con, grade_metrics$zone_con_p20, grade_metrics$zone_con_p50, grade_metrics$zone_con_p80),
      ba_grade = value_to_grade(ba, grade_metrics$ba_p20, grade_metrics$ba_p50, grade_metrics$ba_p80),
      contact = round((zcon_grade + ba_grade) / 2)
    ) %>%
    ungroup()
  
  if (debug) {
    cat("Final grades:\n")
    print(batter_split %>% select(PitcherThrows, swing_decisions, game_power, raw_power, avoid_k, contact))
  }
  
  return(batter_split)
}

# ============================================================================
# LAST N GAMES FUNCTIONS
# ============================================================================

get_last_n_games <- function(df, batter, n = 15) {
  df %>%
    dplyr::filter(Batter == batter) %>%
    dplyr::arrange(dplyr::desc(as.Date(Date))) %>%
    dplyr::distinct(Date, .keep_all = TRUE) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::pull(Date)
}

# ============================================================================
# COLOR FUNCTIONS
# ============================================================================

pill_color_higher_better <- function(val, benchmark, threshold_pct = 10) {
  if (is.na(val) || is.na(benchmark)) return(list(bg = "#E0E0E0", text = "#666666", class = "pill-gray"))
  # Special handling for rv100 where benchmark is 0
  if (benchmark == 0) {
    if (val > 0.5) return(list(class = "pill-green"))
    if (val > 0) return(list(class = "pill-light-green"))
    if (val >= -0.5) return(list(class = "pill-yellow"))
    return(list(class = "pill-red"))
  }
  pct_diff <- (val - benchmark) / abs(benchmark) * 100
  if (pct_diff > threshold_pct) return(list(bg = "#C8E6C9", text = "#2E7D32", class = "pill-green"))
  if (pct_diff > 0) return(list(bg = "#DCEDC8", text = "#558B2F", class = "pill-light-green"))
  if (pct_diff >= -threshold_pct) return(list(bg = "#FFF9C4", text = "#F57F17", class = "pill-yellow"))
  return(list(bg = "#FFCDD2", text = "#C62828", class = "pill-red"))
}

pill_color_lower_better <- function(val, benchmark, threshold_pct = 10) {
  if (is.na(val) || is.na(benchmark)) return(list(bg = "#E0E0E0", text = "#666666", class = "pill-gray"))
  pct_diff <- (benchmark - val) / abs(benchmark) * 100
  if (pct_diff > threshold_pct) return(list(bg = "#C8E6C9", text = "#2E7D32", class = "pill-green"))
  if (pct_diff > 0) return(list(bg = "#DCEDC8", text = "#558B2F", class = "pill-light-green"))
  if (pct_diff >= -threshold_pct) return(list(bg = "#FFF9C4", text = "#F57F17", class = "pill-yellow"))
  return(list(bg = "#FFCDD2", text = "#C62828", class = "pill-red"))
}

grade_color_light <- function(grade) {
  if (is.na(grade)) return("#BDBDBD")
  if (grade >= 70) return("#66BB6A")
  if (grade >= 60) return("#81C784")
  if (grade >= 55) return("#AED581")
  if (grade >= 45) return("#FFF176")
  if (grade >= 40) return("#FFB74D")
  if (grade >= 30) return("#FF8A65")
  return("#EF5350")
}

# Helper function to convert value to 20-80 grade using percentiles
value_to_grade <- function(val, p20, p50, p80, invert = FALSE) {
  if (is.na(val)) return(50)
  if (invert) { tmp <- p20; p20 <- p80; p80 <- tmp }
  # Linear interpolation: p20 -> 30, p50 -> 50, p80 -> 70
  if (val <= p20) {
    grade <- 20 + (val - (p20 - (p50 - p20))) / (p50 - p20) * 10
  } else if (val <= p50) {
    grade <- 30 + (val - p20) / (p50 - p20) * 20
  } else if (val <= p80) {
    grade <- 50 + (val - p50) / (p80 - p50) * 20
  } else {
    grade <- 70 + (val - p80) / (p80 - p50) * 10
  }
  round(pmax(20, pmin(80, grade)))
}

create_ba_pill <- function(value, benchmark, higher_better = TRUE) {
  if (is.na(value)) return(tags$span(class = "stat-pill pill-gray", "-"))
  colors <- if (higher_better) pill_color_higher_better(value, benchmark) else pill_color_lower_better(value, benchmark)
  tags$span(class = paste("stat-pill", colors$class), sprintf(".%03d", round(value * 1000)))
}

create_woba_pill <- function(value, benchmark, higher_better = TRUE) {
  if (is.na(value)) return(tags$span(class = "stat-pill pill-gray", "-"))
  colors <- if (higher_better) pill_color_higher_better(value, benchmark) else pill_color_lower_better(value, benchmark)
  tags$span(class = paste("stat-pill", colors$class), sprintf(".%03d", round(value * 1000)))
}

create_rv_pill <- function(value, benchmark = 0, higher_better = TRUE) {
  if (is.na(value)) return(tags$span(class = "stat-pill pill-gray", "-"))
  colors <- if (higher_better) pill_color_higher_better(value, benchmark) else pill_color_lower_better(value, benchmark)
  tags$span(class = paste("stat-pill", colors$class), sprintf("%+.2f", value))
}

create_pct_pill <- function(value, benchmark, higher_better = TRUE) {
  if (is.na(value)) return(tags$span(class = "stat-pill pill-gray", "-"))
  colors <- if (higher_better) pill_color_higher_better(value, benchmark) else pill_color_lower_better(value, benchmark)
  tags$span(class = paste("stat-pill", colors$class), paste0(sprintf("%.1f", value), "%"))
}

create_dec_pill <- function(value, benchmark, higher_better = TRUE) {
  if (is.na(value)) return(tags$span(class = "stat-pill pill-gray", "-"))
  colors <- if (higher_better) pill_color_higher_better(value, benchmark) else pill_color_lower_better(value, benchmark)
  tags$span(class = paste("stat-pill", colors$class), sprintf("%.1f", value))
}

create_stat_item <- function(value, label, benchmark, higher_better = TRUE, format_type = "decimal") {
  pill_fn <- switch(format_type, 
                    "woba" = create_woba_pill, "rv" = create_rv_pill,
                    "pct" = create_pct_pill, create_dec_pill)
  tags$div(class = "stat-item",
           tags$div(class = "stat-value", pill_fn(value, benchmark, higher_better)),
           tags$div(class = "stat-name", label))
}

create_grade_box <- function(grade, label) {
  tags$div(class = "grade-item",
           tags$div(class = "grade-box", style = paste0("background:", grade_color_light(grade), ";"), grade),
           tags$div(class = "grade-label", label))
}

# ============================================================================
# SEC PERCENTILE CHART FUNCTION
# ============================================================================

create_sec_percentile_chart <- function(batter_rows, pool_rows, batter_name, split_name = "Overall") {
  summarize_block <- function(df) {
    df %>%
      group_by(Batter) %>%
      summarize(
        RV100 = 100 * mean(mean_DRE_bat, na.rm = TRUE),
        wOBA = mean(woba, na.rm = TRUE),
        wOBACON = mean(wobacon, na.rm = TRUE),
        K_pct = sum(is_k, na.rm = TRUE) / pmax(1, sum(is_pa, na.rm = TRUE)) * 100,
        BB_pct = sum(is_walk, na.rm = TRUE) / pmax(1, sum(is_pa, na.rm = TRUE)) * 100,
        Whiff_pct = sum(is_whiff, na.rm = TRUE) / pmax(1, sum(is_swing, na.rm = TRUE)) * 100,
        Chase_pct = sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)) * 100,
        ZSwing_pct = sum(z_swing, na.rm = TRUE) / pmax(1, sum(in_zone, na.rm = TRUE)) * 100,
        ZCon_pct = 100 * (1 - sum(in_zone_whiff, na.rm = TRUE) / pmax(1, sum(z_swing, na.rm = TRUE))),
        OSwing_pct = sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)) * 100,
        Avg_EV = mean(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE),
        EV90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
        Max_EV = suppressWarnings(max(ExitSpeed[PitchCall == "InPlay"], na.rm = TRUE)),
        SLG = mean(slg, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(wOBA) | !is.na(RV100))
  }
  
  pool_summary <- summarize_block(pool_rows)
  batter_summary <- summarize_block(batter_rows) %>% filter(Batter == batter_name)
  
  if (!nrow(batter_summary) || !nrow(pool_summary)) {
    return(ggplot() + theme_void() + ggtitle(paste("No data for", batter_name)))
  }
  
  metrics <- c("RV/100", "wOBA", "wOBACON", "K%", "BB%", "Whiff%", "Chase%", 
               "Z-Swing%", "Z-Con%", "O-Swing%", "Avg EV", "EV90", "Max EV", "SLG")
  
  batter_vals <- c(batter_summary$RV100, batter_summary$wOBA, batter_summary$wOBACON,
                   batter_summary$K_pct, batter_summary$BB_pct, batter_summary$Whiff_pct,
                   batter_summary$Chase_pct, batter_summary$ZSwing_pct, batter_summary$ZCon_pct,
                   batter_summary$OSwing_pct, batter_summary$Avg_EV, batter_summary$EV90,
                   batter_summary$Max_EV, batter_summary$SLG)
  
  pool_lists <- list(pool_summary$RV100, pool_summary$wOBA, pool_summary$wOBACON,
                     pool_summary$K_pct, pool_summary$BB_pct, pool_summary$Whiff_pct,
                     pool_summary$Chase_pct, pool_summary$ZSwing_pct, pool_summary$ZCon_pct,
                     pool_summary$OSwing_pct, pool_summary$Avg_EV, pool_summary$EV90,
                     pool_summary$Max_EV, pool_summary$SLG)
  
  lower_better <- c("K%", "Whiff%", "Chase%", "O-Swing%")
  
  percentiles <- sapply(seq_along(metrics), function(i) {
    v <- batter_vals[i]; pop <- pool_lists[[i]]
    if (all(is.na(pop)) || is.na(v) || is.infinite(v)) return(NA_real_)
    if (metrics[i] %in% lower_better) {
      mean(pop >= v, na.rm = TRUE) * 100
    } else {
      mean(pop <= v, na.rm = TRUE) * 100
    }
  })
  
  formatted_values <- sapply(seq_along(metrics), function(i) {
    if (is.na(batter_vals[i]) || is.infinite(batter_vals[i])) return("-")
    if (metrics[i] %in% c("wOBA", "wOBACON", "SLG")) {
      sprintf("%.3f", batter_vals[i])
    } else if (metrics[i] == "RV/100") {
      sprintf("%+.2f", batter_vals[i])
    } else if (metrics[i] %in% c("Avg EV", "EV90", "Max EV")) {
      sprintf("%.1f", batter_vals[i])
    } else {
      sprintf("%.1f%%", batter_vals[i])
    }
  })
  
  chart_data <- data.frame(
    Metric = factor(metrics, levels = rev(metrics)),
    Value = formatted_values,
    Percentile = percentiles,
    Fill = colorRampPalette(c("#E1463E", "white", "#00840D"))(100)[pmax(1, pmin(100, round(ifelse(is.na(percentiles), 1, percentiles))))]
  )
  
  ggplot(chart_data, aes(y = Metric)) +
    geom_tile(aes(x = 50, width = 100), fill = "#c7dcdc", alpha = 0.3, height = 0.8) +
    geom_tile(aes(x = Percentile / 2, width = Percentile, fill = Fill), height = 0.7) +
    geom_text(aes(x = -3, label = Metric), hjust = 1, size = 3, color = "black") +
    geom_text(aes(x = 103, label = Value), hjust = 0, size = 3, color = "black") +
    geom_point(aes(x = Percentile), color = "black", size = 6, shape = 21, stroke = 1.5, fill = chart_data$Fill) +
    geom_text(aes(x = Percentile, label = round(Percentile)), size = 2.5, color = "black", fontface = "bold") +
    scale_x_continuous(limits = c(-20, 115), expand = c(0, 0)) +
    scale_fill_identity() + 
    theme_void() + 
    ggtitle(paste0("SEC Percentiles: ", split_name)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
          panel.background = element_rect(fill = "white", color = NA),
          axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
          plot.margin = margin(5, 10, 5, 10))
}

# ============================================================================
# GRADE METRICS & PROFILE CALCULATIONS
# ============================================================================

# Note: calculate_grade_metrics is deprecated in favor of calculate_enhanced_grade_metrics
# Keeping for backwards compatibility but using the enhanced version
calculate_grade_metrics <- function(tm_data) {
  calculate_enhanced_grade_metrics(tm_data)
}

gm <- calculate_grade_metrics(sec_pool_data)

calculate_hitter_profile <- function(hitter_name, tm_data, gm, pitcher_hand = "All") {
  h_data <- tm_data %>% filter(Batter == hitter_name)
  if (nrow(h_data) < 20) return(NULL)
  
  if (pitcher_hand != "All") {
    h_data <- h_data %>% filter(PitcherThrows == pitcher_hand)
    if (nrow(h_data) < 10) return(NULL)
  }
  
  # Use switch hitter detection
  h_side <- detect_batter_handedness(hitter_name, tm_data)
  n_total <- nrow(h_data)
  n_pa <- sum(h_data$is_pa, na.rm = TRUE)
  n_ab <- sum(h_data$is_ab, na.rm = TRUE)
  
  # Run Value from mean_DRE_bat
  rv100 <- 100 * mean(h_data$mean_DRE_bat, na.rm = TRUE)
  
  # wOBA and wOBACON from TM2025 columns
  woba <- mean(h_data$woba, na.rm = TRUE)
  wobacon <- mean(h_data$wobacon, na.rm = TRUE)
  
  # SLG
  slg <- mean(h_data$slg, na.rm = TRUE)
  
  # K% and BB% using binary indicators
  k_pct <- 100 * sum(h_data$is_k, na.rm = TRUE) / pmax(1, n_pa)
  bb_pct <- 100 * sum(h_data$is_walk, na.rm = TRUE) / pmax(1, n_pa)
  
  # Plate discipline using TM2025 columns
  n_swings <- sum(h_data$is_swing, na.rm = TRUE)
  n_ooz <- sum(h_data$out_of_zone, na.rm = TRUE)
  n_iz <- sum(h_data$in_zone, na.rm = TRUE)
  
  whiff_pct <- 100 * sum(h_data$is_whiff, na.rm = TRUE) / pmax(1, n_swings)
  chase_pct <- 100 * sum(h_data$chase, na.rm = TRUE) / pmax(1, n_ooz)
  zone_con_pct <- 100 * (1 - sum(h_data$in_zone_whiff, na.rm = TRUE) / pmax(1, sum(h_data$z_swing, na.rm = TRUE)))
  swing_pct <- 100 * n_swings / n_total
  z_swing_pct <- 100 * sum(h_data$z_swing, na.rm = TRUE) / pmax(1, n_iz)
  o_swing_pct <- chase_pct
  o_contact_pct <- if (sum(h_data$chase, na.rm = TRUE) >= 5) {
    chase_data <- h_data %>% filter(chase == 1)
    100 * (1 - sum(chase_data$is_whiff, na.rm = TRUE) / nrow(chase_data))
  } else NA
  
  # Put away %
  put_away_pct <- if (sum(h_data$TwoStrikeInd, na.rm = TRUE) >= 10) {
    100 * sum(h_data$is_put_away[h_data$TwoStrikeInd == 1], na.rm = TRUE) / sum(h_data$TwoStrikeInd, na.rm = TRUE)
  } else NA
  
  # Batted ball data
  bip_data <- h_data %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
  ev_mean <- if (nrow(bip_data) >= 5) mean(bip_data$ExitSpeed, na.rm = TRUE) else NA
  ev90 <- if (nrow(bip_data) >= 10) as.numeric(quantile(bip_data$ExitSpeed, 0.90, na.rm = TRUE)) else NA
  max_ev <- if (nrow(bip_data) >= 5) max(bip_data$ExitSpeed, na.rm = TRUE) else NA
  hard_hit_pct <- if (nrow(bip_data) >= 5) 100 * sum(bip_data$ExitSpeed >= 95, na.rm = TRUE) / nrow(bip_data) else NA
  
  la_mean <- if (nrow(bip_data) >= 5) mean(bip_data$Angle, na.rm = TRUE) else NA
  gb_pct <- if (nrow(bip_data) >= 10) 100 * sum(bip_data$Angle < 10, na.rm = TRUE) / nrow(bip_data) else NA
  ld_pct <- if (nrow(bip_data) >= 10) 100 * sum(bip_data$Angle >= 10 & bip_data$Angle < 25, na.rm = TRUE) / nrow(bip_data) else NA
  fb_pct <- if (nrow(bip_data) >= 10) 100 * sum(bip_data$Angle >= 25, na.rm = TRUE) / nrow(bip_data) else NA
  
  # Spray chart data
  bip_bearing <- h_data %>% filter(PitchCall == "InPlay", !is.na(Bearing))
  if (nrow(bip_bearing) >= 10) {
    if (h_side == "Right") {
      pull_pct <- 100 * sum(bip_bearing$Bearing < -15, na.rm = TRUE) / nrow(bip_bearing)
      oppo_pct <- 100 * sum(bip_bearing$Bearing > 15, na.rm = TRUE) / nrow(bip_bearing)
    } else {
      pull_pct <- 100 * sum(bip_bearing$Bearing > 15, na.rm = TRUE) / nrow(bip_bearing)
      oppo_pct <- 100 * sum(bip_bearing$Bearing < -15, na.rm = TRUE) / nrow(bip_bearing)
    }
    middle_pct <- 100 - pull_pct - oppo_pct
  } else { pull_pct <- oppo_pct <- middle_pct <- NA }
  
  # Grades using new percentile-based approach from grade_metrics_enhanced
  # Use calculate_hitter_grades function for consistency
  hitter_grades <- calculate_hitter_grades(hitter_name, tm_data, grade_metrics_enhanced)
  
  # Get overall grades (average across splits)
  if (pitcher_hand == "All") {
    swing_dec_grade <- round(mean(hitter_grades$swing_decisions, na.rm = TRUE))
    power_grade <- round(mean(hitter_grades$game_power, na.rm = TRUE))
    raw_power_grade <- round(mean(hitter_grades$raw_power, na.rm = TRUE))
    avoid_k_grade <- round(mean(hitter_grades$avoid_k, na.rm = TRUE))
    contact_grade <- round(mean(hitter_grades$contact, na.rm = TRUE))
  } else {
    split_grades <- hitter_grades %>% filter(PitcherThrows == pitcher_hand)
    if (nrow(split_grades) > 0) {
      swing_dec_grade <- split_grades$swing_decisions[1]
      power_grade <- split_grades$game_power[1]
      raw_power_grade <- split_grades$raw_power[1]
      avoid_k_grade <- split_grades$avoid_k[1]
      contact_grade <- split_grades$contact[1]
    } else {
      swing_dec_grade <- power_grade <- raw_power_grade <- avoid_k_grade <- contact_grade <- 50
    }
  }
  overall_grade <- round(mean(c(power_grade, contact_grade, avoid_k_grade, swing_dec_grade)))
  
  # Pitch family stats helper
  calc_fam_stats <- function(fam_data) {
    if (nrow(fam_data) < 10) return(list(n = nrow(fam_data), rv100 = NA, woba = NA, wobacon = NA, whiff = NA, chase = NA, hh_pct = NA))
    bip_fam <- fam_data %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
    n_sw <- sum(fam_data$is_swing, na.rm = TRUE)
    n_ooz_fam <- sum(fam_data$out_of_zone, na.rm = TRUE)
    list(n = nrow(fam_data), 
         rv100 = 100 * mean(fam_data$mean_DRE_bat, na.rm = TRUE),
         woba = mean(fam_data$woba, na.rm = TRUE),
         wobacon = mean(fam_data$wobacon, na.rm = TRUE),
         whiff = if (n_sw >= 5) 100 * sum(fam_data$is_whiff, na.rm = TRUE) / n_sw else NA,
         chase = if (n_ooz_fam >= 5) 100 * sum(fam_data$chase, na.rm = TRUE) / n_ooz_fam else NA,
         hh_pct = if (nrow(bip_fam) >= 3) 100 * sum(bip_fam$ExitSpeed >= 95, na.rm = TRUE) / nrow(bip_fam) else NA)
  }
  
  list(name = hitter_name, hand = h_side, n = n_total, n_pa = n_pa, n_ab = n_ab,
       rv100 = rv100, woba = woba, wobacon = wobacon, slg = slg, k_pct = k_pct, bb_pct = bb_pct,
       whiff_pct = whiff_pct, chase_pct = chase_pct, zone_con = zone_con_pct, swing_pct = swing_pct,
       z_swing = z_swing_pct, z_contact = zone_con_pct, o_swing = o_swing_pct, o_contact = o_contact_pct,
       put_away_pct = put_away_pct,
       ev_mean = ev_mean, ev90 = ev90, max_ev = max_ev, hard_hit_pct = hard_hit_pct, 
       la_mean = la_mean, gb_pct = gb_pct, ld_pct = ld_pct, fb_pct = fb_pct,
       pull_pct = pull_pct, oppo_pct = oppo_pct, middle_pct = middle_pct,
       overall_grade = overall_grade, power_grade = power_grade, raw_power_grade = raw_power_grade,
       contact_grade = contact_grade, avoid_k_grade = avoid_k_grade, swing_dec_grade = swing_dec_grade,
       fb_stats = calc_fam_stats(h_data %>% filter(PitchFamily == "FB")),
       bb_stats = calc_fam_stats(h_data %>% filter(PitchFamily == "BB")),
       os_stats = calc_fam_stats(h_data %>% filter(PitchFamily == "OS")))
}

calc_count_stats <- function(data) {
  if (nrow(data) < 5) return(list(n = 0, rv100 = NA, woba = NA, whiff = NA, chase = NA, swing_pct = NA))
  n_swings <- sum(data$is_swing, na.rm = TRUE)
  n_ooz <- sum(data$out_of_zone, na.rm = TRUE)
  list(n = nrow(data), 
       rv100 = 100 * mean(data$mean_DRE_bat, na.rm = TRUE),
       woba = mean(data$woba, na.rm = TRUE),
       whiff = if (n_swings >= 5) sum(data$is_whiff, na.rm = TRUE) / n_swings * 100 else NA,
       chase = if (n_ooz >= 5) sum(data$chase, na.rm = TRUE) / n_ooz * 100 else NA,
       swing_pct = if (nrow(data) >= 5) n_swings / nrow(data) * 100 else NA)
}

# ============================================================================
# VISUALIZATION FUNCTIONS
# ============================================================================

create_base_zone <- function(title = "", n = 0) {
  ggplot() +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.8) +
    annotate("polygon", x = c(-0.708, 0.708, 0.708, 0, -0.708), y = c(0.4, 0.4, 0.55, 0.75, 0.55), fill = NA, color = "black", linewidth = 0.6) +
    coord_fixed(ratio = 1) + xlim(-1.8, 1.8) + ylim(0, 4.5) + theme_void() +
    annotate("text", x = 0, y = 4.2, label = title, size = 2.5, fontface = "bold") +
    annotate("text", x = 1.3, y = 3.8, label = paste0("n=", n), size = 2, color = "gray50")
}

create_spray_chart <- function(data, title = "Spray Chart", filter_type = "all") {
  if (is.null(data) || nrow(data) < 5) return(ggplot() + theme_void() + annotate("text", x = 0, y = 200, label = "Insufficient data", size = 3, color = "gray50"))
  
  df <- data %>% filter(PitchCall == "InPlay", !is.na(Bearing), !is.na(Distance))
  if (filter_type == "early") { df <- df %>% filter(Strikes < 2); title <- "0-1 Strikes" }
  else if (filter_type == "2k") { df <- df %>% filter(Strikes == 2); title <- "2 Strikes" }
  else if (filter_type == "rhp") { df <- df %>% filter(PitcherThrows == "Right"); title <- "vs RHP" }
  
  if (nrow(df) < 3) return(ggplot() + theme_void() + annotate("text", x = 0, y = 200, label = paste0("n=", nrow(df)), size = 3, color = "gray50"))
  
  df <- df %>% mutate(Bearing2 = Bearing * pi / 180, x = Distance * sin(Bearing2), y = Distance * cos(Bearing2),
                      hit_type = case_when(PlayResult == "HomeRun" ~ "HR", PlayResult == "Triple" ~ "3B", 
                                           PlayResult == "Double" ~ "2B", PlayResult == "Single" ~ "1B", TRUE ~ "Out"))
  hit_colors <- c("HR" = "#E41A1C", "3B" = "#377EB8", "2B" = "#4DAF4A", "1B" = "#984EA3", "Out" = "gray50")
  
  ggplot(df, aes(x = x, y = y, color = hit_type)) +
    annotate("segment", x = 0, y = 0, xend = 247.487, yend = 247.487, color = "gray70", linewidth = 0.5) +
    annotate("segment", x = 0, y = 0, xend = -247.487, yend = 247.487, color = "gray70", linewidth = 0.5) +
    annotate("path", x = 300 * sin(seq(-pi/4, pi/4, length.out = 50)), y = 300 * cos(seq(-pi/4, pi/4, length.out = 50)), color = "gray70", linewidth = 0.5) +
    geom_point(size = 2, alpha = 0.8) + scale_color_manual(values = hit_colors, name = NULL) +
    coord_fixed(xlim = c(-320, 320), ylim = c(-20, 400)) + labs(title = paste0(title, " (n=", nrow(df), ")")) +
    theme_void() + theme(legend.position = "bottom", legend.text = element_text(size = 7), plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
}

create_heatmap <- function(data, title = "Heatmap", type = "whiff", colors = c("white", "#FFCCCC", "#FF6666", "#CC0000"),
                          pitch_family = "All", pitcher_hand = "All") {
  if (is.null(data)) return(create_base_zone(title, 0))
  
  # Filter by pitch family
  if (pitch_family != "All") {
    data <- data %>% filter(PitchFamily == pitch_family)
  }
  # Filter by pitcher hand
  if (pitcher_hand != "All") {
    data <- data %>% filter(PitcherThrows == pitcher_hand)
  }
  
  plot_data <- switch(type,
    "whiff" = data %>% filter(is_whiff == 1),
    "chase" = data %>% filter(chase == 1),
    "damage" = data %>% filter((is_hit == 1) | (PitchCall == "InPlay" & ExitSpeed >= 95)),
    "swing" = data %>% filter(is_swing == 1),
    data)
  plot_data <- plot_data %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
  
  if (nrow(plot_data) < 5) return(create_base_zone(title, nrow(plot_data)))
  
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
    scale_fill_gradientn(colours = colors, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.8) +
    annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708), y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.6) +
    coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) + labs(title = paste0(title, " (n=", nrow(plot_data), ")")) +
    theme_void() + theme(legend.position = "none", plot.title = element_text(size = 8, face = "bold", hjust = 0.5))
}

create_hit_out_chart <- function(tm_data, batter_name, chart_type = "overall_hits") {
  if (is.null(tm_data)) return(create_base_zone("No Data", 0))
  h_data <- tm_data %>% filter(Batter == batter_name, !is.na(PlateLocSide), !is.na(PlateLocHeight),
                                PlateLocSide >= -2.5, PlateLocSide <= 2.5, PlateLocHeight >= 0, PlateLocHeight <= 5)
  
  filter_map <- list(
    "early_hits" = list(filter = quote(Strikes < 2 & is_hit == 1), title = "0-1 Strike Hits"),
    "early_outs" = list(filter = quote(Strikes < 2 & PitchCall == "InPlay" & is_hit == 0), title = "0-1 Strike Outs"),
    "2k_hits" = list(filter = quote(TwoStrikeInd == 1 & is_hit == 1), title = "2 Strike Hits"),
    "2k_outs" = list(filter = quote(TwoStrikeInd == 1 & PitchCall == "InPlay" & is_hit == 0), title = "2 Strike Outs"),
    "rhp_hits" = list(filter = quote(PitcherThrows == "Right" & is_hit == 1), title = "vs RHP Hits"),
    "rhp_outs" = list(filter = quote(PitcherThrows == "Right" & PitchCall == "InPlay" & is_hit == 0), title = "vs RHP Outs"),
    "lhp_hits" = list(filter = quote(PitcherThrows == "Left" & is_hit == 1), title = "vs LHP Hits"),
    "lhp_outs" = list(filter = quote(PitcherThrows == "Left" & PitchCall == "InPlay" & is_hit == 0), title = "vs LHP Outs"))
  
  if (!chart_type %in% names(filter_map)) return(create_base_zone("Invalid Type", 0))
  filter_info <- filter_map[[chart_type]]
  plot_data <- h_data %>% filter(!!filter_info$filter)
  if (nrow(plot_data) < 1) return(create_base_zone(filter_info$title, 0))
  
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.8) +
    annotate("polygon", x = c(-0.708, 0.708, 0.708, 0, -0.708), y = c(0.4, 0.4, 0.55, 0.75, 0.55), fill = NA, color = "black", linewidth = 0.6) +
    geom_point(aes(color = PitchFamily), size = 2, alpha = 0.7) +
    scale_color_manual(values = c("FB" = "#FA8072", "BB" = "#A020F0", "OS" = "#2E8B57", "Other" = "#888888"), guide = "none") +
    annotate("text", x = 0, y = 4.2, label = filter_info$title, size = 2.5, fontface = "bold") +
    annotate("text", x = 1.3, y = 3.8, label = paste0("n=", nrow(plot_data)), size = 2, color = "gray50") +
    coord_fixed(ratio = 1) + xlim(-1.8, 1.8) + ylim(0, 4.5) + theme_void()
}

# ============================================================================
# ADVANCED HEAT MAP WITH STATS - For detailed pitch profile analysis
# ============================================================================

create_advanced_heatmap_with_stats <- function(data, title = "Heatmap", stat_value = NA, n_pitches = NA,
                                                colors = c("white", "#FFCCCC", "#FF6666", "#CC0000"),
                                                type = "whiff") {
  if (is.null(data)) return(create_base_zone(title, 0))
  
  plot_data <- switch(type,
    "whiff" = data %>% filter(is_whiff == 1),
    "chase" = data %>% filter(chase == 1),
    "damage" = data %>% filter((is_hit == 1) | (PitchCall == "InPlay" & ExitSpeed >= 95)),
    "swing" = data %>% filter(is_swing == 1),
    "iz_whiff" = data %>% filter(in_zone_whiff == 1),
    data)
  plot_data <- plot_data %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
  
  # Format stat display
  stat_text <- if (!is.na(stat_value)) {
    if (stat_value > 1) sprintf("%.1f%%", stat_value) else sprintf("%.3f", stat_value)
  } else "-"
  n_text <- if (!is.na(n_pitches)) paste0("n=", n_pitches) else ""
  
  if (nrow(plot_data) < 3) {
    # Return minimal zone with stats overlay
    p <- ggplot() +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = "#f5f5f5", color = "black", linewidth = 0.8) +
      annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708), y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.6) +
      annotate("text", x = 0, y = 2.5, label = stat_text, size = 4, fontface = "bold", color = "#333") +
      annotate("text", x = 0, y = 4.2, label = title, size = 2.5, fontface = "bold") +
      annotate("text", x = 1.3, y = 3.8, label = n_text, size = 2, color = "gray50") +
      coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) + theme_void()
    return(p)
  }
  
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, alpha = 0.8) +
    scale_fill_gradientn(colours = colors, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.8) +
    annotate("path", x = c(-0.708, 0.708, 0.708, 0, -0.708, -0.708), y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15), color = "black", linewidth = 0.6) +
    # Stats overlay
    annotate("rect", xmin = -0.5, xmax = 0.5, ymin = 2.2, ymax = 2.8, fill = "white", color = NA, alpha = 0.85) +
    annotate("text", x = 0, y = 2.5, label = stat_text, size = 3.5, fontface = "bold", color = "#333") +
    annotate("text", x = 0, y = 4.2, label = title, size = 2.5, fontface = "bold") +
    annotate("text", x = 1.3, y = 3.8, label = n_text, size = 2, color = "gray50") +
    coord_fixed(xlim = c(-2, 2), ylim = c(0, 4.5)) +
    theme_void() + theme(legend.position = "none")
}

# ============================================================================
# ADVANCED HEAT MAP STATS TABLE GENERATOR
# ============================================================================

# Generate advanced heat map stats table for a batter
get_advanced_heatmap_stats <- function(batter_name, pitcher_hand, split_type = "pitch_type") {
  # Stats columns: Pitch Frequency, Swing%, wOBA, Damage (95+), Chase, Whiff, IZ Whiff, IZ Damage
  
  if (split_type == "pitch_type") {
    # Split by pitch type: 4S, 2S/Si, SL/SW, CB, CH/Spl, Overall
    pitch_types <- c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl")
    
    # Get data from cache
    cached_data <- batter_pitch_profile_cache %>%
      filter(Batter == batter_name, PitcherThrows == pitcher_hand)
    
    # Build stats for each pitch type
    stats_list <- lapply(pitch_types, function(pt) {
      pt_data <- cached_data %>% filter(DetailedPitchType == pt)
      if (nrow(pt_data) == 0) {
        return(list(
          pitch_type = pt,
          n = 0, pitch_freq = 0, swing_pct = NA, woba = NA,
          damage_pct = NA, chase_pct = NA, whiff_pct = NA, 
          iz_whiff_pct = NA, iz_damage_pct = NA
        ))
      }
      list(
        pitch_type = pt,
        n = pt_data$n_pitches[1],
        pitch_freq = pt_data$n_pitches[1],
        swing_pct = pt_data$swing_pct[1],
        woba = pt_data$woba[1],
        damage_pct = pt_data$damage_pct[1],
        chase_pct = pt_data$chase_pct[1],
        whiff_pct = pt_data$whiff_pct[1],
        iz_whiff_pct = pt_data$iz_whiff_pct[1],
        iz_damage_pct = pt_data$iz_damage_pct[1]
      )
    })
    
    # Add overall row
    overall_data <- batter_overall_cache %>%
      filter(Batter == batter_name, PitcherThrows == pitcher_hand)
    
    if (nrow(overall_data) > 0) {
      stats_list[[length(stats_list) + 1]] <- list(
        pitch_type = "Overall",
        n = overall_data$n_pitches[1],
        pitch_freq = overall_data$n_pitches[1],
        swing_pct = overall_data$swing_pct[1],
        woba = overall_data$woba[1],
        damage_pct = overall_data$damage_pct[1],
        chase_pct = overall_data$chase_pct[1],
        whiff_pct = overall_data$whiff_pct[1],
        iz_whiff_pct = overall_data$iz_whiff_pct[1],
        iz_damage_pct = overall_data$iz_damage_pct[1]
      )
    }
    
    return(stats_list)
    
  } else if (split_type == "count_type") {
    # Split by count type: 1P, 2K, Ahead, Behind, Last 15
    count_types <- c("1P", "2K", "Ahead", "Behind")
    
    # Get data from cache
    cached_data <- batter_count_profile_cache %>%
      filter(Batter == batter_name, PitcherThrows == pitcher_hand)
    
    # Build stats for each count type
    stats_list <- lapply(count_types, function(ct) {
      ct_data <- cached_data %>% filter(CountType == ct)
      if (nrow(ct_data) == 0) {
        return(list(
          count_type = ct,
          n = 0, pitch_freq = 0, swing_pct = NA, woba = NA,
          damage_pct = NA, chase_pct = NA, whiff_pct = NA,
          iz_whiff_pct = NA, iz_damage_pct = NA
        ))
      }
      list(
        count_type = ct,
        n = ct_data$n_pitches[1],
        pitch_freq = ct_data$n_pitches[1],
        swing_pct = ct_data$swing_pct[1],
        woba = ct_data$woba[1],
        damage_pct = ct_data$damage_pct[1],
        chase_pct = ct_data$chase_pct[1],
        whiff_pct = ct_data$whiff_pct[1],
        iz_whiff_pct = ct_data$iz_whiff_pct[1],
        iz_damage_pct = ct_data$iz_damage_pct[1]
      )
    })
    
    # Add Last 15 row from cache
    last15_data <- batter_last15_cache %>%
      filter(Batter == batter_name, PitcherThrows == pitcher_hand)
    
    if (nrow(last15_data) > 0) {
      stats_list[[length(stats_list) + 1]] <- list(
        count_type = "Last 15",
        n = last15_data$n_pitches[1],
        pitch_freq = last15_data$n_pitches[1],
        swing_pct = last15_data$swing_pct[1],
        woba = last15_data$woba[1],
        damage_pct = last15_data$damage_pct[1],
        chase_pct = last15_data$chase_pct[1],
        whiff_pct = last15_data$whiff_pct[1],
        iz_whiff_pct = last15_data$iz_whiff_pct[1],
        iz_damage_pct = last15_data$iz_damage_pct[1]
      )
    } else {
      stats_list[[length(stats_list) + 1]] <- list(
        count_type = "Last 15",
        n = 0, pitch_freq = 0, swing_pct = NA, woba = NA,
        damage_pct = NA, chase_pct = NA, whiff_pct = NA,
        iz_whiff_pct = NA, iz_damage_pct = NA
      )
    }
    
    return(stats_list)
  }
  
  return(list())
}

# ============================================================================
# TREND CHARTS - Rolling stat averages by game date
# ============================================================================

create_trend_chart <- function(batter_name, tm_data, selected_stats = c("swing_pct", "woba", "chase_pct", "whiff_pct"),
                               start_date = NULL, end_date = NULL, rolling_games = 5) {
  # Get batter data with dates
  h_data <- tm_data %>%
    filter(Batter == batter_name, !is.na(Date)) %>%
    mutate(Date = as.Date(Date))
  
  if (nrow(h_data) < 10) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "Insufficient data for trend analysis", size = 4, color = "gray50"))
  }
  
  # Apply date filter if specified
  if (!is.null(start_date)) {
    h_data <- h_data %>% filter(Date >= as.Date(start_date))
  }
  if (!is.null(end_date)) {
    h_data <- h_data %>% filter(Date <= as.Date(end_date))
  }
  
  if (nrow(h_data) < 10) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "Insufficient data in selected date range", size = 4, color = "gray50"))
  }
  
  # Calculate daily stats
  daily_stats <- h_data %>%
    group_by(Date) %>%
    summarise(
      n_pitches = n(),
      n_swings = sum(is_swing, na.rm = TRUE),
      n_whiffs = sum(is_whiff, na.rm = TRUE),
      n_out_zone = sum(out_of_zone, na.rm = TRUE),
      n_chase = sum(chase, na.rm = TRUE),
      n_in_zone = sum(in_zone, na.rm = TRUE),
      n_iz_swing = sum(z_swing, na.rm = TRUE),
      n_iz_whiff = sum(in_zone_whiff, na.rm = TRUE),
      n_bip = sum(PitchCall == "InPlay", na.rm = TRUE),
      n_damage = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE),
      woba_sum = sum(woba, na.rm = TRUE),
      woba_n = sum(!is.na(woba)),
      .groups = "drop"
    ) %>%
    arrange(Date) %>%
    mutate(
      swing_pct = ifelse(n_pitches > 0, 100 * n_swings / n_pitches, NA),
      whiff_pct = ifelse(n_swings > 0, 100 * n_whiffs / n_swings, NA),
      chase_pct = ifelse(n_out_zone > 0, 100 * n_chase / n_out_zone, NA),
      woba = ifelse(woba_n > 0, woba_sum / woba_n, NA),
      iz_whiff_pct = ifelse(n_iz_swing > 0, 100 * n_iz_whiff / n_iz_swing, NA),
      iz_damage_pct = ifelse(n_bip > 0, 100 * n_damage / n_bip, NA)
    )
  
  # Calculate rolling averages - handles edge cases properly
  calc_rolling_avg <- function(values, window = rolling_games) {
    n <- length(values)
    if (n == 0) return(numeric(0))
    if (n < window) {
      # If fewer values than window, use expanding window
      result <- rep(NA_real_, n)
      for (i in seq_len(n)) {
        result[i] <- mean(values[1:i], na.rm = TRUE)
      }
      return(result)
    }
    # Standard rolling average
    result <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      start_idx <- max(1, i - window + 1)
      result[i] <- mean(values[start_idx:i], na.rm = TRUE)
    }
    result
  }
  
  # Handle case where daily_stats might have 0 rows
  if (nrow(daily_stats) == 0) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected period", size = 4, color = "gray50"))
  }
  
  # Apply rolling averages safely
  daily_stats <- daily_stats %>%
    ungroup() %>%
    mutate(
      swing_pct_roll = calc_rolling_avg(swing_pct, rolling_games),
      whiff_pct_roll = calc_rolling_avg(whiff_pct, rolling_games),
      chase_pct_roll = calc_rolling_avg(chase_pct, rolling_games),
      woba_roll = calc_rolling_avg(woba, rolling_games),
      iz_whiff_pct_roll = calc_rolling_avg(iz_whiff_pct, rolling_games),
      iz_damage_pct_roll = calc_rolling_avg(iz_damage_pct, rolling_games)
    )
  
  # Prepare data for plotting
  stat_mapping <- list(
    "swing_pct" = list(col = "swing_pct_roll", label = "Swing%", color = "#1976D2"),
    "woba" = list(col = "woba_roll", label = "wOBA", color = "#388E3C"),
    "chase_pct" = list(col = "chase_pct_roll", label = "Chase%", color = "#F57C00"),
    "whiff_pct" = list(col = "whiff_pct_roll", label = "Whiff%", color = "#D32F2F"),
    "iz_whiff_pct" = list(col = "iz_whiff_pct_roll", label = "IZ Whiff%", color = "#7B1FA2"),
    "iz_damage_pct" = list(col = "iz_damage_pct_roll", label = "IZ Damage%", color = "#00796B")
  )
  
  # Filter to selected stats
  plot_data <- daily_stats %>%
    select(Date, ends_with("_roll")) %>%
    pivot_longer(cols = -Date, names_to = "stat", values_to = "value") %>%
    mutate(
      stat_clean = gsub("_roll", "", stat),
      stat_label = sapply(stat_clean, function(s) {
        if (s %in% names(stat_mapping)) stat_mapping[[s]]$label else s
      }),
      stat_color = sapply(stat_clean, function(s) {
        if (s %in% names(stat_mapping)) stat_mapping[[s]]$color else "gray50"
      })
    ) %>%
    filter(stat_clean %in% selected_stats, !is.na(value))
  
  if (nrow(plot_data) == 0) {
    return(ggplot() + theme_void() + 
             annotate("text", x = 0.5, y = 0.5, label = "No data for selected stats", size = 4, color = "gray50"))
  }
  
  # Create separate scales for wOBA vs percentage stats
  # wOBA is typically 0.200-0.500, percentages are 0-100
  has_woba <- "woba" %in% selected_stats
  has_pct <- any(selected_stats %in% c("swing_pct", "whiff_pct", "chase_pct", "iz_whiff_pct", "iz_damage_pct"))
  
  # Build the plot - unclass to avoid jsonlite serialization warnings
  color_vals <- setNames(
    sapply(selected_stats, function(s) stat_mapping[[s]]$color, USE.NAMES = FALSE),
    sapply(selected_stats, function(s) stat_mapping[[s]]$label, USE.NAMES = FALSE)
  )
  
  p <- ggplot(plot_data, aes(x = Date, y = value, color = stat_label, group = stat_label)) +
    geom_line(linewidth = 1.2, alpha = 0.9) +
    geom_point(size = 2, alpha = 0.7) +
    scale_color_manual(values = color_vals) +
    labs(
      title = paste0("Rolling ", rolling_games, "-Game Trends: ", batter_name),
      x = "Date", y = "Value",
      color = "Stat"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Add facets if mixing wOBA (different scale) with percentages
  if (has_woba && has_pct) {
    p <- p + facet_wrap(~stat_label, scales = "free_y", ncol = 2)
  }
  
  p
}

# Helper function to get date range for a batter
get_batter_date_range <- function(batter_name, tm_data) {
  h_data <- tm_data %>%
    filter(Batter == batter_name, !is.na(Date)) %>%
    mutate(Date = as.Date(Date))
  
  if (nrow(h_data) == 0) {
    return(list(min = Sys.Date() - 365, max = Sys.Date()))
  }
  
  list(
    min = min(h_data$Date, na.rm = TRUE),
    max = max(h_data$Date, na.rm = TRUE)
  )
}

# ============================================================================
# CSS STYLES
# ============================================================================

app_css <- HTML("
  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #f5f5f5; }
  .app-header { display:flex; justify-content:space-between; align-items:center; padding:15px 30px; background:#fff; border-bottom:3px solid #006F71; margin-bottom:15px; }
  .header-title { font-size: 28px; font-weight: bold; color: #006F71; }
  .header-subtitle { font-size: 14px; color: #666; }
  .chart-box { background: white; border-radius: 12px; border: 1px solid rgba(0,111,113,.2); padding: 12px; margin-bottom: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.05); }
  .chart-box-title { font-weight: 700; color: #006F71; font-size: 14px; margin-bottom: 10px; text-align: center; border-bottom: 2px solid rgba(0,111,113,.2); padding-bottom: 6px; }
  .stats-header { background: linear-gradient(135deg, #6A1B9A, #AB47BC); color: white; padding: 12px 20px; border-radius: 12px; margin-bottom: 15px; }
  .stats-header h3 { margin: 0; font-weight: bold; }
  .stats-header p { margin: 0; opacity: 0.9; font-size: 13px; }
  .btn-bronze { background: linear-gradient(135deg, #CD853F, #DAA520); color: white; border: none; border-radius: 25px; padding: 10px 25px; font-weight: 600; }
  .stat-pill { display: inline-block; padding: 4px 12px; border-radius: 20px; font-weight: 600; font-size: 12px; min-width: 55px; text-align: center; }
  .pill-green { background: #C8E6C9; color: #2E7D32; }
  .pill-light-green { background: #DCEDC8; color: #558B2F; }
  .pill-yellow { background: #FFF9C4; color: #F57F17; }
  .pill-red { background: #FFCDD2; color: #C62828; }
  .pill-gray { background: #E0E0E0; color: #666666; }
  .player-list-item { padding: 10px 15px; border: 2px solid #ddd; border-radius: 8px; margin-bottom: 8px; background: white; cursor: pointer; display: flex; justify-content: space-between; align-items: center; }
  .player-list-item:hover { border-color: #006F71; background: #f8fffe; }
  .player-list-item.selected { border-color: #006F71; border-width: 3px; background: #e8f5f4; }
  .player-name { font-weight: bold; font-size: 14px; }
  .player-grade { width: 32px; height: 32px; line-height: 32px; text-align: center; border-radius: 6px; font-weight: bold; font-size: 13px; }
  .detail-section { background: #fafafa; border-radius: 8px; padding: 12px; margin-bottom: 12px; border: 1px solid #e0e0e0; }
  .section-header { background: linear-gradient(135deg, #37474F, #546E7A); color: white; padding: 8px 12px; border-radius: 6px; margin-bottom: 10px; font-weight: 600; font-size: 13px; }
  .section-header-blue { background: linear-gradient(135deg, #1565C0, #1976D2); }
  .section-header-purple { background: linear-gradient(135deg, #6A1B9A, #8E24AA); }
  .section-header-green { background: linear-gradient(135deg, #2E7D32, #43A047); }
  .section-header-orange { background: linear-gradient(135deg, #E65100, #F57C00); }
  .section-header-red { background: linear-gradient(135deg, #C62828, #E53935); }
  .section-header-teal { background: linear-gradient(135deg, #00695C, #00897B); }
  .grade-row { display: flex; gap: 10px; margin-bottom: 10px; flex-wrap: wrap; justify-content: center; }
  .grade-item { text-align: center; }
  .grade-box { width: 44px; height: 44px; line-height: 44px; text-align: center; font-weight: bold; font-size: 15px; border-radius: 8px; border: 1px solid #999; }
  .grade-label { font-size: 10px; color: #666; margin-top: 3px; }
  .stat-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; }
  .stat-item { text-align: center; padding: 10px 6px; background: white; border-radius: 8px; border: 1px solid #e0e0e0; }
  .stat-value { font-size: 14px; font-weight: bold; }
  .stat-name { font-size: 10px; color: #666; margin-top: 3px; }
  .splits-table { width: 100%; font-size: 11px; }
  .splits-table th { background: #37474F; color: white; padding: 6px 8px; text-align: center; font-weight: 600; }
  .splits-table td { padding: 5px 8px; text-align: center; border-bottom: 1px solid #eee; }
  .splits-table tr:nth-child(even) { background: #f9f9f9; }
  .splits-label { text-align: left !important; font-weight: 600; }
  .scroll-container { max-height: 600px; overflow-y: auto; }
  .hitter-header { display: flex; justify-content: space-between; align-items: flex-start; padding-bottom: 10px; border-bottom: 2px solid #006F71; margin-bottom: 15px; }
  .hitter-info h3 { margin: 0 0 5px 0; color: #006F71; font-size: 22px; }
  .hitter-meta { color: #666; font-size: 13px; }
  .pitch-legend { display: flex; gap: 12px; justify-content: center; margin-top: 8px; font-size: 11px; }
  .legend-item { display: flex; align-items: center; gap: 4px; }
  .legend-dot { width: 10px; height: 10px; border-radius: 50%; }
  .legend-fb { background: #FA8072; }
  .legend-bb { background: #A020F0; }
  .legend-os { background: #2E8B57; }
  .heatmap-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; }
  .heatmap-row { display: flex; justify-content: space-around; gap: 4px; }
  .symmetric-row { display: flex; justify-content: center; gap: 12px; }
  .matrix-table { border-collapse: collapse; width: 100%; }
  .matrix-table th, .matrix-table td { border: 1px solid #ddd; padding: 8px; text-align: center; }
  .matrix-table th { background: #006F71; color: white; }
  .nav-tabs > li > a { color: #006F71; font-weight: 600; }
  .nav-tabs > li.active > a { background: #006F71 !important; color: white !important; }
  .nav-pills > li > a { color: #006F71; }
  .nav-pills > li.active > a { background: #006F71 !important; color: white !important; }
  
  /* Advanced Heat Maps Styles */
  .adv-heatmap-container { margin-top: 15px; }
  .adv-heatmap-table { width: 100%; border-collapse: collapse; font-size: 11px; }
  .adv-heatmap-table th { background: #006F71; color: white; padding: 8px; text-align: center; font-weight: 600; }
  .adv-heatmap-table td { padding: 6px; text-align: center; border-bottom: 1px solid #eee; }
  .adv-heatmap-table tr:nth-child(even) { background: #f9f9f9; }
  .adv-heatmap-table .row-label { text-align: left; font-weight: 600; background: #f5f5f5; }
  
  /* Trend Chart Styles */
  .trend-controls { background: #f8f9fa; padding: 12px; border-radius: 8px; margin-bottom: 15px; }
  .trend-controls .checkbox-inline { margin-right: 15px; }
  .trend-controls label { font-weight: 500; }
  
  /* Toggle switch for advanced heatmaps */
  .shiny-input-container.checkbox { margin-bottom: 10px; }
  .shiny-input-container.checkbox label { font-weight: 600; color: #006F71; }
")

# ============================================================================
# UI
# ============================================================================
login_ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f0f4f8;
      font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      color: #006F71;
    }
    .login-container {
      max-width: 360px;
      margin: 120px auto;
      background: #A27752;
      padding: 30px 25px;
      border-radius: 8px;
      box-shadow: 0 4px 15px #A1A1A4;
      text-align: center;
      color: white;
    }
    .login-message {
      margin-bottom: 20px;
      font-size: 14px;
      color: #ffffff;
      font-weight: 600;
    }
    .btn-primary {
      background-color: #006F71 !important;
      border-color: #006F71 !important;
      color: white !important;
      font-weight: bold;
      width: 100%;
      margin-top: 10px;
      box-shadow: 0 2px 5px #006F71;
      transition: background-color 0.3s ease;
    }
    .btn-primary:hover {
      background-color: #006F71 !important;
      border-color: #A27752 !important;
    }
    .form-control {
      border-radius: 4px;
      border: 1.5px solid #006F71 !important;
      color: #006F71;
      font-weight: 600;
    }
  ")),
  
  div(class = "login-container",
      tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/e/ef/Coastal_Carolina_Chanticleers_logo.svg/1200px-Coastal_Carolina_Chanticleers_logo.svg.png", height = "150px"),
      passwordInput("password", "Password:"),
      actionButton("login", "Login"),
      textOutput("wrong_pass")
  )
)


  
app_ui <- fluidPage(
  tags$head(tags$style(app_css)),
  div(class = "app-header",
      div(div(class = "header-title", "Hitter Scouting Cards"), div(class = "header-subtitle", "Enhanced Analytics Dashboard - SEC Pool Comparison"))),
  
  tabsetPanel(id = "main_tabs", type = "tabs",
    # ===== SCOUTING CARDS TAB =====
    tabPanel("Scouting Cards",
      div(class = "stats-header", h3("Enhanced Hitter Scouting Cards"), p("Comprehensive stats, heatmaps, and charts with SEC percentile rankings")),
      
      fluidRow(
        column(3,
               div(class = "chart-box",
                   div(class = "chart-box-title", "Select Hitters"),
                   selectizeInput("scout_hitters", "Add Hitters:", choices = NULL, multiple = TRUE, 
                                  options = list(placeholder = "Search hitters...", maxItems = 30)),
                   hr(),
                   div(class = "scroll-container", style = "max-height: 500px;", uiOutput("hitter_list_ui")),
                   hr(),
                   textInput("pdf_custom_title", "Custom PDF Title:", placeholder = "Optional: Enter a custom title"),
                   downloadButton("download_scout_pdf", "Download Scouting Report (PDF)", class = "btn-bronze", style = "width: 100%;"))),
        column(9, uiOutput("hitter_detail_panel")))
    ),
    
    # ===== MAC MATCHUP MATRIX TAB =====
    # Based on: "Using Euclidean Distance and Clustering to quantify batter vs. pitcher matchups"
    # by Chap Cunningham and Zack Aisen (Saber Seminar 2025)
    tabPanel("MAC Matchup Matrix",
      div(class = "stats-header", 
          h3("MAC Matchup Analysis"),
          p("Matchup Analysis using Euclidean Distance and Clustering (MAC) - Score 0-100 where 100 = Pitcher Advantage")),
      
      fluidRow(
        column(12,
          div(class = "chart-box", style = "margin-top: 15px;",
            div(class = "chart-box-title", "MAC Batter vs Pitcher Matchups"),
            fluidRow(
              column(4,
                selectizeInput("mac_pitchers", "Select Pitchers:", choices = NULL, multiple = TRUE,
                               options = list(placeholder = "Search pitchers...", maxItems = 12))),
              column(4,
                selectizeInput("mac_hitters", "Select Hitters:", choices = NULL, multiple = TRUE,
                               options = list(placeholder = "Search hitters...", maxItems = 12))),
              column(4,
                div(style = "margin-top: 25px;",
                  actionButton("generate_mac_matrix", "Generate MAC Matrix", class = "btn-bronze", style = "width: 100%; margin-bottom: 10px;"),
                  downloadButton("download_mac_png", "Download as PNG", class = "btn-bronze", style = "width: 100%;")))
            ),
            div(style = "margin-top: 10px; padding: 10px; background: #f5f5f5; border-radius: 5px;",
              div(style = "font-size: 12px; color: #333;",
                tags$b("MAC Score (0-100):"), " Uses Euclidean distance (threshold 0.6) to find similar pitches hitters have faced."),
              div(style = "display: flex; gap: 10px; margin-top: 8px; font-size: 11px; flex-wrap: wrap;",
                tags$span(style = "background: #1B5E20; color: white; padding: 3px 10px; border-radius: 3px;", "80-100 Strong Pitcher"),
                tags$span(style = "background: #4CAF50; color: white; padding: 3px 10px; border-radius: 3px;", "60-79 Pitcher Adv"),
                tags$span(style = "background: #FFF9C4; padding: 3px 10px; border-radius: 3px;", "40-59 Neutral"),
                tags$span(style = "background: #FF9800; color: white; padding: 3px 10px; border-radius: 3px;", "20-39 Hitter Adv"),
                tags$span(style = "background: #C62828; color: white; padding: 3px 10px; border-radius: 3px;", "0-19 Strong Hitter")
              ),
              div(style = "margin-top: 8px; font-size: 11px; color: #666;",
                tags$b("Methodology:"), " Scans features (Velo, IVB, HB, Spin, Rel Ht, Rel Side) to find similar pitches. Click cells to add notes.",
                tags$br(),
                tags$b("RV/100:"), " Run Value per 100 pitches on similar pitches (+ = Hitter advantage). FB/BB/OS = Fastball/Breaking/Offspeed.")
            ),
            hr(),
            div(id = "mac_matrix_container", style = "overflow-x: auto;", 
                uiOutput("mac_matrix_output"))
          )
        )
      )
    )
  )
)



ui <- fluidPage(
  uiOutput("page")
)
# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  logged_in <- reactiveVal(FALSE)
  
  output$page <- renderUI({
    if (logged_in()) {
      app_ui
    } else {
      login_ui
    }
  })
  
  observeEvent(input$login, {
    if (input$password == PASSWORD) {
      logged_in(TRUE)
      output$wrong_pass <- renderText("")
    } else {
      output$wrong_pass <- renderText("Incorrect password, please try again.")
    }
  })
  
  scout_data <- reactiveVal(list())
  selected_hitter <- reactiveVal(NULL)
  # MAC Matrix reactive values
  mac_matrix_data <- reactiveVal(NULL)
  mac_notes <- reactiveVal(list())  # Store notes for each matchup cell
  
  # Initialize all selectize inputs AFTER login
  observeEvent(logged_in(), {
    if (logged_in()) {
      updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE) 
      # MAC Matchup Matrix inputs
      updateSelectizeInput(session, "mac_pitchers", choices = all_pitchers, server = TRUE)
      updateSelectizeInput(session, "mac_hitters", choices = all_hitters, server = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$scout_hitters, {
    hitters <- input$scout_hitters
    current_data <- scout_data()
    for (h in hitters) {
      if (is.null(current_data[[h]])) {
        profile <- calculate_hitter_profile(h, tm_data, gm)
        if (!is.null(profile)) current_data[[h]] <- list(profile = profile, lhp_plan = "", rhp_plan = "", overall_notes = "")
      }
    }
    current_data <- current_data[names(current_data) %in% hitters]
    scout_data(current_data)
    if (is.null(selected_hitter()) && length(hitters) > 0) selected_hitter(hitters[1])
  })
  
  observeEvent(input$select_hitter, { selected_hitter(input$select_hitter) })
  
  output$hitter_list_ui <- renderUI({
    data <- scout_data()
    if (length(data) == 0) return(div(style = "text-align: center; padding: 20px; color: #666;", p("Add hitters above to begin")))
    sel <- selected_hitter()
    lapply(names(data), function(h_name) {
      profile <- data[[h_name]]$profile
      rv <- if(is.na(profile$rv100)) 0 else profile$rv100
      border_col <- if(rv >= 1.5) "#2E7D32" else if(rv >= 0.5) "#81C784" else if(rv >= -0.5) "#E0E0E0" else if(rv >= -1.5) "#FF8A65" else "#D32F2F"
      is_sel <- !is.null(sel) && sel == h_name
      div(class = paste("player-list-item", if(is_sel) "selected" else ""), style = paste0("border-color:", border_col, ";"),
          onclick = paste0("Shiny.setInputValue('select_hitter', '", h_name, "', {priority: 'event'});"),
          div(tags$span(class = "player-name", h_name), tags$span(style = "color:#666; margin-left:8px;", paste0("(", profile$hand, ")"))),
          tags$span(class = "player-grade", style = paste0("background:", grade_color_light(profile$overall_grade), ";"), profile$overall_grade))
    })
  })
        
  output$hitter_detail_panel <- renderUI({
    h_name <- selected_hitter()
    data <- scout_data()
    if (is.null(h_name) || is.null(data[[h_name]])) return(div(class = "chart-box", style = "text-align: center; padding: 60px;", h4("Select a hitter from the list", style = "color: #666;")))
    
    h_data <- data[[h_name]]
    profile <- h_data$profile
    hitter_id <- gsub("[^A-Za-z0-9]", "_", h_name)
    h_raw <- tm_data %>% filter(Batter == h_name)
    cs <- list(fp = calc_count_stats(h_raw %>% filter(count_cat == "First Pitch")),
               ahead = calc_count_stats(h_raw %>% filter(count_cat == "Ahead")),
               behind = calc_count_stats(h_raw %>% filter(count_cat == "Behind")),
               ts = calc_count_stats(h_raw %>% filter(two_strike == TRUE)),
               early = calc_count_stats(h_raw %>% filter(Strikes < 2)))
    
    # Calculate LHP/RHP splits
    rhp_profile <- calculate_hitter_profile(h_name, tm_data, gm, "Right")
    lhp_profile <- calculate_hitter_profile(h_name, tm_data, gm, "Left")
    
    # Last 15 games stats
    last15_dates <- get_last_n_games(tm_data, h_name, 15)
    last15_data <- h_raw %>% filter(Date %in% last15_dates)
    last15_stats <- calc_count_stats(last15_data)
    
    div(
      # Grade Legend at top
      div(class = "chart-box", style = "padding: 8px; margin-bottom: 10px;",
          div(style = "display: flex; justify-content: center; align-items: center; gap: 15px; font-size: 11px;",
              tags$span(style = "font-weight: bold; color: #006F71;", "Grade Scale:"),
              tags$span(style = "background: #EF5350; color: white; padding: 2px 8px; border-radius: 4px;", "20-29 Poor"),
              tags$span(style = "background: #FF8A65; color: white; padding: 2px 8px; border-radius: 4px;", "30-39 Below Avg"),
              tags$span(style = "background: #FFB74D; color: black; padding: 2px 8px; border-radius: 4px;", "40-44 Fringe"),
              tags$span(style = "background: #FFF176; color: black; padding: 2px 8px; border-radius: 4px;", "45-54 Average"),
              tags$span(style = "background: #AED581; color: black; padding: 2px 8px; border-radius: 4px;", "55-59 Above Avg"),
              tags$span(style = "background: #81C784; color: white; padding: 2px 8px; border-radius: 4px;", "60-69 Plus"),
              tags$span(style = "background: #66BB6A; color: white; padding: 2px 8px; border-radius: 4px;", "70-80 Elite"))),
      
      div(class = "chart-box",
          div(class = "hitter-header",
              div(class = "hitter-info", h3(h_name), div(class = "hitter-meta", paste0("Bats: ", profile$hand, " | PA: ", profile$n_pa, " | Pitches: ", profile$n))),
              div(class = "grade-row", style = "margin-left: auto;",
                  create_grade_box(profile$overall_grade, "OVR"),
                  create_grade_box(profile$raw_power_grade, "RAW"), create_grade_box(profile$contact_grade, "CON"),
                  create_grade_box(profile$avoid_k_grade, "AvK"), create_grade_box(profile$swing_dec_grade, "SwD")))),
      
      fluidRow(
        column(6,
               div(class = "detail-section", div(class = "section-header section-header-blue", "Production"),
                   div(class = "stat-grid", 
                       create_stat_item(profile$rv100, "RV/100", benchmarks$rv100, TRUE, "rv"),
                       create_stat_item(profile$woba, "wOBA", benchmarks$woba, TRUE, "woba"),
                       create_stat_item(profile$wobacon, "wOBACON", benchmarks$wobacon, TRUE, "woba"),
                       create_stat_item(profile$k_pct, "K%", benchmarks$k_pct, FALSE, "pct"), 
                       create_stat_item(profile$bb_pct, "BB%", benchmarks$bb_pct, TRUE, "pct"),
                       create_stat_item(profile$put_away_pct, "PutAway%", benchmarks$put_away_pct, FALSE, "pct"))),
               
               div(class = "detail-section", div(class = "section-header section-header-purple", "Plate Discipline"),
                   div(class = "stat-grid", create_stat_item(profile$whiff_pct, "Whiff%", benchmarks$whiff_pct, FALSE, "pct"),
                       create_stat_item(profile$chase_pct, "Chase%", benchmarks$chase_pct, FALSE, "pct"),
                       create_stat_item(profile$z_swing, "Z-Swing%", benchmarks$z_swing, TRUE, "pct"),
                       create_stat_item(profile$z_contact, "Z-Con%", benchmarks$z_contact, TRUE, "pct"),
                       create_stat_item(profile$o_swing, "O-Swing%", benchmarks$chase_pct, FALSE, "pct"),
                       create_stat_item(profile$o_contact, "O-Con%", 65, TRUE, "pct"))),
               
               div(class = "detail-section", div(class = "section-header section-header-green", "Batted Ball"),
                   div(class = "stat-grid", create_stat_item(profile$ev_mean, "Avg EV", benchmarks$ev_mean, TRUE, "decimal"),
                       create_stat_item(profile$ev90, "EV90", benchmarks$ev90, TRUE, "decimal"),
                       create_stat_item(profile$max_ev, "Max EV", 110, TRUE, "decimal"),
                       create_stat_item(profile$hard_hit_pct, "HH%", benchmarks$hard_hit_pct, TRUE, "pct"),
                       create_stat_item(profile$slg, "SLG", 0.450, TRUE, "woba"),
                       create_stat_item(profile$la_mean, "LA", 12, TRUE, "decimal")),
                   hr(),
                   div(class = "stat-grid",
                       create_stat_item(profile$gb_pct, "GB%", benchmarks$gb_pct, FALSE, "pct"),
                       create_stat_item(profile$ld_pct, "LD%", benchmarks$ld_pct, TRUE, "pct"),
                       create_stat_item(profile$fb_pct, "FB%", benchmarks$fb_pct, TRUE, "pct"))),
               
               div(class = "detail-section", div(class = "section-header section-header-orange", "vs Pitch Types"),
                   tags$table(class = "splits-table",
                              tags$thead(tags$tr(tags$th("Type"), tags$th("N"), tags$th("RV/100"), tags$th("wOBA"), tags$th("Whiff%"), tags$th("Chase%"), tags$th("HH%"))),
                              tags$tbody(
                                tags$tr(tags$td(class = "splits-label", "Fastballs"), tags$td(profile$fb_stats$n),
                                        tags$td(create_rv_pill(profile$fb_stats$rv100, benchmarks$rv100, TRUE)),
                                        tags$td(create_woba_pill(profile$fb_stats$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(profile$fb_stats$whiff, benchmarks$whiff_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$fb_stats$chase, benchmarks$chase_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$fb_stats$hh_pct, benchmarks$hard_hit_pct, TRUE))),
                                tags$tr(tags$td(class = "splits-label", "Breaking"), tags$td(profile$bb_stats$n),
                                        tags$td(create_rv_pill(profile$bb_stats$rv100, benchmarks$rv100, TRUE)),
                                        tags$td(create_woba_pill(profile$bb_stats$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(profile$bb_stats$whiff, benchmarks$whiff_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$bb_stats$chase, benchmarks$chase_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$bb_stats$hh_pct, benchmarks$hard_hit_pct, TRUE))),
                                tags$tr(tags$td(class = "splits-label", "Offspeed"), tags$td(profile$os_stats$n),
                                        tags$td(create_rv_pill(profile$os_stats$rv100, benchmarks$rv100, TRUE)),
                                        tags$td(create_woba_pill(profile$os_stats$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(profile$os_stats$whiff, benchmarks$whiff_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$os_stats$chase, benchmarks$chase_pct, FALSE)),
                                        tags$td(create_pct_pill(profile$os_stats$hh_pct, benchmarks$hard_hit_pct, TRUE))))))),
        
        column(6,
               div(class = "detail-section", div(class = "section-header section-header-red", "Situational"),
                   tags$table(class = "splits-table",
                              tags$thead(tags$tr(tags$th("Situation"), tags$th("N"), tags$th("RV/100"), tags$th("wOBA"), tags$th("Whiff%"), tags$th("Chase%"))),
                              tags$tbody(
                                tags$tr(tags$td(class = "splits-label", "First Pitch"), tags$td(cs$fp$n),
                                        tags$td(create_rv_pill(cs$fp$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(cs$fp$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(cs$fp$whiff, benchmarks$whiff_pct, FALSE)), 
                                        tags$td(create_pct_pill(cs$fp$chase, benchmarks$chase_pct, FALSE))),
                                tags$tr(tags$td(class = "splits-label", "0-1 Strikes"), tags$td(cs$early$n),
                                        tags$td(create_rv_pill(cs$early$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(cs$early$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(cs$early$whiff, benchmarks$whiff_pct, FALSE)), 
                                        tags$td(create_pct_pill(cs$early$chase, benchmarks$chase_pct, FALSE))),
                                tags$tr(tags$td(class = "splits-label", "2 Strikes"), tags$td(cs$ts$n),
                                        tags$td(create_rv_pill(cs$ts$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(cs$ts$woba, benchmarks$woba * 0.85, TRUE)),
                                        tags$td(create_pct_pill(cs$ts$whiff, benchmarks$whiff_pct * 1.2, FALSE)), 
                                        tags$td(create_pct_pill(cs$ts$chase, benchmarks$chase_pct * 1.3, FALSE))),
                                tags$tr(tags$td(class = "splits-label", "Ahead"), tags$td(cs$ahead$n),
                                        tags$td(create_rv_pill(cs$ahead$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(cs$ahead$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(cs$ahead$whiff, benchmarks$whiff_pct, FALSE)), 
                                        tags$td(create_pct_pill(cs$ahead$chase, benchmarks$chase_pct, FALSE))),
                                tags$tr(tags$td(class = "splits-label", "Behind"), tags$td(cs$behind$n),
                                        tags$td(create_rv_pill(cs$behind$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(cs$behind$woba, benchmarks$woba * 0.9, TRUE)),
                                        tags$td(create_pct_pill(cs$behind$whiff, benchmarks$whiff_pct * 1.1, FALSE)), 
                                        tags$td(create_pct_pill(cs$behind$chase, benchmarks$chase_pct * 1.2, FALSE))),
                                tags$tr(tags$td(class = "splits-label", "Last 15 Games"), tags$td(last15_stats$n),
                                        tags$td(create_rv_pill(last15_stats$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(last15_stats$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(last15_stats$whiff, benchmarks$whiff_pct, FALSE)), 
                                        tags$td(create_pct_pill(last15_stats$chase, benchmarks$chase_pct, FALSE)))))),
               
               # LHP/RHP Splits Section
               div(class = "detail-section", div(class = "section-header", "LHP / RHP Splits"),
                   tags$table(class = "splits-table",
                              tags$thead(tags$tr(tags$th("Split"), tags$th("N"), tags$th("RV/100"), tags$th("wOBA"), tags$th("Whiff%"), tags$th("Chase%"), tags$th("K%"))),
                              tags$tbody(
                                tags$tr(tags$td(class = "splits-label", "vs RHP"), 
                                        tags$td(if(!is.null(rhp_profile)) rhp_profile$n else "-"),
                                        tags$td(if(!is.null(rhp_profile)) create_rv_pill(rhp_profile$rv100, 0, TRUE) else "-"),
                                        tags$td(if(!is.null(rhp_profile)) create_woba_pill(rhp_profile$woba, benchmarks$woba, TRUE) else "-"),
                                        tags$td(if(!is.null(rhp_profile)) create_pct_pill(rhp_profile$whiff_pct, benchmarks$whiff_pct, FALSE) else "-"),
                                        tags$td(if(!is.null(rhp_profile)) create_pct_pill(rhp_profile$chase_pct, benchmarks$chase_pct, FALSE) else "-"),
                                        tags$td(if(!is.null(rhp_profile)) create_pct_pill(rhp_profile$k_pct, benchmarks$k_pct, FALSE) else "-")),
                                tags$tr(tags$td(class = "splits-label", "vs LHP"), 
                                        tags$td(if(!is.null(lhp_profile)) lhp_profile$n else "-"),
                                        tags$td(if(!is.null(lhp_profile)) create_rv_pill(lhp_profile$rv100, 0, TRUE) else "-"),
                                        tags$td(if(!is.null(lhp_profile)) create_woba_pill(lhp_profile$woba, benchmarks$woba, TRUE) else "-"),
                                        tags$td(if(!is.null(lhp_profile)) create_pct_pill(lhp_profile$whiff_pct, benchmarks$whiff_pct, FALSE) else "-"),
                                        tags$td(if(!is.null(lhp_profile)) create_pct_pill(lhp_profile$chase_pct, benchmarks$chase_pct, FALSE) else "-"),
                                        tags$td(if(!is.null(lhp_profile)) create_pct_pill(lhp_profile$k_pct, benchmarks$k_pct, FALSE) else "-"))))),
               
               div(class = "detail-section", div(class = "section-header section-header-teal", "Spray Tendency"),
                   div(class = "stat-grid", create_stat_item(profile$pull_pct, "Pull%", benchmarks$pull_pct, TRUE, "pct"),
                       create_stat_item(profile$middle_pct, "Mid%", 38, TRUE, "pct"), create_stat_item(profile$oppo_pct, "Oppo%", benchmarks$oppo_pct, TRUE, "pct"))),
               
               div(class = "detail-section", div(class = "section-header", "Spray Charts by Pitcher Hand"),
                   fluidRow(column(4, plotOutput(paste0("spray_all_", hitter_id), height = "130px")),
                            column(4, plotOutput(paste0("spray_rhp_", hitter_id), height = "130px")),
                            column(4, plotOutput(paste0("spray_lhp_", hitter_id), height = "130px"))),
                   div(class = "pitch-legend", span(class = "legend-item", span(class = "legend-dot", style = "background:#E41A1C;"), "HR"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#377EB8;"), "3B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#4DAF4A;"), "2B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#984EA3;"), "1B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:gray;"), "Out"))),
               div(class = "detail-section", div(class = "section-header", "Spray Charts by Count"),
                   fluidRow(column(6, plotOutput(paste0("spray_early_", hitter_id), height = "130px")),
                            column(6, plotOutput(paste0("spray_2k_", hitter_id), height = "130px"))),
                   div(class = "pitch-legend", span(class = "legend-item", span(class = "legend-dot", style = "background:#E41A1C;"), "HR"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#377EB8;"), "3B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#4DAF4A;"), "2B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#984EA3;"), "1B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:gray;"), "Out"))))),
      
      # Whiff Zones by Pitch Type and Pitcher Hand - Symmetrical Layout
      fluidRow(
        column(12,
               div(class = "detail-section", div(class = "section-header", "Whiff Zones by Pitch Type & Pitcher Hand"),
                   # Header row
                   fluidRow(
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Fastballs")),
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Breaking")),
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Offspeed"))),
                   # vs RHP row
                   fluidRow(
                     column(4, plotOutput(paste0("hm_whiff_fb_rhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_bb_rhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_os_rhp_", hitter_id), height = "100px"))),
                   # vs LHP row
                   fluidRow(
                     column(4, plotOutput(paste0("hm_whiff_fb_lhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_bb_lhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_os_lhp_", hitter_id), height = "100px")))))),
      
      fluidRow(
        column(6,
               div(class = "detail-section", div(class = "section-header", "Damage Zones by Pitcher Hand"),
                   fluidRow(column(6, plotOutput(paste0("hm_damage_rhp_", hitter_id), height = "120px")),
                            column(6, plotOutput(paste0("hm_damage_lhp_", hitter_id), height = "120px"))),
                   div(style = "margin-top: 8px; border-top: 1px solid #e0e0e0; padding-top: 8px;",
                       div(style = "text-align: center; font-weight: bold; font-size: 11px; margin-bottom: 5px; color: #006F71;", "Damage on Fastballs"),
                       fluidRow(column(6, plotOutput(paste0("hm_damage_fb_rhp_", hitter_id), height = "100px")),
                                column(6, plotOutput(paste0("hm_damage_fb_lhp_", hitter_id), height = "100px")))))),
        column(6,
               div(class = "detail-section", div(class = "section-header", "Hits/Outs by Pitcher Hand"),
                   fluidRow(column(3, plotOutput(paste0("ho_rhp_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_rhp_outs_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_lhp_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_lhp_outs_", hitter_id), height = "100px")))),
               div(class = "detail-section", div(class = "section-header", "Hits/Outs by Count"),
                   fluidRow(column(3, plotOutput(paste0("ho_early_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_early_outs_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_2k_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_2k_outs_", hitter_id), height = "100px")))))),
      
      # Percentile Charts - SEC and Overall D1 side by side
      fluidRow(
        column(12,
               div(class = "detail-section", div(class = "section-header section-header-purple", "Percentile Rankings"),
                   fluidRow(
                     column(6, plotOutput(paste0("pct_sec_", hitter_id), height = "320px")),
                     column(6, plotOutput(paste0("pct_d1_", hitter_id), height = "320px")))))),
      
      # ============================================================================
      # ADVANCED HEAT MAPS SECTION
      # ============================================================================
      fluidRow(
        column(12,
               div(class = "detail-section", 
                   div(class = "section-header section-header-orange", "Advanced Heat Maps"),
                   
                   # Toggle to show/hide advanced heat maps
                   checkboxInput(paste0("show_adv_heatmaps_", hitter_id), "Show Advanced Heat Maps", value = FALSE),
                   
                   conditionalPanel(
                     condition = paste0("input.show_adv_heatmaps_", hitter_id, " == true"),
                     
                     # Tab panel for the 4 sheets
                     tabsetPanel(id = paste0("adv_heatmap_tabs_", hitter_id), type = "pills",
                       
                       # Sheet 1: By Pitch Type vs RHP
                       tabPanel("Pitch Type vs RHP",
                         div(style = "margin-top: 15px;",
                             div(style = "font-weight: bold; margin-bottom: 10px; text-align: center;", "Performance by Pitch Type vs RHP"),
                             uiOutput(paste0("adv_hm_pitch_rhp_", hitter_id))
                         )
                       ),
                       
                       # Sheet 2: By Pitch Type vs LHP
                       tabPanel("Pitch Type vs LHP",
                         div(style = "margin-top: 15px;",
                             div(style = "font-weight: bold; margin-bottom: 10px; text-align: center;", "Performance by Pitch Type vs LHP"),
                             uiOutput(paste0("adv_hm_pitch_lhp_", hitter_id))
                         )
                       ),
                       
                       # Sheet 3: By Count Type vs RHP
                       tabPanel("Count Type vs RHP",
                         div(style = "margin-top: 15px;",
                             div(style = "font-weight: bold; margin-bottom: 10px; text-align: center;", "Performance by Count vs RHP"),
                             uiOutput(paste0("adv_hm_count_rhp_", hitter_id))
                         )
                       ),
                       
                       # Sheet 4: By Count Type vs LHP
                       tabPanel("Count Type vs LHP",
                         div(style = "margin-top: 15px;",
                             div(style = "font-weight: bold; margin-bottom: 10px; text-align: center;", "Performance by Count vs LHP"),
                             uiOutput(paste0("adv_hm_count_lhp_", hitter_id))
                         )
                       )
                     )
                   )
               )
        )
      ),
      
      # ============================================================================
      # TREND CHARTS SECTION
      # ============================================================================
      fluidRow(
        column(12,
               div(class = "detail-section", 
                   div(class = "section-header section-header-blue", "Performance Trends"),
                   
                   # Controls for trend chart
                   fluidRow(
                     column(3,
                       dateRangeInput(paste0("trend_dates_", hitter_id), "Date Range:",
                                      start = Sys.Date() - 180, end = Sys.Date(),
                                      min = "2024-01-01", max = Sys.Date())
                     ),
                     column(6,
                       checkboxGroupInput(paste0("trend_stats_", hitter_id), "Select Stats:",
                                          choices = c("Swing%" = "swing_pct", "wOBA" = "woba", 
                                                     "Chase%" = "chase_pct", "Whiff%" = "whiff_pct",
                                                     "IZ Whiff%" = "iz_whiff_pct", "IZ Damage%" = "iz_damage_pct"),
                                          selected = c("swing_pct", "woba", "whiff_pct", "chase_pct"),
                                          inline = TRUE)
                     ),
                     column(3,
                       div(style = "margin-top: 25px;",
                         actionButton(paste0("select_all_stats_", hitter_id), "Select All", 
                                     class = "btn btn-sm", style = "margin-right: 5px;"),
                         actionButton(paste0("deselect_all_stats_", hitter_id), "Deselect All", 
                                     class = "btn btn-sm")
                       )
                     )
                   ),
                   
                   # Trend chart output
                   plotOutput(paste0("trend_chart_", hitter_id), height = "350px")
               )
        )
      ),
      
      div(class = "detail-section", div(class = "section-header", "Scouting Notes"),
          fluidRow(
            column(4, 
                   tags$label(style = "color: #C62828; font-weight: bold; font-size: 14px;", "vs LHP Plan:"),
                   textAreaInput(paste0("lhp_plan_", hitter_id), NULL, value = h_data$lhp_plan, rows = 3, placeholder = "Attack plan vs lefties")),
            column(4, 
                   tags$label(style = "color: black; font-weight: bold; font-size: 14px;", "vs RHP Plan:"),
                   textAreaInput(paste0("rhp_plan_", hitter_id), NULL, value = h_data$rhp_plan, rows = 3, placeholder = "Attack plan vs righties")),
            column(4, 
                   tags$label(style = "color: #1565C0; font-weight: bold; font-size: 14px;", "Overall Notes:"),
                   textAreaInput(paste0("notes_", hitter_id), NULL, value = h_data$overall_notes, rows = 3, placeholder = "General observations"))),
          # Save button to avoid constant updates
          actionButton(paste0("save_notes_", hitter_id), "Save Notes", class = "btn-bronze", style = "margin-top: 10px;"))
    )
  })
  
  observe({
    h_name <- selected_hitter()
    if (is.null(h_name)) return()
    hitter_id <- gsub("[^A-Za-z0-9]", "_", h_name)
    h_raw <- tm_data %>% filter(Batter == h_name)
    
    # Spray charts by pitcher hand
    output[[paste0("spray_all_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "Overall", "all") }, bg = "transparent")
    output[[paste0("spray_rhp_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "vs RHP", "rhp") }, bg = "transparent")
    output[[paste0("spray_lhp_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw %>% filter(PitcherThrows == "Left"), "vs LHP", "all") }, bg = "transparent")
    
    # Spray charts by count (0-1 strikes and 2 strikes)
    output[[paste0("spray_early_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "0-1 Strikes", "early") }, bg = "transparent")
    output[[paste0("spray_2k_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "2 Strikes", "2k") }, bg = "transparent")
    
    # Whiff zones by pitch family AND pitcher hand (6 plots total)
    output[[paste0("hm_whiff_fb_rhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "FB vs RHP", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "FB", "Right") 
    }, bg = "transparent")
    output[[paste0("hm_whiff_bb_rhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "BB vs RHP", "whiff", c("white", "#E1BEE7", "#9C27B0", "#4A148C"), "BB", "Right") 
    }, bg = "transparent")
    output[[paste0("hm_whiff_os_rhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "OS vs RHP", "whiff", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "OS", "Right") 
    }, bg = "transparent")
    output[[paste0("hm_whiff_fb_lhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "FB vs LHP", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "FB", "Left") 
    }, bg = "transparent")
    output[[paste0("hm_whiff_bb_lhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "BB vs LHP", "whiff", c("white", "#E1BEE7", "#9C27B0", "#4A148C"), "BB", "Left") 
    }, bg = "transparent")
    output[[paste0("hm_whiff_os_lhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "OS vs LHP", "whiff", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "OS", "Left") 
    }, bg = "transparent")
    
    # Damage zones by pitcher hand
    output[[paste0("hm_damage_rhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "Damage vs RHP", "damage", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "All", "Right") 
    }, bg = "transparent")
    output[[paste0("hm_damage_lhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "Damage vs LHP", "damage", c("white", "#BBDEFB", "#2196F3", "#0D47A1"), "All", "Left") 
    }, bg = "transparent")
    
    # Damage zones on fastballs by pitcher hand
    output[[paste0("hm_damage_fb_rhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "FB vs RHP", "damage", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "FB", "Right") 
    }, bg = "transparent")
    output[[paste0("hm_damage_fb_lhp_", hitter_id)]] <- renderPlot({ 
      create_heatmap(h_raw, "FB vs LHP", "damage", c("white", "#FFE0B2", "#FF9800", "#E65100"), "FB", "Left") 
    }, bg = "transparent")
    
    # Hit/Out charts by hand
    output[[paste0("ho_rhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "rhp_hits") }, bg = "transparent")
    output[[paste0("ho_rhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "rhp_outs") }, bg = "transparent")
    output[[paste0("ho_lhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "lhp_hits") }, bg = "transparent")
    output[[paste0("ho_lhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "lhp_outs") }, bg = "transparent")
    
    # Hit/Out charts by count (0-1 strikes and 2 strikes)
    output[[paste0("ho_early_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "early_hits") }, bg = "transparent")
    output[[paste0("ho_early_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "early_outs") }, bg = "transparent")
    output[[paste0("ho_2k_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "2k_hits") }, bg = "transparent")
    output[[paste0("ho_2k_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "2k_outs") }, bg = "transparent")
    
    # SEC Percentile Chart
    output[[paste0("pct_sec_", hitter_id)]] <- renderPlot({
      create_sec_percentile_chart(h_raw, sec_pool_data, h_name, "SEC")
    }, bg = "transparent")
    
    # D1 Overall Percentile Chart
    output[[paste0("pct_d1_", hitter_id)]] <- renderPlot({
      create_sec_percentile_chart(h_raw, d1_pool_data, h_name, "Overall D1")
    }, bg = "transparent")
    
    # ============================================================================
    # ADVANCED HEAT MAPS RENDERING
    # ============================================================================
    
    # Helper function to create advanced heat map stats table
    create_adv_heatmap_table <- function(stats_list, split_col_name, h_raw, pitcher_hand) {
      stat_cols <- c("Pitch Freq", "Swing%", "wOBA", "Damage (95+)", "Chase%", "Whiff%", "IZ Whiff%", "IZ Damage%")
      
      # Create header row
      header_row <- tags$tr(
        tags$th(style = "padding: 8px; background: #006F71; color: white; text-align: left;", split_col_name),
        lapply(stat_cols, function(col) {
          tags$th(style = "padding: 8px; background: #006F71; color: white; text-align: center; font-size: 11px;", col)
        })
      )
      
      # Create data rows
      data_rows <- lapply(stats_list, function(row_data) {
        row_name <- if (!is.null(row_data$pitch_type)) row_data$pitch_type else row_data$count_type
        
        # Format stat values
        format_stat <- function(val, is_pct = TRUE, is_woba = FALSE) {
          if (is.na(val) || is.null(val)) return("-")
          if (is_woba) return(sprintf("%.3f", val))
          if (is_pct) return(sprintf("%.1f%%", val))
          return(sprintf("%.0f", val))
        }
        
        # Color coding helper
        get_cell_style <- function(val, benchmark, higher_better = TRUE) {
          if (is.na(val)) return("background: #f5f5f5; color: #666;")
          if (benchmark == 0) {
            bg_col <- if (higher_better) {
              if (val > 0.5) "#C8E6C9" else if (val > 0) "#DCEDC8" else if (val >= -0.5) "#FFF9C4" else "#FFCDD2"
            } else {
              if (val < -0.5) "#C8E6C9" else if (val < 0) "#DCEDC8" else if (val <= 0.5) "#FFF9C4" else "#FFCDD2"
            }
          } else {
            pct_diff <- if (higher_better) (val - benchmark) / abs(benchmark) * 100 else (benchmark - val) / abs(benchmark) * 100
            bg_col <- if (pct_diff > 10) "#C8E6C9" else if (pct_diff > 0) "#DCEDC8" else if (pct_diff >= -10) "#FFF9C4" else "#FFCDD2"
          }
          paste0("background: ", bg_col, "; padding: 6px; text-align: center; font-size: 11px;")
        }
        
        tags$tr(
          tags$td(style = "padding: 8px; font-weight: bold; background: #f5f5f5;", row_name),
          tags$td(style = "padding: 6px; text-align: center; font-size: 11px;", format_stat(row_data$pitch_freq, is_pct = FALSE)),
          tags$td(style = get_cell_style(row_data$swing_pct, 48, TRUE), format_stat(row_data$swing_pct)),
          tags$td(style = get_cell_style(row_data$woba, 0.320, TRUE), format_stat(row_data$woba, is_woba = TRUE)),
          tags$td(style = get_cell_style(row_data$damage_pct, 35, TRUE), format_stat(row_data$damage_pct)),
          tags$td(style = get_cell_style(row_data$chase_pct, 28, FALSE), format_stat(row_data$chase_pct)),
          tags$td(style = get_cell_style(row_data$whiff_pct, 25, FALSE), format_stat(row_data$whiff_pct)),
          tags$td(style = get_cell_style(row_data$iz_whiff_pct, 15, FALSE), format_stat(row_data$iz_whiff_pct)),
          tags$td(style = get_cell_style(row_data$iz_damage_pct, 35, TRUE), format_stat(row_data$iz_damage_pct))
        )
      })
      
      # Combine table
      tags$table(class = "splits-table", style = "width: 100%; border-collapse: collapse;",
        tags$thead(header_row),
        tags$tbody(data_rows)
      )
    }
    
    # Advanced Heat Map: Pitch Type vs RHP
    output[[paste0("adv_hm_pitch_rhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Right", "pitch_type")
      if (length(stats) == 0) return(div("No data available"))
      
      # Create table with heat maps
      div(
        create_adv_heatmap_table(stats, "Pitch Type", h_raw, "Right"),
        div(style = "margin-top: 15px;",
            div(style = "font-weight: bold; margin-bottom: 8px;", "Heat Map Zones by Pitch Type"),
            fluidRow(
              lapply(c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl"), function(pt) {
                pt_data <- h_raw %>% filter(PitcherThrows == "Right") %>%
                  mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType)) %>%
                  filter(DetailedPitchType == pt)
                pt_stats <- stats[sapply(stats, function(x) x$pitch_type == pt)]
                whiff_pct <- if (length(pt_stats) > 0) pt_stats[[1]]$whiff_pct else NA
                n_pitches <- if (length(pt_stats) > 0) pt_stats[[1]]$n else 0
                
                column(2, 
                       plotOutput(paste0("adv_hm_pt_rhp_", gsub("/", "_", pt), "_", hitter_id), height = "90px"),
                       div(style = "text-align: center; font-size: 10px; margin-top: -5px;", pt)
                )
              }),
              column(2, div())  # Empty column for alignment
            )
        )
      )
    })
    
    # Advanced Heat Map: Pitch Type vs LHP
    output[[paste0("adv_hm_pitch_lhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Left", "pitch_type")
      if (length(stats) == 0) return(div("No data available"))
      
      div(
        create_adv_heatmap_table(stats, "Pitch Type", h_raw, "Left"),
        div(style = "margin-top: 15px;",
            div(style = "font-weight: bold; margin-bottom: 8px;", "Heat Map Zones by Pitch Type"),
            fluidRow(
              lapply(c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl"), function(pt) {
                column(2, 
                       plotOutput(paste0("adv_hm_pt_lhp_", gsub("/", "_", pt), "_", hitter_id), height = "90px"),
                       div(style = "text-align: center; font-size: 10px; margin-top: -5px;", pt)
                )
              }),
              column(2, div())
            )
        )
      )
    })
    
    # Advanced Heat Map: Count Type vs RHP
    output[[paste0("adv_hm_count_rhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Right", "count_type")
      if (length(stats) == 0) return(div("No data available"))
      
      div(
        create_adv_heatmap_table(stats, "Count", h_raw, "Right"),
        div(style = "margin-top: 15px;",
            div(style = "font-weight: bold; margin-bottom: 8px;", "Heat Map Zones by Count"),
            fluidRow(
              lapply(c("1P", "2K", "Ahead", "Behind", "Last 15"), function(ct) {
                column(2, 
                       plotOutput(paste0("adv_hm_ct_rhp_", gsub(" ", "_", ct), "_", hitter_id), height = "90px"),
                       div(style = "text-align: center; font-size: 10px; margin-top: -5px;", ct)
                )
              }),
              column(2, div())
            )
        )
      )
    })
    
    # Advanced Heat Map: Count Type vs LHP
    output[[paste0("adv_hm_count_lhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Left", "count_type")
      if (length(stats) == 0) return(div("No data available"))
      
      div(
        create_adv_heatmap_table(stats, "Count", h_raw, "Left"),
        div(style = "margin-top: 15px;",
            div(style = "font-weight: bold; margin-bottom: 8px;", "Heat Map Zones by Count"),
            fluidRow(
              lapply(c("1P", "2K", "Ahead", "Behind", "Last 15"), function(ct) {
                column(2, 
                       plotOutput(paste0("adv_hm_ct_lhp_", gsub(" ", "_", ct), "_", hitter_id), height = "90px"),
                       div(style = "text-align: center; font-size: 10px; margin-top: -5px;", ct)
                )
              }),
              column(2, div())
            )
        )
      )
    })
    
    # Render individual heat map plots for pitch types
    for (pt in c("4S", "2S_Si", "SL_SW", "CB", "CH_Spl")) {
      local({
        pitch_type <- gsub("_", "/", pt)
        pt_safe <- pt
        
        # vs RHP
        output[[paste0("adv_hm_pt_rhp_", pt_safe, "_", hitter_id)]] <- renderPlot({
          pt_data <- h_raw %>% 
            filter(PitcherThrows == "Right") %>%
            mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType)) %>%
            filter(DetailedPitchType == pitch_type)
          create_heatmap(pt_data, "", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "All", "All")
        }, bg = "transparent")
        
        # vs LHP
        output[[paste0("adv_hm_pt_lhp_", pt_safe, "_", hitter_id)]] <- renderPlot({
          pt_data <- h_raw %>% 
            filter(PitcherThrows == "Left") %>%
            mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType)) %>%
            filter(DetailedPitchType == pitch_type)
          create_heatmap(pt_data, "", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "All", "All")
        }, bg = "transparent")
      })
    }
    
    # Render individual heat map plots for count types
    for (ct in c("1P", "2K", "Ahead", "Behind", "Last_15")) {
      local({
        count_type <- gsub("_", " ", ct)
        ct_safe <- ct
        
        # vs RHP
        output[[paste0("adv_hm_ct_rhp_", ct_safe, "_", hitter_id)]] <- renderPlot({
          if (ct_safe == "Last_15") {
            last15_dates <- get_last_n_games(tm_data, h_name, 15)
            ct_data <- h_raw %>% filter(PitcherThrows == "Right", Date %in% last15_dates)
          } else {
            ct_data <- h_raw %>% 
              filter(PitcherThrows == "Right") %>%
              mutate(
                CountType = case_when(
                  Balls == 0 & Strikes == 0 ~ "1P",
                  Strikes == 2 ~ "2K",
                  Strikes > Balls ~ "Ahead",
                  Balls > Strikes ~ "Behind",
                  TRUE ~ "Even"
                )
              ) %>%
              filter(CountType == count_type)
          }
          create_heatmap(ct_data, "", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "All", "All")
        }, bg = "transparent")
        
        # vs LHP
        output[[paste0("adv_hm_ct_lhp_", ct_safe, "_", hitter_id)]] <- renderPlot({
          if (ct_safe == "Last_15") {
            last15_dates <- get_last_n_games(tm_data, h_name, 15)
            ct_data <- h_raw %>% filter(PitcherThrows == "Left", Date %in% last15_dates)
          } else {
            ct_data <- h_raw %>% 
              filter(PitcherThrows == "Left") %>%
              mutate(
                CountType = case_when(
                  Balls == 0 & Strikes == 0 ~ "1P",
                  Strikes == 2 ~ "2K",
                  Strikes > Balls ~ "Ahead",
                  Balls > Strikes ~ "Behind",
                  TRUE ~ "Even"
                )
              ) %>%
              filter(CountType == count_type)
          }
          create_heatmap(ct_data, "", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "All", "All")
        }, bg = "transparent")
      })
    }
    
    # ============================================================================
    # TREND CHART RENDERING
    # ============================================================================
    
    # Update date range based on batter's data
    date_range <- get_batter_date_range(h_name, tm_data)
    updateDateRangeInput(session, paste0("trend_dates_", hitter_id),
                         start = max(date_range$min, Sys.Date() - 180),
                         end = date_range$max,
                         min = date_range$min,
                         max = date_range$max)
    
    # Trend chart output
    output[[paste0("trend_chart_", hitter_id)]] <- renderPlot({
      selected_stats <- input[[paste0("trend_stats_", hitter_id)]]
      date_range <- input[[paste0("trend_dates_", hitter_id)]]
      
      if (is.null(selected_stats) || length(selected_stats) == 0) {
        return(ggplot() + theme_void() + 
                 annotate("text", x = 0.5, y = 0.5, label = "Select at least one stat to display", 
                         size = 4, color = "gray50"))
      }
      
      start_date <- if (!is.null(date_range[1])) date_range[1] else NULL
      end_date <- if (!is.null(date_range[2])) date_range[2] else NULL
      
      create_trend_chart(h_name, tm_data, selected_stats, start_date, end_date, rolling_games = 5)
    }, bg = "transparent")
    
    # Select/Deselect all stats buttons
    observeEvent(input[[paste0("select_all_stats_", hitter_id)]], {
      updateCheckboxGroupInput(session, paste0("trend_stats_", hitter_id),
                              selected = c("swing_pct", "woba", "chase_pct", "whiff_pct", "iz_whiff_pct", "iz_damage_pct"))
    }, ignoreInit = TRUE)
    
    observeEvent(input[[paste0("deselect_all_stats_", hitter_id)]], {
      updateCheckboxGroupInput(session, paste0("trend_stats_", hitter_id), selected = character(0))
    }, ignoreInit = TRUE)
  })
  
  # Optimized note saving - only triggers on save button click
  observe({
    h_name <- selected_hitter()
    if (is.null(h_name)) return()
    hitter_id <- gsub("[^A-Za-z0-9]", "_", h_name)
    
    # Listen for save button click
    observeEvent(input[[paste0("save_notes_", hitter_id)]], {
      data <- scout_data()
      if (!is.null(data[[h_name]])) {
        if (!is.null(input[[paste0("lhp_plan_", hitter_id)]])) data[[h_name]]$lhp_plan <- input[[paste0("lhp_plan_", hitter_id)]]
        if (!is.null(input[[paste0("rhp_plan_", hitter_id)]])) data[[h_name]]$rhp_plan <- input[[paste0("rhp_plan_", hitter_id)]]
        if (!is.null(input[[paste0("notes_", hitter_id)]])) data[[h_name]]$overall_notes <- input[[paste0("notes_", hitter_id)]]
        scout_data(data)
        showNotification(paste("Notes saved for", h_name), type = "message", duration = 2)
      }
    }, ignoreInit = TRUE)
  })
  
output$download_scout_pdf <- downloadHandler(
    filename = function() { paste0("scouting_report_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) {
      data <- scout_data()
      if (length(data) == 0) { showNotification("No hitters selected", type = "warning"); return() }
      
      # Get custom title if provided
      pdf_title <- if (!is.null(input$pdf_custom_title) && nchar(trimws(input$pdf_custom_title)) > 0) {
        trimws(input$pdf_custom_title)
      } else {
        "HITTER SCOUTING REPORT"
      }
      
      # Portrait mode - narrow but long (8.5 x 11 inches)
      pdf(file, width = 8.5, height = 11)
      
      # ============================================================
      # LAYOUT CONFIGURATION - ALL NUMERIC VALUES HERE
      # ============================================================
      
      # Page layout
      PAGE_MARGIN_LEFT <- 0.02
      PAGE_MARGIN_RIGHT <- 0.98
      PAGE_WIDTH <- PAGE_MARGIN_RIGHT - PAGE_MARGIN_LEFT  # 0.96
      
      # Header settings
      HEADER_Y <- 0.975
      HEADER_HEIGHT <- 0.045
      HEADER_FONT_SIZE <- 14
      HEADER_DATE_FONT_SIZE <- 8
      
      # Hitter row configuration
      MAX_HITTERS_PER_PAGE <- 7
      HITTER_ROW_HEIGHT <- 0.125
      
      # Y positions for hitter rows (top of each row, starting from first hitter)
      # Row 1 starts at 0.94, each subsequent row is 0.125 lower
      HITTER_ROW_TOPS <- c(
        0.940,  # Hitter 1
        0.815,  # Hitter 2
        0.690,  # Hitter 3
        0.565,  # Hitter 4
        0.440,  # Hitter 5
        0.315,  # Hitter 6
        0.190   # Hitter 7
      )
      
      # Within each hitter row - offsets from row top
      ROW1_OFFSET <- 0.012      # Name/stats row
      ROW2_OFFSET <- 0.044      # Notes row
      ROW3_OFFSET <- 0.095      # Diamonds row
      
      # Row 1 (Name, Hand, Grades, Stats) settings
      ROW1_HEIGHT <- 0.024
      ROW1_BG_EVEN <- "#e8f5f4"
      ROW1_BG_ODD <- "#f0f0f0"
      
      # Name and hand positioning
      NAME_X <- 0.02
      NAME_FONT_SIZE <- 8
      HAND_X <- 0.12
      HAND_FONT_SIZE <- 7
      
      # Grade boxes positioning
      GRADE_START_X <- 0.18
      GRADE_SPACING <- 0.04
      GRADE_BOX_WIDTH <- 0.026
      GRADE_BOX_HEIGHT <- 0.018
      GRADE_FONT_SIZE <- 5.5
      # Grade X positions: 0.18, 0.22, 0.26, 0.30, 0.34
      
      # Stats pills positioning
      STAT_START_X <- 0.45
      STAT_SPACING <- 0.072
      # Stat X positions: 0.45, 0.522, 0.594, 0.666, 0.738, 0.810
      
      # Spray chart pills
      SPRAY_START_X <- 0.888
      SPRAY_SPACING <- 0.042
      SPRAY_PILL_WIDTH <- 0.038
      # Spray X positions: 0.888, 0.930, 0.972
      
      # Row 2 (Notes) settings
      ROW2_HEIGHT <- 0.032
      ROW2_BG <- "white"
      NOTES_FONT_SIZE <- 6
      LHP_COLOR <- "#C62828"
      RHP_COLOR <- "black"
      OVERALL_COLOR <- "#1565C0"
      LHP_X <- 0.025
      RHP_X <- 0.345
      OVERALL_X <- 0.665
      
      # Row 3 (Diamonds) settings
      DIAMOND_SIZE <- 0.014
      DIAMOND_SPACING <- 0.088
      DIAMOND_START_X <- 0.06
      # Diamond X positions: 0.06, 0.148, 0.236, 0.324, 0.412
      
      # In-game notes box
      NOTES_BOX_X <- 0.50
      NOTES_BOX_WIDTH <- 0.46
      NOTES_BOX_HEIGHT <- 0.048  # diamond_size * 3.4 ≈ 0.048
      
      # Footer
      FOOTER_Y <- 0.015
      FOOTER_FONT_SIZE <- 6
      
      # Separator line offset from row top
      SEPARATOR_OFFSET <- 0.117  # hitter_row_height - 0.008
      
      # ============================================================
      # HELPER FUNCTIONS
      # ============================================================
      
      # Helper function to draw a baseball diamond
      draw_diamond <- function(cx, cy, size) {
        x_pts <- c(cx, cx + size, cx, cx - size, cx)
        y_pts <- c(cy + size, cy, cy - size, cy, cy + size)
        grid::grid.polygon(x = x_pts, y = y_pts, default.units = "npc",
                          gp = grid::gpar(fill = "#f5f5dc", col = "black", lwd = 0.5))
        base_size <- size * 0.12
        grid::grid.rect(x = cx, y = cy - size + base_size, width = base_size * 1.2, height = base_size * 0.8,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        grid::grid.rect(x = cx + size - base_size, y = cy, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        grid::grid.rect(x = cx, y = cy + size - base_size, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        grid::grid.rect(x = cx - size + base_size, y = cy, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
      }
      
      # Helper function to get pill color for stats
      get_stat_pill_color <- function(val, benchmark, higher_better = TRUE, threshold_pct = 10) {
        if (is.na(val) || is.na(benchmark)) return(list(bg = "#E0E0E0", text = "#666666"))
        if (benchmark == 0) {
          if (val > 0.5) return(list(bg = "#C8E6C9", text = "#2E7D32"))
          if (val > 0) return(list(bg = "#DCEDC8", text = "#558B2F"))
          if (val >= -0.5) return(list(bg = "#FFF9C4", text = "#F57F17"))
          return(list(bg = "#FFCDD2", text = "#C62828"))
        }
        pct_diff <- if(higher_better) (val - benchmark) / abs(benchmark) * 100 else (benchmark - val) / abs(benchmark) * 100
        if (pct_diff > threshold_pct) return(list(bg = "#C8E6C9", text = "#2E7D32"))
        if (pct_diff > 0) return(list(bg = "#DCEDC8", text = "#558B2F"))
        if (pct_diff >= -threshold_pct) return(list(bg = "#FFF9C4", text = "#F57F17"))
        return(list(bg = "#FFCDD2", text = "#C62828"))
      }
      
      # Helper function to draw a colored stat pill
      draw_stat_pill <- function(x, y, label, value, benchmark, higher_better = TRUE, width = 0.055) {
        colors <- get_stat_pill_color(value, benchmark, higher_better)
        grid::grid.roundrect(x = x, y = y, width = width, height = 0.014,
                            r = unit(0.5, "mm"), default.units = "npc",
                            gp = grid::gpar(fill = colors$bg, col = NA))
        grid::grid.text(label, x = x, y = y, 
                       gp = grid::gpar(fontsize = 5, fontface = "bold", col = colors$text))
      }
      
      # Helper function to get hand label color
      get_hand_color <- function(hand) {
        if (hand == "Left") return("#C62828")
        if (hand == "Switch") return("#1565C0")
        return("black")
      }
      
      # ============================================================
      # PDF GENERATION
      # ============================================================
      
      n_hitters <- length(data)
      n_pages <- ceiling(n_hitters / MAX_HITTERS_PER_PAGE)
      
      for (page in 1:n_pages) {
        if (page > 1) grid::grid.newpage()
        
        # === PAGE HEADER ===
        grid::grid.rect(x = 0.5, y = HEADER_Y, width = 1, height = HEADER_HEIGHT, 
                       gp = grid::gpar(fill = "#006F71", col = NA))
        grid::grid.text(pdf_title, x = 0.5, y = HEADER_Y, 
                       gp = grid::gpar(fontsize = HEADER_FONT_SIZE, fontface = "bold", col = "white"))
        grid::grid.text(format(Sys.Date(), "%m/%d/%Y"), x = 0.95, y = HEADER_Y, just = "right",
                       gp = grid::gpar(fontsize = HEADER_DATE_FONT_SIZE, col = "white"))
        grid::grid.text(paste0("Page ", page, " of ", n_pages), x = 0.05, y = HEADER_Y, just = "left",
                       gp = grid::gpar(fontsize = HEADER_DATE_FONT_SIZE, col = "white"))
        
        # === GET HITTERS FOR THIS PAGE ===
        start_idx <- (page - 1) * MAX_HITTERS_PER_PAGE + 1
        end_idx <- min(page * MAX_HITTERS_PER_PAGE, n_hitters)
        page_hitters <- names(data)[start_idx:end_idx]
        
        # === DRAW EACH HITTER ROW ===
        for (h_idx in seq_along(page_hitters)) {
          h_name <- page_hitters[h_idx]
          profile <- data[[h_name]]$profile
          notes <- data[[h_name]]
          
          # Get row top Y position from lookup table
          row_top <- HITTER_ROW_TOPS[h_idx]
          
          # Calculate Y positions for each row within this hitter's section
          row1_y <- row_top - ROW1_OFFSET      # e.g., 0.940 - 0.012 = 0.928
          row2_y <- row_top - ROW2_OFFSET      # e.g., 0.940 - 0.044 = 0.896
          row3_y <- row_top - ROW3_OFFSET      # e.g., 0.940 - 0.095 = 0.845
          
          # === ROW 1: NAME, HAND, GRADES, STATS ===
          
          # Background bar
          row1_bg <- if(h_idx %% 2 == 0) ROW1_BG_EVEN else ROW1_BG_ODD
          grid::grid.rect(x = 0.5, y = row1_y, width = PAGE_WIDTH, height = ROW1_HEIGHT,
                         gp = grid::gpar(fill = row1_bg, col = NA))
          
          # Name
          grid::grid.text(h_name, x = NAME_X, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = NAME_FONT_SIZE, fontface = "bold"))
          
          # Hand label
          hand_color <- get_hand_color(profile$hand)
          grid::grid.text(paste0("(", profile$hand, ")"), x = HAND_X, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = HAND_FONT_SIZE, col = hand_color, fontface = "bold"))
          
          # Grade boxes - explicit X positions
          grades <- c(profile$overall_grade, profile$raw_power_grade, profile$contact_grade, 
                     profile$avoid_k_grade, profile$swing_dec_grade)
          grade_labels <- c("OVR", "RAW", "CON", "AvK", "SwD")
          grade_x_positions <- c(0.18, 0.22, 0.26, 0.30, 0.34)
          
          for (g_idx in 1:5) {
            grid::grid.rect(x = grade_x_positions[g_idx], y = row1_y, 
                           width = GRADE_BOX_WIDTH, height = GRADE_BOX_HEIGHT,
                           gp = grid::gpar(fill = grade_color_light(grades[g_idx]), col = "gray50", lwd = 0.3))
            grid::grid.text(grades[g_idx], x = grade_x_positions[g_idx], y = row1_y, 
                           gp = grid::gpar(fontsize = GRADE_FONT_SIZE, fontface = "bold"))
          }
          
          # Stats pills - explicit X positions
          stat_x_positions <- c(0.450, 0.522, 0.594, 0.666, 0.738, 0.810)
          
          # RV/100
          rv_val <- if(!is.na(profile$rv100)) sprintf("%+.1f", profile$rv100) else "-"
          draw_stat_pill(stat_x_positions[1], row1_y, paste0("RV:", rv_val), profile$rv100, 0, TRUE, 0.058)
          
          # wOBA
          woba_val <- if(!is.na(profile$woba)) sprintf(".%03d", round(profile$woba * 1000)) else "-"
          draw_stat_pill(stat_x_positions[2], row1_y, paste0("wOBA:", woba_val), profile$woba, 0.320, TRUE, 0.068)
          
          # EV90
          ev90_val <- if(!is.na(profile$ev90)) sprintf("%.0f", profile$ev90) else "-"
          draw_stat_pill(stat_x_positions[3], row1_y, paste0("EV90:", ev90_val), profile$ev90, 95, TRUE, 0.058)
          
          # K%
          k_val <- if(!is.na(profile$k_pct)) sprintf("%.0f%%", profile$k_pct) else "-"
          draw_stat_pill(stat_x_positions[4], row1_y, paste0("K:", k_val), profile$k_pct, 22, FALSE, 0.052)
          
          # BB%
          bb_val <- if(!is.na(profile$bb_pct)) sprintf("%.0f%%", profile$bb_pct) else "-"
          draw_stat_pill(stat_x_positions[5], row1_y, paste0("BB:", bb_val), profile$bb_pct, 9, TRUE, 0.052)
          
          # Whiff%
          whiff_val <- if(!is.na(profile$whiff_pct)) sprintf("%.0f%%", profile$whiff_pct) else "-"
          draw_stat_pill(stat_x_positions[6], row1_y, paste0("Wh:", whiff_val), profile$whiff_pct, 25, FALSE, 0.052)
          
          # Spray tendencies - explicit X positions
          spray_x_positions <- c(0.888, 0.930, 0.972)
          
          pull_val <- if(!is.na(profile$pull_pct)) sprintf("%.0f", profile$pull_pct) else "-"
          draw_stat_pill(spray_x_positions[1], row1_y, paste0("P:", pull_val), profile$pull_pct, 40, TRUE, SPRAY_PILL_WIDTH)
          
          mid_val <- if(!is.na(profile$middle_pct)) sprintf("%.0f", profile$middle_pct) else "-"
          draw_stat_pill(spray_x_positions[2], row1_y, paste0("M:", mid_val), profile$middle_pct, 38, TRUE, SPRAY_PILL_WIDTH)
          
          oppo_val <- if(!is.na(profile$oppo_pct)) sprintf("%.0f", profile$oppo_pct) else "-"
          draw_stat_pill(spray_x_positions[3], row1_y, paste0("O:", oppo_val), profile$oppo_pct, 22, TRUE, SPRAY_PILL_WIDTH)
          
          # === ROW 2: NOTES ===
          
          grid::grid.rect(x = 0.5, y = row2_y, width = PAGE_WIDTH, height = ROW2_HEIGHT,
                         gp = grid::gpar(fill = ROW2_BG, col = "gray80", lwd = 0.3))
          
          # Notes with explicit X positions
          note_x_positions <- c(0.025, 0.345, 0.665)  # LHP, RHP, Overall
          
          if (nchar(notes$lhp_plan) > 0) {
            lhp_text <- paste0("LHP: ", substr(notes$lhp_plan, 1, 55))
            grid::grid.text(lhp_text, x = note_x_positions[1], y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = LHP_COLOR, fontface = "bold"))
          }
          
          if (nchar(notes$rhp_plan) > 0) {
            rhp_text <- paste0("RHP: ", substr(notes$rhp_plan, 1, 55))
            grid::grid.text(rhp_text, x = note_x_positions[2], y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 4, col = RHP_COLOR, fontface = "bold"))
          }
          
          if (nchar(notes$overall_notes) > 0) {
            overall_text <- paste0("Notes: ", substr(notes$overall_notes, 1, 50))
            grid::grid.text(overall_text, x = note_x_positions[3], y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = OVERALL_COLOR, fontface = "bold"))
          }
          
          if (nchar(notes$lhp_plan) == 0 && nchar(notes$rhp_plan) == 0 && nchar(notes$overall_notes) == 0) {
            grid::grid.text("(add scouting notes)", x = note_x_positions[1], y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
          }
          
          # === ROW 3: DIAMONDS AND IN-GAME NOTES ===
          
          # Draw 5 diamonds - explicit X positions
          diamond_x_positions <- c(0.060, 0.148, 0.236, 0.324, 0.412)
          
          for (d in 1:5) {
            draw_diamond(diamond_x_positions[d], row3_y, DIAMOND_SIZE)
            grid::grid.text("P:_______", x = diamond_x_positions[d], y = row3_y + DIAMOND_SIZE + 0.010, 
                           gp = grid::gpar(fontsize = 5, col = "gray40"))
          }
          
          # In-game notes box
          notes_box_center_x <- NOTES_BOX_X + NOTES_BOX_WIDTH / 2  # 0.50 + 0.23 = 0.73
          grid::grid.rect(x = notes_box_center_x, y = row3_y, 
                         width = NOTES_BOX_WIDTH, height = NOTES_BOX_HEIGHT,
                         gp = grid::gpar(fill = "#fafafa", col = "gray60", lwd = 0.3))
          grid::grid.text("In-Game Notes:", x = NOTES_BOX_X + 0.01, y = row3_y + NOTES_BOX_HEIGHT/2 - 0.005, just = "left",
                         gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
          
          # Dotted lines for writing - explicit Y positions relative to row3_y
          note_line_y_offsets <- c(0.012, 0.023, 0.034, 0.045)
          for (line_idx in seq_along(note_line_y_offsets)) {
            line_y <- row3_y + NOTES_BOX_HEIGHT/2 - note_line_y_offsets[line_idx]
            grid::grid.lines(x = c(NOTES_BOX_X + 0.01, NOTES_BOX_X + NOTES_BOX_WIDTH - 0.01), 
                            y = c(line_y, line_y),
                            gp = grid::gpar(col = "gray80", lwd = 0.3, lty = "dotted"))
          }
          
          # === SEPARATOR LINE ===
          if (h_idx < length(page_hitters)) {
            sep_y <- row_top - SEPARATOR_OFFSET  # e.g., 0.940 - 0.117 = 0.823
            grid::grid.lines(x = c(PAGE_MARGIN_LEFT, PAGE_MARGIN_RIGHT), y = c(sep_y, sep_y),
                            gp = grid::gpar(col = "#006F71", lwd = 0.5, lty = "dashed"))
          }
        }
        
        # === FOOTER ===
        grid::grid.text("Data: TrackMan | Generated: Coastal Carolina Baseball Analytics", x = 0.5, y = FOOTER_Y,
                       gp = grid::gpar(fontsize = FOOTER_FONT_SIZE, col = "gray50", fontface = "italic"))
      }
      
      dev.off()
    }
  )
                                                
  # ============================================================================
  # MAC MATCHUP MATRIX SERVER LOGIC
  # Based on: "Using Euclidean Distance and Clustering to quantify batter vs. pitcher matchups"
  # by Chap Cunningham and Zack Aisen (Saber Seminar 2025)
  # ============================================================================
  
  # Helper function for MAC score color (0-100 scale, higher = pitcher advantage)
  get_mac_score_color <- function(score) {
    if (is.na(score)) return("#E0E0E0")
    if (score >= 80) return("#1B5E20")  # Dark green - strong pitcher
    if (score >= 60) return("#4CAF50")  # Green - pitcher advantage
    if (score >= 55) return("#8BC34A")  # Light green - slight pitcher
    if (score >= 45) return("#FFF9C4")  # Yellow - neutral
    if (score >= 40) return("#FFEB3B")  # Yellow - neutral
    if (score >= 20) return("#FF9800")  # Orange - hitter advantage
    return("#C62828")                    # Red - strong hitter
  }
  
  # Helper function for MAC score text color
  get_mac_text_color <- function(score) {
    if (is.na(score)) return("#666666")
    if (score >= 60 || score <= 20) return("white")
    return("#333333")
  }
  
  # Generate MAC Matchup Matrix using Euclidean distance similarity
  observeEvent(input$generate_mac_matrix, {
    req(input$mac_pitchers, input$mac_hitters)
    
    pitchers <- input$mac_pitchers
    hitters <- input$mac_hitters
    
    if (length(pitchers) == 0) {
      showNotification("Please select at least one pitcher", type = "warning")
      return()
    }
    
    if (length(hitters) == 0) {
      showNotification("Please select at least one hitter", type = "warning")
      return()
    }
    
    # Build results using MAC methodology
    results_list <- list()
    
    withProgress(message = "Computing MAC matchups...", value = 0, {
      total <- length(pitchers) * length(hitters)
      current <- 0
      
      for (pitcher in pitchers) {
        for (hitter in hitters) {
          key <- paste0(pitcher, "||", hitter)
          
          # Calculate MAC matchup using Euclidean distance (0.6 threshold)
          mac_result <- calculate_mac_matchup(pitcher, hitter, MAC_DISTANCE_THRESHOLD)
          
          # Also get pitch-type specific scores
          fb_mac <- calculate_pitch_type_matchup(pitcher, hitter, "FB", MAC_DISTANCE_THRESHOLD)
          bb_mac <- calculate_pitch_type_matchup(pitcher, hitter, "BB", MAC_DISTANCE_THRESHOLD)
          os_mac <- calculate_pitch_type_matchup(pitcher, hitter, "OS", MAC_DISTANCE_THRESHOLD)
          
          results_list[[key]] <- list(
            score = mac_result$score,
            rv100 = mac_result$rv100,
            n_similar = mac_result$n_similar,
            whiff_pct = mac_result$whiff_pct,
            chase_pct = mac_result$chase_pct,
            woba = mac_result$woba,
            confidence = mac_result$confidence,
            fb_score = fb_mac$score,
            fb_rv = fb_mac$rv100,
            fb_n = fb_mac$n_similar,
            bb_score = bb_mac$score,
            bb_rv = bb_mac$rv100,
            bb_n = bb_mac$n_similar,
            os_score = os_mac$score,
            os_rv = os_mac$rv100,
            os_n = os_mac$n_similar
          )
          
          current <- current + 1
          incProgress(1/total)
        }
      }
    })
    
    mac_matrix_data(list(
      results = results_list, 
      pitchers = pitchers, 
      hitters = hitters
    ))
    showNotification("MAC Matrix generated!", type = "message")
  })
  
  # Handle note updates from matrix cells
  observeEvent(input$mac_note_update, {
    note_data <- input$mac_note_update
    if (!is.null(note_data)) {
      current_notes <- mac_notes()
      current_notes[[note_data$key]] <- note_data$value
      mac_notes(current_notes)
    }
  })
  
  # Render MAC Matchup Matrix
  output$mac_matrix_output <- renderUI({
    data <- mac_matrix_data()
    if (is.null(data)) {
      return(div(style = "text-align: center; padding: 40px; color: #666;",
                 p("Select pitchers and hitters, then click 'Generate MAC Matrix'."),
                 p(style = "font-size: 12px; margin-top: 10px;", 
                   "MAC uses Euclidean distance to find similar pitches hitters have faced, providing a matchup score from 0-100.")))
    }
    
    results <- data$results
    pitchers <- data$pitchers
    hitters <- data$hitters
    current_notes <- mac_notes()
    
    # Build HTML table with hitters on rows, pitchers on columns
    # Header row with pitcher names
    header_cells <- lapply(pitchers, function(p) {
      p_hand <- pitcher_arsenal %>% filter(Pitcher == p) %>% pull(PitcherThrows) %>% unique() %>% first()
      p_hand_label <- if(!is.na(p_hand)) paste0(" (", substr(p_hand, 1, 1), "HP)") else ""
      
      tags$th(style = "padding: 10px; background: #006F71; color: white; font-size: 11px; min-width: 170px; text-align: center; vertical-align: bottom;", 
              div(style = "font-weight: bold;", substr(p, 1, 18)),
              div(style = "font-size: 9px; font-weight: normal;", p_hand_label))
    })
    
    # Table rows - one per hitter
    table_rows <- lapply(hitters, function(h_name) {
      # Get hitter hand
      h_hand <- tm_data %>% filter(Batter == h_name) %>% pull(BatterSide) %>% first()
      h_hand_label <- if(!is.na(h_hand)) paste0("(", substr(h_hand, 1, 1), ")") else ""
      
      # Create cells for each pitcher
      matchup_cells <- lapply(pitchers, function(pitcher) {
        key <- paste0(pitcher, "||", h_name)
        matchup <- results[[key]]
        
        if (is.null(matchup) || is.na(matchup$score)) {
          return(tags$td(style = "padding: 8px; text-align: center; background: #E0E0E0; vertical-align: top;",
                        div(style = "font-size: 14px; color: #666;", "N/A"),
                        div(style = "font-size: 10px; color: #888; margin-top: 3px;", "Insufficient data"),
                        tags$textarea(
                          class = "mac-note-input",
                          style = "width: 100%; height: 35px; font-size: 9px; border: 1px solid #ccc; border-radius: 3px; padding: 2px; resize: none; margin-top: 5px;",
                          placeholder = "Notes...",
                          onchange = sprintf("Shiny.setInputValue('mac_note_update', {key: '%s', value: this.value}, {priority: 'event'});", key)
                        )))
        }
        
        # Get colors based on MAC score
        score_bg <- get_mac_score_color(matchup$score)
        score_text <- get_mac_text_color(matchup$score)
        
        fb_bg <- get_mac_score_color(matchup$fb_score)
        bb_bg <- get_mac_score_color(matchup$bb_score)
        os_bg <- get_mac_score_color(matchup$os_score)
        
        # Format values
        format_score <- function(val) if(is.na(val)) "-" else sprintf("%d", val)
        format_rv <- function(val) if(is.na(val)) "-" else sprintf("%+.1f", val)
        format_pct <- function(val) if(is.na(val)) "-" else sprintf("%.0f%%", val)
        
        # Get existing note
        existing_note <- if (!is.null(current_notes[[key]])) current_notes[[key]] else ""
        
        # Confidence badge color
        conf_color <- switch(matchup$confidence,
          "High" = "#4CAF50",
          "Medium" = "#FF9800", 
          "#F44336")
        
        # Cell content
        tags$td(style = paste0("padding: 6px; vertical-align: top; background: ", score_bg, "; border: 1px solid #ddd;"),
          # Overall MAC Score (large, prominent)
          div(style = paste0("text-align: center; font-size: 28px; font-weight: bold; color: ", score_text, ";"),
              format_score(matchup$score)),
          
          # Confidence and sample size
          div(style = "text-align: center; margin-top: 2px;",
              tags$span(style = paste0("font-size: 9px; padding: 1px 5px; border-radius: 3px; background: ", conf_color, "; color: white;"),
                       matchup$confidence),
              tags$span(style = "font-size: 9px; color: #666; margin-left: 5px;", 
                       paste0("n=", matchup$n_similar))),
          
          # RV/100 and key metrics
          div(style = "text-align: center; font-size: 10px; color: #444; margin-top: 4px;",
              paste0("RV/100: ", format_rv(matchup$rv100))),
          
          div(style = "display: flex; justify-content: center; gap: 8px; font-size: 9px; margin-top: 3px; color: #555;",
              tags$span(paste0("W:", format_pct(matchup$whiff_pct))),
              tags$span(paste0("Ch:", format_pct(matchup$chase_pct)))),
          
          # Pitch type breakdown with mini-scores
          div(style = "display: flex; justify-content: space-around; font-size: 9px; margin-top: 5px; padding-top: 5px; border-top: 1px solid rgba(0,0,0,0.1);",
              tags$span(style = paste0("background: ", fb_bg, "; color: ", get_mac_text_color(matchup$fb_score), "; padding: 2px 5px; border-radius: 3px;"),
                       paste0("FB:", format_score(matchup$fb_score))),
              tags$span(style = paste0("background: ", bb_bg, "; color: ", get_mac_text_color(matchup$bb_score), "; padding: 2px 5px; border-radius: 3px;"),
                       paste0("BB:", format_score(matchup$bb_score))),
              tags$span(style = paste0("background: ", os_bg, "; color: ", get_mac_text_color(matchup$os_score), "; padding: 2px 5px; border-radius: 3px;"),
                       paste0("OS:", format_score(matchup$os_score)))
          ),
          
          # Notes input
          tags$textarea(
            class = "mac-note-input",
            style = "width: 100%; height: 35px; font-size: 9px; border: 1px solid rgba(0,0,0,0.15); border-radius: 3px; padding: 2px; resize: none; margin-top: 5px; background: rgba(255,255,255,0.7);",
            placeholder = "Notes...",
            onchange = sprintf("Shiny.setInputValue('mac_note_update', {key: '%s', value: this.value}, {priority: 'event'});", key),
            existing_note
          )
        )
      })
      
      tags$tr(
        tags$td(style = "padding: 8px; background: #f5f5f5; font-weight: bold; font-size: 11px; vertical-align: middle; min-width: 140px;", 
                div(substr(h_name, 1, 20)),
                div(style = "font-size: 9px; color: #666; font-weight: normal;", h_hand_label)),
        matchup_cells
      )
    })
    
    # Legend row at bottom
    legend_row <- tags$tr(
      tags$td(colspan = length(pitchers) + 1, style = "padding: 12px; background: #fafafa; font-size: 10px; color: #666;",
        div(style = "display: flex; justify-content: center; gap: 25px; flex-wrap: wrap;",
          tags$span(tags$b("MAC Score:"), " 0-100 (100 = Strong Pitcher Advantage)"),
          tags$span(tags$b("RV/100:"), " Run Value per 100 similar pitches"),
          tags$span(tags$b("FB/BB/OS:"), " Fastball/Breaking/Offspeed scores"),
          tags$span(tags$b("W:"), " Whiff%"),
          tags$span(tags$b("Ch:"), " Chase%")
        ),
        div(style = "text-align: center; margin-top: 5px; font-size: 9px;",
            "Methodology: Scans pitches using Euclidean distance (threshold 0.6) on Velocity, IVB, HB, Spin, Release Height, Release Side")
      )
    )
    
    tagList(
      tags$style(HTML("
        .mac-note-input:focus {
          outline: 2px solid #006F71;
          border-color: #006F71;
        }
      ")),
      tags$table(style = "border-collapse: collapse; width: 100%; margin-top: 10px;",
                 tags$thead(
                   tags$tr(
                     tags$th(style = "padding: 10px; background: #37474F; color: white; font-size: 12px;", "Hitter \\ Pitcher"), 
                     header_cells
                   )
                 ),
                 tags$tbody(table_rows, legend_row))
    )
  })
  
  # Download MAC Matrix as PNG
  output$download_mac_png <- downloadHandler(
    filename = function() {
      paste0("MAC_Matchup_Matrix_", format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      data <- mac_matrix_data()
      if (is.null(data)) {
        png(file, width = 400, height = 200)
        plot.new()
        text(0.5, 0.5, "No data to export. Generate matrix first.", cex = 1.2)
        dev.off()
        return()
      }
      
      results <- data$results
      pitchers <- data$pitchers
      hitters <- data$hitters
      current_notes <- mac_notes()
      
      # Calculate dimensions
      n_pitchers <- length(pitchers)
      n_hitters <- length(hitters)
      
      cell_width <- 150
      cell_height <- 100
      header_height <- 50
      row_label_width <- 170
      
      img_width <- row_label_width + n_pitchers * cell_width + 40
      img_height <- header_height + n_hitters * cell_height + 100
      
      png(file, width = img_width, height = img_height, res = 100)
      
      grid::grid.newpage()
      
      # Title
      grid::grid.text("MAC Matchup Matrix (Euclidean Distance Method)", x = 0.5, y = 0.97, 
                     gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#006F71"))
      grid::grid.text(paste0("Generated: ", Sys.Date(), " | Score: 0-100 (100 = Pitcher Advantage)"), x = 0.5, y = 0.94,
                     gp = grid::gpar(fontsize = 9, col = "gray50"))
      
      plot_top <- 0.90
      
      # Draw header row (pitcher names)
      for (p_idx in seq_along(pitchers)) {
        x_pos <- (row_label_width + (p_idx - 0.5) * cell_width) / img_width
        y_pos <- plot_top
        
        p_hand <- pitcher_arsenal %>% filter(Pitcher == pitchers[p_idx]) %>% pull(PitcherThrows) %>% unique() %>% first()
        p_label <- if(!is.na(p_hand)) paste0(substr(pitchers[p_idx], 1, 14), " (", substr(p_hand, 1, 1), ")") else substr(pitchers[p_idx], 1, 15)
        
        grid::grid.rect(x = x_pos, y = y_pos, width = (cell_width - 2) / img_width, height = 0.04,
                       gp = grid::gpar(fill = "#006F71", col = "white"))
        grid::grid.text(p_label, x = x_pos, y = y_pos,
                       gp = grid::gpar(fontsize = 8, col = "white", fontface = "bold"))
      }
      
      # Row label header
      grid::grid.rect(x = (row_label_width / 2) / img_width, y = plot_top, 
                     width = (row_label_width - 4) / img_width, height = 0.04,
                     gp = grid::gpar(fill = "#37474F", col = "white"))
      grid::grid.text("Hitter", x = (row_label_width / 2) / img_width, y = plot_top,
                     gp = grid::gpar(fontsize = 9, col = "white", fontface = "bold"))
      
      # Draw data rows
      for (h_idx in seq_along(hitters)) {
        hitter <- hitters[h_idx]
        row_y <- plot_top - 0.04 - (h_idx - 0.5) * (cell_height / img_height)
        
        # Hitter name cell
        grid::grid.rect(x = (row_label_width / 2) / img_width, y = row_y,
                       width = (row_label_width - 4) / img_width, height = (cell_height - 2) / img_height,
                       gp = grid::gpar(fill = "#f5f5f5", col = "#ddd"))
        grid::grid.text(substr(hitter, 1, 20), x = (row_label_width / 2) / img_width, y = row_y,
                       gp = grid::gpar(fontsize = 8, fontface = "bold"))
        
        # Data cells
        for (p_idx in seq_along(pitchers)) {
          pitcher <- pitchers[p_idx]
          key <- paste0(pitcher, "||", hitter)
          matchup <- results[[key]]
          
          x_pos <- (row_label_width + (p_idx - 0.5) * cell_width) / img_width
          
          if (is.null(matchup) || is.na(matchup$score)) {
            grid::grid.rect(x = x_pos, y = row_y,
                           width = (cell_width - 2) / img_width, height = (cell_height - 2) / img_height,
                           gp = grid::gpar(fill = "#E0E0E0", col = "#ddd"))
            grid::grid.text("N/A", x = x_pos, y = row_y,
                           gp = grid::gpar(fontsize = 12, col = "#666"))
          } else {
            bg_col <- get_mac_score_color(matchup$score)
            text_col <- get_mac_text_color(matchup$score)
            
            grid::grid.rect(x = x_pos, y = row_y,
                           width = (cell_width - 2) / img_width, height = (cell_height - 2) / img_height,
                           gp = grid::gpar(fill = bg_col, col = "#ddd"))
            
            # MAC Score (large)
            grid::grid.text(sprintf("%d", matchup$score), x = x_pos, y = row_y + 0.03,
                           gp = grid::gpar(fontsize = 18, fontface = "bold", col = text_col))
            
            # RV/100
            format_rv <- function(val) if(is.na(val)) "-" else sprintf("%+.1f", val)
            grid::grid.text(paste0("RV/100: ", format_rv(matchup$rv100)), x = x_pos, y = row_y,
                           gp = grid::gpar(fontsize = 7, col = text_col))
            
            # Pitch type breakdown
            breakdown_text <- paste0("FB:", matchup$fb_score, " BB:", matchup$bb_score, " OS:", matchup$os_score)
            grid::grid.text(breakdown_text, x = x_pos, y = row_y - 0.02,
                           gp = grid::gpar(fontsize = 6, col = text_col))
            
            # Sample size
            grid::grid.text(paste0("n=", matchup$n_similar, " (", matchup$confidence, ")"), x = x_pos, y = row_y - 0.035,
                           gp = grid::gpar(fontsize = 5, col = text_col))
            
            # Notes if present
            note <- current_notes[[key]]
            if (!is.null(note) && nchar(note) > 0) {
              grid::grid.text(substr(note, 1, 18), x = x_pos, y = row_y - 0.05,
                             gp = grid::gpar(fontsize = 5, col = text_col, fontface = "italic"))
            }
          }
        }
      }
      
      # Legend
      legend_y <- 0.03
      grid::grid.text("MAC Score: 0-100 (100=Pitcher Adv) | Euclidean Distance Threshold: 0.6 | Features: Velo, IVB, HB, Spin, RelHt, RelSide",
                     x = 0.5, y = legend_y, gp = grid::gpar(fontsize = 8, col = "gray50"))
      
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
