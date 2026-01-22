# ============================================================================
# HITTER SCOUTING CARDS APP - Streamlined Version
# ============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(scales)

# Debounce helper for optimized note-taking
debounce_value <- function(value, millis = 500) {
  shiny::debounce(value, millis)
}

# ============================================================================
# DATA LOADING - MEMORY OPTIMIZED
# ============================================================================

cat("Loading TrackMan data with memory optimization...\n")

# Force garbage collection before loading
gc(verbose = FALSE, full = TRUE)

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
  
  tm_data <- data.frame(
    Pitcher = sample(pitchers, n, replace = TRUE),
    Batter = sample(hitters, n, replace = TRUE),
    PitcherThrows = sample(c("Left", "Right"), n, replace = TRUE, prob = c(0.3, 0.7)),
    BatterSide = sample(c("Left", "Right"), n, replace = TRUE, prob = c(0.4, 0.6)),
    BatterTeam = sample(sec_teams, n, replace = TRUE),
    TaggedPitchType = sample(pitch_types, n, replace = TRUE),
    PlateLocSide = rnorm(n, 0, 0.8),
    PlateLocHeight = rnorm(n, 2.5, 0.8),
    ExitSpeed = c(rep(NA, n*0.6), rnorm(n*0.4, 88, 10)),
    Angle = c(rep(NA, n*0.6), rnorm(n*0.4, 15, 15)),
    Distance = c(rep(NA, n*0.6), rnorm(n*0.4, 250, 80)),
    Bearing = c(rep(NA, n*0.6), rnorm(n*0.4, 0, 25)),
    PitchCall = sample(c("StrikeCalled", "BallCalled", "StrikeSwinging", "FoulBall", "InPlay"), 
                       n, replace = TRUE, prob = c(0.20, 0.30, 0.10, 0.15, 0.25)),
    PlayResult = sample(c(NA, "Single", "Double", "Triple", "HomeRun", "Out", "FieldersChoice"), 
                        n, replace = TRUE, prob = c(0.75, 0.08, 0.04, 0.01, 0.02, 0.08, 0.02)),
    Balls = sample(0:3, n, replace = TRUE),
    Strikes = sample(0:2, n, replace = TRUE),
    Date = sample(seq(as.Date("2024-02-15"), as.Date("2024-05-15"), by = "day"), n, replace = TRUE),
    # TM2025 specific columns
    mean_DRE_bat = rnorm(n, 0, 0.03),
    woba = c(rep(NA, n*0.75), runif(n*0.25, 0, 1.5)),
    wobacon = c(rep(NA, n*0.75), runif(n*0.25, 0.1, 0.8)),
    is_put_away = sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15)),
    is_walk = sample(0:1, n, replace = TRUE, prob = c(0.92, 0.08)),
    is_ab = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
    slg = c(rep(NA, n*0.7), runif(n*0.3, 0, 4)),
    is_hit = sample(0:1, n, replace = TRUE, prob = c(0.75, 0.25)),
    in_zone = sample(0:1, n, replace = TRUE, prob = c(0.5, 0.5)),
    in_zone_whiff = sample(0:1, n, replace = TRUE, prob = c(0.95, 0.05)),
    is_whiff = sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15)),
    is_swing = sample(0:1, n, replace = TRUE, prob = c(0.55, 0.45)),
    chase = sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15)),
    is_pa = sample(0:1, n, replace = TRUE, prob = c(0.75, 0.25)),
    is_k = sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15)),
    stringsAsFactors = FALSE
  )
}

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

# Create pitcher arsenal summary
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

# Memory cleanup after initial data processing
cat("Data loading complete. Memory cleanup...\n")
gc(verbose = FALSE)
cat("Final memory usage:", round(sum(gc()[,2]), 1), "MB\n")
cat("Loaded", nrow(tm_data), "pitch records for", length(all_hitters), "hitters and", length(all_pitchers), "pitchers\n")

# ============================================================================
# MAC-STYLE MATCHUP CALCULATION FUNCTION
# ============================================================================

calculate_mac_matchup <- function(p_name, h_name, distance_threshold = 2.5) {
  # Get pitcher's arsenal
  p_arsenal <- pitcher_arsenal %>% filter(Pitcher == p_name)
  
  if (nrow(p_arsenal) == 0) {
    return(list(score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL))
  }
  
  p_hand <- p_arsenal$PitcherThrows[1]
  
  # Get batter's pitch history
  batter_pitches <- batter_pitch_performance %>%
    filter(Batter == h_name)
  
  if (nrow(batter_pitches) < 20) {
    return(list(score = 50, rv100 = 0, n_similar = 0, by_pitch = NULL))
  }
  
  # For each pitch type in pitcher's arsenal, find similar pitches batter has faced
  results_by_pitch <- list()
  total_similar <- 0
  
  for (i in 1:nrow(p_arsenal)) {
    pitch_type <- p_arsenal$PitchFamily[i]
    usage <- p_arsenal$usage_pct[i]
    
    # Target pitch profile (standardized)
    target <- list(
      RelSpeed_z = (p_arsenal$RelSpeed[i] - feature_means$RelSpeed) / feature_sds$RelSpeed,
      IVB_z = (p_arsenal$InducedVertBreak[i] - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak,
      HB_z = (p_arsenal$HorzBreak[i] - feature_means$HorzBreak) / feature_sds$HorzBreak,
      SpinRate_z = (p_arsenal$SpinRate[i] - feature_means$SpinRate) / feature_sds$SpinRate,
      RelHeight_z = (p_arsenal$RelHeight[i] - feature_means$RelHeight) / feature_sds$RelHeight,
      RelSide_z = (p_arsenal$RelSide[i] - feature_means$RelSide) / feature_sds$RelSide
    )
    
    # Handle NA in target
    target <- lapply(target, function(x) if(is.na(x) || is.nan(x)) 0 else x)
    
    # Calculate distance for each pitch batter has seen
    batter_pitches_calc <- batter_pitches %>%
      mutate(
        RelSpeed_z_safe = ifelse(is.na(RelSpeed_z), 0, RelSpeed_z),
        IVB_z_safe = ifelse(is.na(IVB_z), 0, IVB_z),
        HB_z_safe = ifelse(is.na(HB_z), 0, HB_z),
        SpinRate_z_safe = ifelse(is.na(SpinRate_z), 0, SpinRate_z),
        RelHeight_z_safe = ifelse(is.na(RelHeight_z), 0, RelHeight_z),
        RelSide_z_safe = ifelse(is.na(RelSide_z), 0, RelSide_z),
        distance = sqrt(
          (RelSpeed_z_safe - target$RelSpeed_z)^2 +
            (IVB_z_safe - target$IVB_z)^2 +
            (HB_z_safe - target$HB_z)^2 +
            (SpinRate_z_safe - target$SpinRate_z)^2 +
            (RelHeight_z_safe - target$RelHeight_z)^2 +
            (RelSide_z_safe - target$RelSide_z)^2
        )
      )
    
    # Filter to similar pitches
    similar_pitches <- batter_pitches_calc %>%
      filter(!is.na(distance), distance <= distance_threshold)
    
    n_similar_pitch <- nrow(similar_pitches)
    
    if (n_similar_pitch >= 5) {
      rv100_similar <- 100 * mean(similar_pitches$hitter_rv, na.rm = TRUE)
      
      # Apply shrinkage based on sample size
      k <- 60
      shrinkage <- n_similar_pitch / (n_similar_pitch + k)
      rv100_shrunk <- shrinkage * rv100_similar + (1 - shrinkage) * 0
      
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = rv100_shrunk,
        rv100_raw = rv100_similar,
        usage = usage
      )
      total_similar <- total_similar + n_similar_pitch
    } else {
      results_by_pitch[[pitch_type]] <- list(
        n = n_similar_pitch,
        rv100 = 0,
        rv100_raw = NA,
        usage = usage
      )
    }
  }
  
  # Calculate weighted RV/100
  weighted_rv100 <- 0
  total_usage_weight <- 0
  
  for (pt in names(results_by_pitch)) {
    pitch_data <- results_by_pitch[[pt]]
    usage <- pitch_data$usage
    
    if (pitch_data$n >= 5 && !is.na(pitch_data$rv100_raw)) {
      usage_weight <- usage^1.5
      weighted_rv100 <- weighted_rv100 + pitch_data$rv100 * usage_weight
      total_usage_weight <- total_usage_weight + usage_weight
    }
  }
  
  if (total_usage_weight > 0) {
    weighted_rv100 <- weighted_rv100 / total_usage_weight
  } else {
    weighted_rv100 <- 0
  }
  
  # Convert RV/100 to 0-100 score
  # SCORING: 0 = hitter advantage, 100 = pitcher advantage
  rv_score_contribution <- -weighted_rv100 * 15
  base_score <- 50 + rv_score_contribution
  final_score <- round(max(15, min(85, base_score)))
  
  if (abs(weighted_rv100) > 2) {
    final_score <- round(max(5, min(95, 50 + rv_score_contribution)))
  }
  
  list(
    score = final_score,
    rv100 = round(weighted_rv100, 2),
    n_similar = total_similar,
    by_pitch = results_by_pitch
  )
}

# Calculate pitch type specific matchup (FB, BB, OS)
calculate_pitch_type_matchup <- function(p_name, h_name, pitch_family, distance_threshold = 2.5) {
  # Get pitcher's arsenal for specific pitch type
  p_arsenal <- pitcher_arsenal %>% filter(Pitcher == p_name, PitchFamily == pitch_family)
  
  if (nrow(p_arsenal) == 0) {
    return(list(score = 50, rv100 = 0, n_similar = 0))
  }
  
  # Get batter's pitch history
  batter_pitches <- batter_pitch_performance %>%
    filter(Batter == h_name)
  
  if (nrow(batter_pitches) < 20) {
    return(list(score = 50, rv100 = 0, n_similar = 0))
  }
  
  # Target pitch profile (standardized)
  target <- list(
    RelSpeed_z = (p_arsenal$RelSpeed[1] - feature_means$RelSpeed) / feature_sds$RelSpeed,
    IVB_z = (p_arsenal$InducedVertBreak[1] - feature_means$InducedVertBreak) / feature_sds$InducedVertBreak,
    HB_z = (p_arsenal$HorzBreak[1] - feature_means$HorzBreak) / feature_sds$HorzBreak,
    SpinRate_z = (p_arsenal$SpinRate[1] - feature_means$SpinRate) / feature_sds$SpinRate,
    RelHeight_z = (p_arsenal$RelHeight[1] - feature_means$RelHeight) / feature_sds$RelHeight,
    RelSide_z = (p_arsenal$RelSide[1] - feature_means$RelSide) / feature_sds$RelSide
  )
  
  target <- lapply(target, function(x) if(is.na(x) || is.nan(x)) 0 else x)
  
  # Calculate distance for each pitch batter has seen
  batter_pitches_calc <- batter_pitches %>%
    mutate(
      RelSpeed_z_safe = ifelse(is.na(RelSpeed_z), 0, RelSpeed_z),
      IVB_z_safe = ifelse(is.na(IVB_z), 0, IVB_z),
      HB_z_safe = ifelse(is.na(HB_z), 0, HB_z),
      SpinRate_z_safe = ifelse(is.na(SpinRate_z), 0, SpinRate_z),
      RelHeight_z_safe = ifelse(is.na(RelHeight_z), 0, RelHeight_z),
      RelSide_z_safe = ifelse(is.na(RelSide_z), 0, RelSide_z),
      distance = sqrt(
        (RelSpeed_z_safe - target$RelSpeed_z)^2 +
          (IVB_z_safe - target$IVB_z)^2 +
          (HB_z_safe - target$HB_z)^2 +
          (SpinRate_z_safe - target$SpinRate_z)^2 +
          (RelHeight_z_safe - target$RelHeight_z)^2 +
          (RelSide_z_safe - target$RelSide_z)^2
      )
    )
  
  similar_pitches <- batter_pitches_calc %>%
    filter(!is.na(distance), distance <= distance_threshold)
  
  n_similar <- nrow(similar_pitches)
  
  if (n_similar >= 5) {
    rv100_similar <- 100 * mean(similar_pitches$hitter_rv, na.rm = TRUE)
    k <- 60
    shrinkage <- n_similar / (n_similar + k)
    rv100_shrunk <- shrinkage * rv100_similar + (1 - shrinkage) * 0
    
    rv_score_contribution <- -rv100_shrunk * 15
    base_score <- 50 + rv_score_contribution
    final_score <- round(max(15, min(85, base_score)))
    
    return(list(score = final_score, rv100 = round(rv100_shrunk, 2), n_similar = n_similar))
  }
  
  list(score = 50, rv100 = 0, n_similar = n_similar)
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
      # Swing Decision composite: z_swing - chase (higher = better decisions)
      swing_dec_score = 100 * sum(Zswing, na.rm = TRUE) / pmax(1, sum(in_zone, na.rm = TRUE)) - 
                        100 * sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)),
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
      swing_dec_score = 100 * sum(Zswing, na.rm = TRUE) / pmax(1, sum(in_zone, na.rm = TRUE)) - 
                        100 * sum(chase, na.rm = TRUE) / pmax(1, sum(out_of_zone, na.rm = TRUE)),
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
")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
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
    
    # ===== MATCHUP MATRICES TAB =====
    tabPanel("Matchup Matrices",
      div(class = "stats-header", h3("Matchup Matrices"), p("MAC-style pitcher vs hitter matchup calculations with 0-100 scores")),
      
      fluidRow(
        column(12,
          tabsetPanel(id = "matrix_subtabs", type = "pills",
            # Overall Matrix Tab
            tabPanel("Overall Matrix",
              div(class = "chart-box", style = "margin-top: 15px;",
                div(class = "chart-box-title", "Multi-Pitcher vs Multi-Hitter Matchup Matrix"),
                fluidRow(
                  column(4,
                    selectizeInput("matrix_pitchers", "Select Pitchers:", choices = NULL, multiple = TRUE,
                                   options = list(placeholder = "Search pitchers...", maxItems = 15))),
                  column(4,
                    selectizeInput("matrix_hitters", "Select Hitters:", choices = NULL, multiple = TRUE,
                                   options = list(placeholder = "Search hitters...", maxItems = 15))),
                  column(4,
                    actionButton("generate_matrix", "Generate Matrix", class = "btn-bronze", style = "margin-top: 25px;"))
                ),
                div(style = "margin-top: 10px; font-size: 12px; color: #666;",
                    "Score interpretation: 0-40 = Hitter Advantage (red), 45-55 = Neutral (yellow), 60-100 = Pitcher Advantage (green)"),
                hr(),
                div(style = "overflow-x: auto;", uiOutput("overall_matrix_output"))
              )
            ),
            
            # Pitch Type Matrix Tab
            tabPanel("Pitch Type Matrix",
              div(class = "chart-box", style = "margin-top: 15px;",
                div(class = "chart-box-title", "Single Pitcher vs Multiple Hitters - By Pitch Type"),
                fluidRow(
                  column(4,
                    selectizeInput("pt_matrix_pitcher", "Select Pitcher:", choices = NULL, multiple = FALSE,
                                   options = list(placeholder = "Search pitcher..."))),
                  column(4,
                    selectizeInput("pt_matrix_hitters", "Select Hitters:", choices = NULL, multiple = TRUE,
                                   options = list(placeholder = "Search hitters...", maxItems = 15))),
                  column(4,
                    actionButton("generate_pt_matrix", "Generate Pitch Type Matrix", class = "btn-bronze", style = "margin-top: 25px;"))
                ),
                div(style = "margin-top: 10px; font-size: 12px; color: #666;",
                    "Shows matchup scores for each pitch type group: FB (Fastballs), BB (Breaking Balls), OS (Offspeed)"),
                hr(),
                div(style = "overflow-x: auto;", uiOutput("pitch_type_matrix_output"))
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  scout_data <- reactiveVal(list())
  selected_hitter <- reactiveVal(NULL)
  matrix_data <- reactiveVal(NULL)
  pt_matrix_data <- reactiveVal(NULL)
  
  # Initialize all selectize inputs
  observe({ 
    updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE) 
    updateSelectizeInput(session, "matrix_pitchers", choices = all_pitchers, server = TRUE)
    updateSelectizeInput(session, "matrix_hitters", choices = all_hitters, server = TRUE)
    updateSelectizeInput(session, "pt_matrix_pitcher", choices = all_pitchers, server = TRUE)
    updateSelectizeInput(session, "pt_matrix_hitters", choices = all_hitters, server = TRUE)
  })
  
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
      
      # Helper function to draw a baseball diamond
      draw_diamond <- function(cx, cy, size) {
        # Diamond shape (rotated square)
        x_pts <- c(cx, cx + size, cx, cx - size, cx)
        y_pts <- c(cy + size, cy, cy - size, cy, cy + size)
        grid::grid.polygon(x = x_pts, y = y_pts, default.units = "npc",
                          gp = grid::gpar(fill = "#f5f5dc", col = "black", lwd = 0.5))
        # Base positions (small squares)
        base_size <- size * 0.12
        # Home plate (bottom)
        grid::grid.rect(x = cx, y = cy - size + base_size, width = base_size * 1.2, height = base_size * 0.8,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        # First base (right)
        grid::grid.rect(x = cx + size - base_size, y = cy, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        # Second base (top)
        grid::grid.rect(x = cx, y = cy + size - base_size, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
        # Third base (left)
        grid::grid.rect(x = cx - size + base_size, y = cy, width = base_size, height = base_size,
                       default.units = "npc", gp = grid::gpar(fill = "white", col = "black", lwd = 0.3))
      }
      
      # Helper function to get pill color for stats (green-red gradient)
      get_stat_pill_color <- function(val, benchmark, higher_better = TRUE, threshold_pct = 10) {
        if (is.na(val) || is.na(benchmark)) return(list(bg = "#E0E0E0", text = "#666666"))
        # Special handling for rv100 where benchmark is 0
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
        if (hand == "Left") return("#C62828")      # Red for lefty
        if (hand == "Switch") return("#1565C0")    # Blue for switch
        return("black")                             # Black for righty
      }
      
      # Helper function to draw grade legend
      draw_grade_legend <- function(y_pos) {
        # Grade categories with colors
        grade_cats <- list(
          list(label = "20-29", desc = "Poor", color = "#EF5350"),
          list(label = "30-39", desc = "Below Avg", color = "#FF8A65"),
          list(label = "40-44", desc = "Fringe", color = "#FFB74D"),
          list(label = "45-54", desc = "Average", color = "#FFF176"),
          list(label = "55-59", desc = "Above Avg", color = "#AED581"),
          list(label = "60-69", desc = "Plus", color = "#81C784"),
          list(label = "70-80", desc = "Elite", color = "#66BB6A")
        )
        
        legend_start_x <- 0.08
        spacing <- 0.125
        
        for (i in seq_along(grade_cats)) {
          cat <- grade_cats[[i]]
          x_pos <- legend_start_x + (i - 1) * spacing
          
          # Color box
          grid::grid.rect(x = x_pos, y = y_pos, width = 0.025, height = 0.012,
                         gp = grid::gpar(fill = cat$color, col = "gray50", lwd = 0.3))
          
          # Label text
          grid::grid.text(paste0(cat$label, " ", cat$desc), 
                         x = x_pos + 0.02, y = y_pos, just = "left",
                         gp = grid::gpar(fontsize = 5, col = "gray30"))
        }
      }
      
      # Calculate rows per page - each hitter gets about 0.125 of page height
      hitter_row_height <- 0.125
      max_hitters_per_page <- 7
      n_hitters <- length(data)
      n_pages <- ceiling(n_hitters / max_hitters_per_page)
      
      for (page in 1:n_pages) {
        if (page > 1) grid::grid.newpage()
        
        # Page header with custom title
        grid::grid.rect(x = 0.5, y = 0.975, width = 1, height = 0.045, 
                       gp = grid::gpar(fill = "#006F71", col = NA))
        grid::grid.text(pdf_title, x = 0.5, y = 0.975, 
                       gp = grid::gpar(fontsize = 14, fontface = "bold", col = "white"))
        grid::grid.text(format(Sys.Date(), "%m/%d/%Y"), x = 0.95, y = 0.975, just = "right",
                       gp = grid::gpar(fontsize = 8, col = "white"))
        grid::grid.text(paste0("Page ", page, " of ", n_pages), x = 0.05, y = 0.975, just = "left",
                       gp = grid::gpar(fontsize = 8, col = "white"))
        
        # Grade legend (only on first page, below header)
        if (page == 1) {
          draw_grade_legend(0.945)
        }
        
        # Get hitters for this page
        start_idx <- (page - 1) * max_hitters_per_page + 1
        end_idx <- min(page * max_hitters_per_page, n_hitters)
        page_hitters <- names(data)[start_idx:end_idx]
        
        # Draw each hitter row
        for (h_idx in seq_along(page_hitters)) {
          h_name <- page_hitters[h_idx]
          profile <- data[[h_name]]$profile
          notes <- data[[h_name]]
          
          # Calculate Y position for this hitter's row (from top)
          row_top <- 0.94 - (h_idx - 1) * hitter_row_height
          
          # Row 1: Name, Hand, Grades, Stats (narrow - about 0.025 height)
          row1_y <- row_top - 0.012
          
          # Background bar for row 1
          grid::grid.rect(x = 0.5, y = row1_y, width = 0.96, height = 0.024,
                         gp = grid::gpar(fill = if(h_idx %% 2 == 0) "#e8f5f4" else "#f0f0f0", col = NA))
          
          # Name (bold) with hand label closer and colored
          grid::grid.text(h_name, x = 0.02, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = 8, fontface = "bold"))
          
          # Hand label - closer to name and colored by handedness
          hand_color <- get_hand_color(profile$hand)
          grid::grid.text(paste0("(", profile$hand, ")"), x = 0.12, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = 7, col = hand_color, fontface = "bold"))
          
          # Grades with colored boxes (removed wOBA grade)
          grades <- c(profile$overall_grade, profile$raw_power_grade, profile$contact_grade, 
                     profile$avoid_k_grade, profile$swing_dec_grade)
          grade_labels <- c("OVR", "RAW", "CON", "AvK", "SwD")
          grade_start_x <- 0.17
          for (g_idx in 1:5) {
            gx <- grade_start_x + (g_idx - 1) * 0.04
            grid::grid.rect(x = gx, y = row1_y, width = 0.026, height = 0.018,
                           gp = grid::gpar(fill = grade_color_light(grades[g_idx]), col = "gray50", lwd = 0.3))
            grid::grid.text(grades[g_idx], x = gx, y = row1_y, 
                           gp = grid::gpar(fontsize = 5.5, fontface = "bold"))
          }
          
          # Stats with colored pills - moved even further to the right
          stat_start_x <- 0.42
          stat_spacing <- 0.072
          
          # RV/100
          rv_val <- if(!is.na(profile$rv100)) sprintf("%+.1f", profile$rv100) else "-"
          draw_stat_pill(stat_start_x, row1_y, paste0("RV:", rv_val), profile$rv100, 0, TRUE, 0.058)
          
          # wOBA
          woba_val <- if(!is.na(profile$woba)) sprintf(".%03d", round(profile$woba * 1000)) else "-"
          draw_stat_pill(stat_start_x + stat_spacing, row1_y, paste0("wOBA:", woba_val), profile$woba, 0.320, TRUE, 0.068)
          
          # EV90
          ev90_val <- if(!is.na(profile$ev90)) sprintf("%.0f", profile$ev90) else "-"
          draw_stat_pill(stat_start_x + stat_spacing * 2, row1_y, paste0("EV90:", ev90_val), profile$ev90, 95, TRUE, 0.058)
          
          # K%
          k_val <- if(!is.na(profile$k_pct)) sprintf("%.0f%%", profile$k_pct) else "-"
          draw_stat_pill(stat_start_x + stat_spacing * 3, row1_y, paste0("K:", k_val), profile$k_pct, 22, FALSE, 0.052)
          
          # BB%
          bb_val <- if(!is.na(profile$bb_pct)) sprintf("%.0f%%", profile$bb_pct) else "-"
          draw_stat_pill(stat_start_x + stat_spacing * 4, row1_y, paste0("BB:", bb_val), profile$bb_pct, 9, TRUE, 0.052)
          
          # Whiff% and Chase% - more useful stats
          whiff_val <- if(!is.na(profile$whiff_pct)) sprintf("%.0f%%", profile$whiff_pct) else "-"
          draw_stat_pill(stat_start_x + stat_spacing * 5, row1_y, paste0("Wh:", whiff_val), profile$whiff_pct, 25, FALSE, 0.052)
          
          # Spray tendencies with colored pills - moved right
          spray_x <- stat_start_x + stat_spacing * 6.1
          pull_val <- if(!is.na(profile$pull_pct)) sprintf("%.0f", profile$pull_pct) else "-"
          draw_stat_pill(spray_x, row1_y, paste0("P:", pull_val), profile$pull_pct, 40, TRUE, 0.038)
          mid_val <- if(!is.na(profile$middle_pct)) sprintf("%.0f", profile$middle_pct) else "-"
          draw_stat_pill(spray_x + 0.042, row1_y, paste0("M:", mid_val), profile$middle_pct, 38, TRUE, 0.038)
          oppo_val <- if(!is.na(profile$oppo_pct)) sprintf("%.0f", profile$oppo_pct) else "-"
          draw_stat_pill(spray_x + 0.084, row1_y, paste0("O:", oppo_val), profile$oppo_pct, 22, TRUE, 0.038)
          
          # Row 2: Notes (LHP, RHP, Overall only - no pitch plan box) - about 0.032 height
          row2_y <- row_top - 0.044
          
          # Notes background
          grid::grid.rect(x = 0.5, y = row2_y, width = 0.96, height = 0.032,
                         gp = grid::gpar(fill = "white", col = "gray80", lwd = 0.3))
          
          # Format notes with colored text - LHP (red), RHP (black), Overall (blue)
          # Smaller font (6pt) to fit more words, increased character limits
          note_x <- 0.025
          
          if (nchar(notes$lhp_plan) > 0) {
            lhp_text <- paste0("LHP: ", substr(notes$lhp_plan, 1, 55))
            grid::grid.text(lhp_text, x = note_x, y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 6, col = "#C62828", fontface = "bold"))
            note_x <- note_x + 0.32
          }
          
          if (nchar(notes$rhp_plan) > 0) {
            rhp_text <- paste0("RHP: ", substr(notes$rhp_plan, 1, 55))
            grid::grid.text(rhp_text, x = note_x, y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 6, col = "black", fontface = "bold"))
            note_x <- note_x + 0.32
          }
          
          if (nchar(notes$overall_notes) > 0) {
            overall_text <- paste0("Notes: ", substr(notes$overall_notes, 1, 50))
            grid::grid.text(overall_text, x = note_x, y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 6, col = "#1565C0", fontface = "bold"))
          }
          
          if (nchar(notes$lhp_plan) == 0 && nchar(notes$rhp_plan) == 0 && nchar(notes$overall_notes) == 0) {
            grid::grid.text("(add scouting notes)", x = 0.025, y = row2_y, just = "left",
                           gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
          }
          
          # Row 3: 5 Diamonds with pitcher spots and in-game notes area - moved even lower
          row3_y <- row_top - 0.095
          diamond_size <- 0.014  # Shortened diamonds
          diamond_spacing <- 0.088  # Tighter spacing to make room for bigger notes box
          
          # Draw 5 diamonds (no inning labels)
          for (d in 1:5) {
            dx <- 0.06 + (d - 1) * diamond_spacing
            draw_diamond(dx, row3_y, diamond_size)
            
            # Pitcher name line (above diamond only - no inning label below)
            grid::grid.text("P:_______", x = dx, y = row3_y + diamond_size + 0.010, 
                           gp = grid::gpar(fontsize = 5, col = "gray40"))
          }
          
          # In-game notes area (to the right of diamonds) - BIGGER box
          notes_box_x <- 0.50
          notes_box_width <- 0.46  # Increased width
          notes_box_height <- diamond_size * 3.2  # Increased height
          grid::grid.rect(x = notes_box_x + notes_box_width/2, y = row3_y, 
                         width = notes_box_width, height = notes_box_height,
                         gp = grid::gpar(fill = "#fafafa", col = "gray60", lwd = 0.3))
          grid::grid.text("In-Game Notes:", x = notes_box_x + 0.01, y = row3_y + notes_box_height/2 - 0.005, just = "left",
                         gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
          # Lines for writing - more lines in bigger box
          for (line in 1:4) {
            line_y <- row3_y + notes_box_height/2 - 0.012 - (line - 1) * 0.011
            grid::grid.lines(x = c(notes_box_x + 0.01, notes_box_x + notes_box_width - 0.01), y = c(line_y, line_y),
                            gp = grid::gpar(col = "gray80", lwd = 0.3, lty = "dotted"))
          }
          
          # Separator line between hitters
          if (h_idx < length(page_hitters)) {
            sep_y <- row_top - hitter_row_height + 0.008
            grid::grid.lines(x = c(0.02, 0.98), y = c(sep_y, sep_y),
                            gp = grid::gpar(col = "#006F71", lwd = 0.5, lty = "dashed"))
          }
        }
        
        # Footer
        grid::grid.text("Generated by Hitter Scouting Cards App", x = 0.5, y = 0.015,
                       gp = grid::gpar(fontsize = 6, col = "gray50", fontface = "italic"))
      }
      
      dev.off()
    }
  )
  
  # ============================================================================
  # MATCHUP MATRICES SERVER LOGIC
  # ============================================================================
  
  # Generate Overall Matchup Matrix
  observeEvent(input$generate_matrix, {
    req(input$matrix_pitchers, input$matrix_hitters)
    
    pitchers <- input$matrix_pitchers
    hitters <- input$matrix_hitters
    
    if (length(pitchers) == 0 || length(hitters) == 0) {
      showNotification("Please select at least one pitcher and one hitter", type = "warning")
      return()
    }
    
    # Calculate matchup matrix
    matrix_results <- matrix(NA, nrow = length(hitters), ncol = length(pitchers))
    rownames(matrix_results) <- hitters
    colnames(matrix_results) <- pitchers
    
    withProgress(message = "Calculating matchups...", value = 0, {
      total <- length(pitchers) * length(hitters)
      counter <- 0
      
      for (p_idx in seq_along(pitchers)) {
        for (h_idx in seq_along(hitters)) {
          result <- calculate_mac_matchup(pitchers[p_idx], hitters[h_idx])
          matrix_results[h_idx, p_idx] <- result$score
          counter <- counter + 1
          incProgress(1/total)
        }
      }
    })
    
    matrix_data(list(matrix = matrix_results, pitchers = pitchers, hitters = hitters))
  })
  
  # Render Overall Matchup Matrix
  output$overall_matrix_output <- renderUI({
    data <- matrix_data()
    if (is.null(data)) {
      return(div(style = "text-align: center; padding: 40px; color: #666;",
                 p("Select pitchers and hitters, then click 'Generate Matrix' to see matchup scores.")))
    }
    
    matrix <- data$matrix
    pitchers <- data$pitchers
    hitters <- data$hitters
    
    # Build HTML table
    header_cells <- lapply(pitchers, function(p) {
      tags$th(style = "padding: 8px; background: #006F71; color: white; font-size: 11px; min-width: 80px;", 
              substr(p, 1, 15))
    })
    
    table_rows <- lapply(seq_along(hitters), function(h_idx) {
      h_name <- hitters[h_idx]
      
      score_cells <- lapply(seq_along(pitchers), function(p_idx) {
        score <- matrix[h_idx, p_idx]
        bg_color <- get_matchup_score_color(score)
        text_color <- if(score >= 55) "#2E7D32" else if(score >= 45) "#F57F17" else "#C62828"
        
        tags$td(style = paste0("padding: 8px; text-align: center; background: ", bg_color, 
                               "; font-weight: bold; font-size: 14px; color: ", text_color, ";"),
                if(is.na(score)) "-" else score)
      })
      
      tags$tr(
        tags$td(style = "padding: 8px; background: #f5f5f5; font-weight: bold; font-size: 11px;", 
                substr(h_name, 1, 15)),
        score_cells
      )
    })
    
    tags$table(style = "border-collapse: collapse; width: 100%; margin-top: 10px;",
               tags$thead(tags$tr(tags$th(style = "padding: 8px; background: #37474F; color: white;", "Hitter \\ Pitcher"), header_cells)),
               tags$tbody(table_rows))
  })
  
  # Generate Pitch Type Matrix
  observeEvent(input$generate_pt_matrix, {
    req(input$pt_matrix_pitcher, input$pt_matrix_hitters)
    
    pitcher <- input$pt_matrix_pitcher
    hitters <- input$pt_matrix_hitters
    
    if (is.null(pitcher) || length(hitters) == 0) {
      showNotification("Please select a pitcher and at least one hitter", type = "warning")
      return()
    }
    
    # Calculate pitch type matchup matrix (FB, BB, OS for each hitter)
    pitch_types <- c("FB", "BB", "OS")
    matrix_results <- matrix(NA, nrow = length(hitters), ncol = length(pitch_types))
    rownames(matrix_results) <- hitters
    colnames(matrix_results) <- pitch_types
    
    # Also calculate overall scores
    overall_scores <- numeric(length(hitters))
    
    withProgress(message = "Calculating pitch type matchups...", value = 0, {
      total <- length(hitters)
      
      for (h_idx in seq_along(hitters)) {
        # Overall score
        overall_result <- calculate_mac_matchup(pitcher, hitters[h_idx])
        overall_scores[h_idx] <- overall_result$score
        
        # By pitch type
        for (pt_idx in seq_along(pitch_types)) {
          result <- calculate_pitch_type_matchup(pitcher, hitters[h_idx], pitch_types[pt_idx])
          matrix_results[h_idx, pt_idx] <- result$score
        }
        incProgress(1/total)
      }
    })
    
    pt_matrix_data(list(matrix = matrix_results, pitcher = pitcher, hitters = hitters, 
                        overall = overall_scores, pitch_types = pitch_types))
  })
  
  # Render Pitch Type Matrix
  output$pitch_type_matrix_output <- renderUI({
    data <- pt_matrix_data()
    if (is.null(data)) {
      return(div(style = "text-align: center; padding: 40px; color: #666;",
                 p("Select a pitcher and hitters, then click 'Generate Pitch Type Matrix' to see matchup scores by pitch type.")))
    }
    
    matrix <- data$matrix
    pitcher <- data$pitcher
    hitters <- data$hitters
    overall <- data$overall
    pitch_types <- data$pitch_types
    pitch_labels <- c("FB" = "Fastballs", "BB" = "Breaking", "OS" = "Offspeed")
    
    # Header
    header_cells <- c(
      list(tags$th(style = "padding: 8px; background: #6A1B9A; color: white; font-size: 11px;", "Overall")),
      lapply(pitch_types, function(pt) {
        tags$th(style = "padding: 8px; background: #006F71; color: white; font-size: 11px;", pitch_labels[pt])
      })
    )
    
    table_rows <- lapply(seq_along(hitters), function(h_idx) {
      h_name <- hitters[h_idx]
      
      # Overall cell
      overall_score <- overall[h_idx]
      overall_bg <- get_matchup_score_color(overall_score)
      overall_text <- if(overall_score >= 55) "#2E7D32" else if(overall_score >= 45) "#F57F17" else "#C62828"
      
      overall_cell <- tags$td(style = paste0("padding: 8px; text-align: center; background: ", overall_bg, 
                                             "; font-weight: bold; font-size: 14px; color: ", overall_text, ";"),
                              if(is.na(overall_score)) "-" else overall_score)
      
      # Pitch type cells
      pt_cells <- lapply(seq_along(pitch_types), function(pt_idx) {
        score <- matrix[h_idx, pt_idx]
        bg_color <- get_matchup_score_color(score)
        text_color <- if(score >= 55) "#2E7D32" else if(score >= 45) "#F57F17" else "#C62828"
        
        tags$td(style = paste0("padding: 8px; text-align: center; background: ", bg_color, 
                               "; font-weight: bold; font-size: 14px; color: ", text_color, ";"),
                if(is.na(score)) "-" else score)
      })
      
      tags$tr(
        tags$td(style = "padding: 8px; background: #f5f5f5; font-weight: bold; font-size: 11px;", 
                substr(h_name, 1, 15)),
        overall_cell,
        pt_cells
      )
    })
    
    tagList(
      div(style = "margin-bottom: 10px; font-weight: bold; color: #006F71;",
          paste0("Pitcher: ", pitcher)),
      tags$table(style = "border-collapse: collapse; width: 100%; margin-top: 10px;",
                 tags$thead(tags$tr(tags$th(style = "padding: 8px; background: #37474F; color: white;", "Hitter"), header_cells)),
                 tags$tbody(table_rows))
    )
  })
}

shinyApp(ui = ui, server = server)
