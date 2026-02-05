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
library(gt)
library(base64enc)

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
  # Pitch movement data (for matchup matrices)
  "RelSpeed", "InducedVertBreak", "HorzBreak",
  # Pre-calculated metrics (from TM2025)
  "mean_DRE_bat", "mean_DRE_pit", "woba", "wobacon", "slg",
  "is_put_away", "is_walk", "is_ab", "is_hit",
  "in_zone", "in_zone_whiff", "is_whiff", "is_swing", "chase", "is_pa", "is_k"
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
  if (!"RelSpeed" %in% names(df)) {
    df <- df %>% mutate(RelSpeed = NA_real_)
  }
  if (!"InducedVertBreak" %in% names(df)) {
    df <- df %>% mutate(InducedVertBreak = NA_real_)
  }
  if (!"HorzBreak" %in% names(df)) {
    df <- df %>% mutate(HorzBreak = NA_real_)
  }
  if (!"mean_DRE_bat" %in% names(df)) {
    df <- df %>% mutate(mean_DRE_bat = 0)
  }
  if (!"mean_DRE_pit" %in% names(df)) {
    df <- df %>% mutate(mean_DRE_pit = NA_real_)
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

all_hitters <- sort(unique(tm_data$Batter))
all_pitchers <- sort(unique(tm_data$Pitcher))

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
# MATCHUP MATRIX CONFIGURATIONS
# ============================================================================

MM_SPLIT_CONFIG <- list(
  ride_fb_rhp = list(name = "Ride FB (RHP)", pitch_types = c("Fastball", "Four-Seam"), ivb_min = 17, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 80),
  ride_fb_lhp = list(name = "Ride FB (LHP)", pitch_types = c("Fastball", "Four-Seam"), ivb_min = 17, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 80),
  sink_fb_rhp = list(name = "Sink FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_max = 12, hb_min = 10, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 60),
  sink_fb_lhp = list(name = "Sink FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_max = 12, hb_min = 10, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 60),
  hard_velo_rhp = list(name = "Hard Velo (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_min = 92, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 80),
  hard_velo_lhp = list(name = "Hard Velo (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_min = 92, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 80),
  sweep_rhp = list(name = "Sweep (RHP)", pitch_types = c("Slider", "Sweeper", "Curveball"), hb_min = 14, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "bb_rhp", threshold = 60),
  sweep_lhp = list(name = "Sweep (LHP)", pitch_types = c("Slider", "Sweeper", "Curveball"), hb_min = 14, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "bb_lhp", threshold = 60),
  downer_rhp = list(name = "Downer (RHP)", pitch_types = c("Slider", "Curveball", "Sweeper"), ivb_max = -6, pitcher_hand = "Right", fallback = "bb_rhp", threshold = 60),
  downer_lhp = list(name = "Downer (LHP)", pitch_types = c("Slider", "Curveball", "Sweeper"), ivb_max = -6, pitcher_hand = "Left", fallback = "bb_lhp", threshold = 60),
  gyro_cut_rhp = list(name = "Gyro/Cut (RHP)", pitch_types = c("Cutter", "Slider"), ivb_min = -3, ivb_max = 6, hb_max = 8, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "bb_rhp", threshold = 50),
  gyro_cut_lhp = list(name = "Gyro/Cut (LHP)", pitch_types = c("Cutter", "Slider"), ivb_min = -3, ivb_max = 6, hb_max = 8, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "bb_lhp", threshold = 50),
  chspl_rhp = list(name = "CH/SPL (RHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter"), pitcher_hand = "Right", fallback = "os_rhp", threshold = 60),
  chspl_lhp = list(name = "CH/SPL (LHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter"), pitcher_hand = "Left", fallback = "os_lhp", threshold = 60)
)

MM_FAMILY_CONFIG <- list(
  fb_rhp = list(name = "All FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker", "Cutter"), pitcher_hand = "Right", fallback = "vs_rhp", threshold = 100),
  fb_lhp = list(name = "All FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker", "Cutter"), pitcher_hand = "Left", fallback = "vs_lhp", threshold = 100),
  bb_rhp = list(name = "All BB (RHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Cutter"), pitcher_hand = "Right", fallback = "vs_rhp", threshold = 80),
  bb_lhp = list(name = "All BB (LHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Cutter"), pitcher_hand = "Left", fallback = "vs_lhp", threshold = 80),
  os_rhp = list(name = "All OS (RHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter"), pitcher_hand = "Right", fallback = "vs_rhp", threshold = 60),
  os_lhp = list(name = "All OS (LHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter"), pitcher_hand = "Left", fallback = "vs_lhp", threshold = 60)
)

MM_PLATOON_CONFIG <- list(
  vs_rhp = list(name = "vs RHP", pitch_types = NULL, pitcher_hand = "Right", fallback = "overall", threshold = 150),
  vs_lhp = list(name = "vs LHP", pitch_types = NULL, pitcher_hand = "Left", fallback = "overall", threshold = 150)
)

MM_OVERALL_CONFIG <- list(
  overall = list(name = "Overall", pitch_types = NULL, pitcher_hand = NULL, fallback = NULL, threshold = 200)
)

MM_ALL_SPLITS <- c(MM_SPLIT_CONFIG, MM_FAMILY_CONFIG, MM_PLATOON_CONFIG, MM_OVERALL_CONFIG)

MM_DISPLAY_GROUPS <- list(
  "Movement (RHP)" = c("ride_fb_rhp", "sink_fb_rhp", "hard_velo_rhp", "sweep_rhp", "downer_rhp", "gyro_cut_rhp", "chspl_rhp"),
  "Movement (LHP)" = c("ride_fb_lhp", "sink_fb_lhp", "hard_velo_lhp", "sweep_lhp", "downer_lhp", "gyro_cut_lhp", "chspl_lhp"),
  "Pitch Family (RHP)" = c("fb_rhp", "bb_rhp", "os_rhp"),
  "Pitch Family (LHP)" = c("fb_lhp", "bb_lhp", "os_lhp"),
  "Platoon" = c("vs_rhp", "vs_lhp"),
  "Overall" = c("overall")
)

# ============================================================================
# MATCHUP MATRIX CORE FUNCTIONS
# ============================================================================

mm_fast_filter <- function(df, config, is_pitcher = FALSE) {
  if (nrow(df) == 0) return(df)
  mask <- rep(TRUE, nrow(df))
  if (!is.null(config$pitch_types)) mask <- mask & df$TaggedPitchType %in% config$pitch_types
  if (!is.null(config$pitcher_hand)) {
    if (is_pitcher) {
      batter_side <- ifelse(config$pitcher_hand == "Right", "Right", "Left")
      if ("BatterSide" %in% names(df)) {
        mask <- mask & df$BatterSide == batter_side
      }
    } else {
      mask <- mask & df$PitcherThrows == config$pitcher_hand
    }
  }
  if (!is.null(config$ivb_min)) mask <- mask & !is.na(df$InducedVertBreak) & df$InducedVertBreak >= config$ivb_min
  if (!is.null(config$ivb_max)) mask <- mask & !is.na(df$InducedVertBreak) & df$InducedVertBreak <= config$ivb_max
  hb <- if (!is.null(config$use_abs_hb) && config$use_abs_hb) abs(df$HorzBreak) else df$HorzBreak
  if (!is.null(config$hb_min)) mask <- mask & !is.na(hb) & hb >= config$hb_min
  if (!is.null(config$hb_max)) mask <- mask & !is.na(hb) & hb <= config$hb_max
  if (!is.null(config$velo_min)) mask <- mask & !is.na(df$RelSpeed) & df$RelSpeed >= config$velo_min
  df[mask, , drop = FALSE]
}

mm_calc_rv_stats <- function(df, rv_col) {
  rv <- df[[rv_col]][!is.na(df[[rv_col]])]
  n <- length(rv)
  if (n == 0) return(list(rv100 = NA_real_, n = 0L))
  list(rv100 = 100 * mean(rv), n = n)
}

mm_calc_hierarchical_rv <- function(player_data, rv_col, split_name, is_pitcher = FALSE) {
  config <- MM_ALL_SPLITS[[split_name]]
  if (is.null(config)) return(list(rv100 = 0, n = 0, weight = 0, raw_rv100 = NA_real_))
  filtered <- mm_fast_filter(player_data, config, is_pitcher)
  stats <- mm_calc_rv_stats(filtered, rv_col)
  weight <- min(stats$n / config$threshold, 1)
  if (weight >= 1 || is.null(config$fallback)) {
    return(list(rv100 = ifelse(is.na(stats$rv100), 0, stats$rv100), n = stats$n, weight = weight, raw_rv100 = stats$rv100))
  }
  fallback <- mm_calc_hierarchical_rv(player_data, rv_col, config$fallback, is_pitcher)
  this_rv <- ifelse(is.na(stats$rv100), 0, stats$rv100)
  blended <- weight * this_rv + (1 - weight) * fallback$rv100
  list(rv100 = blended, n = stats$n, weight = weight, raw_rv100 = stats$rv100)
}

mm_calc_all_splits <- function(player_data, rv_col, is_pitcher = FALSE) {
  results <- lapply(names(MM_ALL_SPLITS), function(s) {
    res <- mm_calc_hierarchical_rv(player_data, rv_col, s, is_pitcher)
    res$name <- MM_ALL_SPLITS[[s]]$name
    res
  })
  names(results) <- names(MM_ALL_SPLITS)
  results
}

mm_calc_matchup <- function(h_rv, p_rv, h_w, p_w) {
  combined_conf <- h_w * p_w
  raw <- h_rv - p_rv
  weighted <- combined_conf * raw + (1 - combined_conf) * 0
  list(raw = raw, weighted = weighted, confidence = combined_conf)
}

mm_calc_overall_matchup <- function(h_splits, p_splits, p_data) {
  total <- nrow(p_data)
  if (total == 0) return(list(rv = NA_real_, confidence = 0))
  wsum <- 0; csum <- 0; tweight <- 0
  for (s in names(MM_SPLIT_CONFIG)) {
    n_split <- nrow(mm_fast_filter(p_data, MM_SPLIT_CONFIG[[s]], is_pitcher = TRUE))
    usage <- n_split / total
    if (usage > 0) {
      m <- mm_calc_matchup(h_splits[[s]]$rv100, p_splits[[s]]$rv100, h_splits[[s]]$weight, p_splits[[s]]$weight)
      wsum <- wsum + m$weighted * usage
      csum <- csum + m$confidence * usage
      tweight <- tweight + usage
    }
  }
  if (tweight > 0) list(rv = wsum / tweight, confidence = csum / tweight) else list(rv = NA_real_, confidence = 0)
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
      slg = sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      hard_hit_pct = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE) / pmax(1, sum(PitchCall == "InPlay", na.rm = TRUE)) * 100,
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
    
    slg_p20 = quantile(batter_stats$slg, 0.10, na.rm = TRUE),
    slg_p50 = quantile(batter_stats$slg, 0.50, na.rm = TRUE),
    slg_p80 = quantile(batter_stats$slg, 0.90, na.rm = TRUE),
    
    ev90_p20 = quantile(batter_stats$ev90, 0.10, na.rm = TRUE),
    ev90_p50 = quantile(batter_stats$ev90, 0.50, na.rm = TRUE),
    ev90_p80 = quantile(batter_stats$ev90, 0.90, na.rm = TRUE),
    
    hard_hit_p20 = quantile(batter_stats$hard_hit_pct, 0.10, na.rm = TRUE),
    hard_hit_p50 = quantile(batter_stats$hard_hit_pct, 0.50, na.rm = TRUE),
    hard_hit_p80 = quantile(batter_stats$hard_hit_pct, 0.90, na.rm = TRUE),
    
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
      slg = sum(totalbases, na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      # Game production metrics
      hr_count = sum(PlayResult == "HomeRun", na.rm = TRUE),
      xbh_count = sum(PlayResult %in% c("Double", "Triple", "HomeRun"), na.rm = TRUE),
      n_ab = sum(ABindicator, na.rm = TRUE),
      hr_per_ab = sum(PlayResult == "HomeRun", na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      xbh_per_ab = sum(PlayResult %in% c("Double", "Triple", "HomeRun"), na.rm = TRUE) / pmax(1, sum(ABindicator, na.rm = TRUE)),
      hard_hit_pct = sum(PitchCall == "InPlay" & ExitSpeed >= 95, na.rm = TRUE) / pmax(1, sum(PitchCall == "InPlay", na.rm = TRUE)) * 100,
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
      
      # Game Power: Combines actual game production (SLG, ISO) with raw power indicators (EV90, HH%)
      # This captures ACTUAL power production, not just raw tools
      iso_grade = value_to_grade(iso, grade_metrics$iso_p20, grade_metrics$iso_p50, grade_metrics$iso_p80),
      slg_grade = value_to_grade(slg, grade_metrics$slg_p20, grade_metrics$slg_p50, grade_metrics$slg_p80),
      ev90_grade = value_to_grade(ev90, grade_metrics$ev90_p20, grade_metrics$ev90_p50, grade_metrics$ev90_p80),
      hard_hit_grade = value_to_grade(hard_hit_pct, grade_metrics$hard_hit_p20, grade_metrics$hard_hit_p50, grade_metrics$hard_hit_p80),
      # Game Power = 40% SLG + 30% ISO + 15% EV90 + 15% HH% (weighted toward actual production)
      game_power = round(0.40 * slg_grade + 0.30 * iso_grade + 0.15 * ev90_grade + 0.15 * hard_hit_grade),
      
      # Raw Power: EV90 only (pure bat speed/strength)
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

# Helper function to format name as "F. LastName" (first initial + last name)
format_short_name <- function(full_name) {
  if (is.null(full_name) || is.na(full_name) || nchar(full_name) == 0) return("")
  parts <- strsplit(trimws(full_name), "\\s+")[[1]]
  if (length(parts) == 1) return(full_name)
  # Take first letter of first name and full last name
  first_initial <- substr(parts[1], 1, 1)
  last_name <- paste(parts[-1], collapse = " ")
  paste0(first_initial, ". ", last_name)
}

# Helper function to get abbreviated batter hand: (R), (L), (B) for switch hitters
format_batter_hand <- function(hand) {
  if (is.null(hand) || is.na(hand)) return("")
  if (hand == "Switch") return("(B)")
  if (hand == "Left") return("(L)")
  if (hand == "Right") return("(R)")
  paste0("(", substr(hand, 1, 1), ")")
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
       overall_grade = overall_grade, game_power_grade = power_grade, raw_power_grade = raw_power_grade,
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
# ADVANCED HEAT MAP GRID - Shows heatmaps in grid with stat/sample in each cell
# Rows: Pitch types or Count types
# Columns: Stats (Pitch Freq, Swing%, wOBA, Damage, Chase, Whiff, IZ Whiff, IZ Damage)
# ============================================================================

# Create a single heatmap cell with stat value and sample size overlaid
create_heatmap_cell <- function(data, stat_type = "whiff", stat_value = NA, n_sample = 0) {
  if (is.null(data) || nrow(data) < 3) {
    # Return minimal zone with stat overlay
    stat_text <- if (!is.na(stat_value)) {
      if (abs(stat_value) < 1 && stat_value != 0) sprintf("%.3f", stat_value) else sprintf("%.1f", stat_value)
    } else "-"
    
    p <- ggplot() +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = "#f0f0f0", color = "black", linewidth = 0.5) +
      annotate("rect", xmin = -0.6, xmax = 0.6, ymin = 2.1, ymax = 2.9, fill = "white", color = NA, alpha = 0.9) +
      annotate("text", x = 0, y = 2.5, label = stat_text, size = 4, fontface = "bold", color = "#333") +
      annotate("text", x = 0, y = 1.2, label = paste0("n=", n_sample), size = 2.5, color = "gray40") +
      coord_fixed(xlim = c(-1.5, 1.5), ylim = c(0.5, 4)) + 
      theme_void() + 
      theme(plot.margin = margin(2, 2, 2, 2))
    return(p)
  }
  
  # Filter data based on stat type
  plot_data <- switch(stat_type,
    "whiff" = data %>% filter(is_whiff == 1),
    "chase" = data %>% filter(chase == 1),
    "damage" = data %>% filter((is_hit == 1) | (PitchCall == "InPlay" & ExitSpeed >= 95)),
    "swing" = data %>% filter(is_swing == 1),
    "iz_whiff" = data %>% filter(in_zone_whiff == 1),
    "iz_damage" = data %>% filter(in_zone == 1, (is_hit == 1) | (PitchCall == "InPlay" & ExitSpeed >= 95)),
    "pitch_freq" = data,  # All pitches
    "woba" = data %>% filter(!is.na(woba)),
    data)
  
  plot_data <- plot_data %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
  
  # Format stat display
  stat_text <- if (!is.na(stat_value)) {
    if (abs(stat_value) < 1 && stat_value != 0) sprintf("%.3f", stat_value) else sprintf("%.1f", stat_value)
  } else "-"
  
  # Color scheme based on stat type
  colors <- switch(stat_type,
    "whiff" = c("white", "#FFCCCC", "#FF6666", "#CC0000"),
    "chase" = c("white", "#FFE0B2", "#FF9800", "#E65100"),
    "damage" = c("white", "#C8E6C9", "#4CAF50", "#1B5E20"),
    "swing" = c("white", "#BBDEFB", "#2196F3", "#0D47A1"),
    "iz_whiff" = c("white", "#E1BEE7", "#9C27B0", "#4A148C"),
    "iz_damage" = c("white", "#B2DFDB", "#009688", "#004D40"),
    "woba" = c("white", "#FFF9C4", "#FFEB3B", "#F57F17"),
    c("white", "#E0E0E0", "#9E9E9E", "#424242"))  # default gray
  
  if (nrow(plot_data) < 3) {
    p <- ggplot() +
      annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = "#f0f0f0", color = "black", linewidth = 0.5) +
      annotate("rect", xmin = -0.6, xmax = 0.6, ymin = 2.1, ymax = 2.9, fill = "white", color = NA, alpha = 0.9) +
      annotate("text", x = 0, y = 2.5, label = stat_text, size = 4, fontface = "bold", color = "#333") +
      annotate("text", x = 0, y = 1.2, label = paste0("n=", n_sample), size = 2.5, color = "gray40") +
      coord_fixed(xlim = c(-1.5, 1.5), ylim = c(0.5, 4)) + 
      theme_void() + 
      theme(plot.margin = margin(2, 2, 2, 2))
    return(p)
  }
  
  ggplot(plot_data, aes(x = PlateLocSide, y = PlateLocHeight)) +
    stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, alpha = 0.85) +
    scale_fill_gradientn(colours = colors, guide = "none") +
    annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 0.5) +
    # Stats overlay box
    annotate("rect", xmin = -0.55, xmax = 0.55, ymin = 2.1, ymax = 2.9, fill = "white", color = NA, alpha = 0.85) +
    annotate("text", x = 0, y = 2.5, label = stat_text, size = 4, fontface = "bold", color = "#333") +
    annotate("text", x = 0, y = 1.2, label = paste0("n=", n_sample), size = 2.5, color = "gray40") +
    coord_fixed(xlim = c(-1.5, 1.5), ylim = c(0.5, 4)) +
    theme_void() + 
    theme(legend.position = "none", plot.margin = margin(2, 2, 2, 2))
}

# Create the full heatmap grid (to be rendered as a combined plot)
create_heatmap_grid <- function(h_raw, pitcher_hand, split_type = "pitch_type") {
  # Define stat types and their column names
  stat_types <- c("pitch_freq", "swing", "woba", "damage", "chase", "whiff", "iz_whiff", "iz_damage")
  stat_labels <- c("Pitch Freq", "Swing%", "wOBA", "Damage (95+)", "Chase%", "Whiff%", "IZ Whiff%", "IZ Damage%")
  
  if (split_type == "pitch_type") {
    row_types <- c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl", "Overall")
  } else {
    row_types <- c("1P", "2K", "Ahead", "Behind", "Last 15")
  }
  
  # Filter data by pitcher hand
  h_data <- h_raw %>% filter(PitcherThrows == pitcher_hand)
  
  if (nrow(h_data) < 10) {
    return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Insufficient data", size = 5))
  }
  
  # Get stats from cache
  stats_list <- get_advanced_heatmap_stats(unique(h_raw$Batter)[1], pitcher_hand, split_type)
  
  # Create list to store all plots
  plot_list <- list()
  
  for (r_idx in seq_along(row_types)) {
    row_type <- row_types[r_idx]
    
    # Get data for this row
    if (split_type == "pitch_type") {
      if (row_type == "Overall") {
        row_data <- h_data
      } else {
        row_data <- h_data %>% 
          mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType)) %>%
          filter(DetailedPitchType == row_type)
      }
      # Find stats for this row
      row_stats <- stats_list[sapply(stats_list, function(x) !is.null(x$pitch_type) && x$pitch_type == row_type)]
    } else {
      if (row_type == "Last 15") {
        last15_dates <- get_last_n_games(h_raw, unique(h_raw$Batter)[1], 15)
        row_data <- h_data %>% filter(Date %in% last15_dates)
      } else {
        row_data <- h_data %>%
          mutate(CountType = case_when(
            Balls == 0 & Strikes == 0 ~ "1P",
            Strikes == 2 ~ "2K",
            Strikes > Balls ~ "Ahead",
            Balls > Strikes ~ "Behind",
            TRUE ~ "Even"
          )) %>%
          filter(CountType == row_type)
      }
      # Find stats for this row
      row_stats <- stats_list[sapply(stats_list, function(x) !is.null(x$count_type) && x$count_type == row_type)]
    }
    
    row_stat <- if (length(row_stats) > 0) row_stats[[1]] else list(n = 0)
    n_sample <- if (!is.null(row_stat$n)) row_stat$n else nrow(row_data)
    
    for (s_idx in seq_along(stat_types)) {
      stat_type <- stat_types[s_idx]
      
      # Get stat value
      stat_value <- switch(stat_type,
        "pitch_freq" = n_sample,
        "swing" = row_stat$swing_pct,
        "woba" = row_stat$woba,
        "damage" = row_stat$damage_pct,
        "chase" = row_stat$chase_pct,
        "whiff" = row_stat$whiff_pct,
        "iz_whiff" = row_stat$iz_whiff_pct,
        "iz_damage" = row_stat$iz_damage_pct,
        NA)
      
      # Create cell plot
      cell_plot <- create_heatmap_cell(row_data, stat_type, stat_value, n_sample)
      
      # Add to plot list with position info
      plot_list[[paste0(r_idx, "_", s_idx)]] <- cell_plot
    }
  }
  
  # Return the list of plots (to be arranged in server)
  list(plots = plot_list, rows = row_types, cols = stat_labels)
}

# ============================================================================
# ADVANCED HEAT MAP STATS TABLE GENERATOR
# ============================================================================
get_advanced_heatmap_stats <- function(batter_name, pitcher_hand, split_type = "pitch_type") {
  # Get batter data filtered by pitcher hand (ON-DEMAND, no cache)
  batter_data <- tm_data %>%
    filter(Batter == batter_name, PitcherThrows == pitcher_hand)
  
  if (nrow(batter_data) < 10) return(list())
  
  if (split_type == "pitch_type") {
    pitch_types <- c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl")
    
    batter_data <- batter_data %>%
      mutate(DetailedPitchType = classify_detailed_pitch(TaggedPitchType))
    
    stats_list <- lapply(pitch_types, function(pt) {
      pt_data <- batter_data %>% filter(DetailedPitchType == pt)
      
      if (nrow(pt_data) < 5) {
        return(list(pitch_type = pt, n = nrow(pt_data), pitch_freq = nrow(pt_data), 
                    swing_pct = NA, woba = NA, damage_pct = NA, chase_pct = NA, 
                    whiff_pct = NA, iz_whiff_pct = NA, iz_damage_pct = NA))
      }
      
      n_pitches <- nrow(pt_data)
      n_swings <- sum(pt_data$is_swing, na.rm = TRUE)
      n_whiffs <- sum(pt_data$is_whiff, na.rm = TRUE)
      n_in_zone <- sum(pt_data$in_zone, na.rm = TRUE)
      n_out_zone <- sum(pt_data$out_of_zone, na.rm = TRUE)
      n_chase <- sum(pt_data$chase, na.rm = TRUE)
      n_iz_swing <- sum(pt_data$z_swing, na.rm = TRUE)
      n_iz_whiff <- sum(pt_data$in_zone_whiff, na.rm = TRUE)
      n_bip <- sum(pt_data$PitchCall == "InPlay", na.rm = TRUE)
      n_damage <- sum(pt_data$PitchCall == "InPlay" & pt_data$ExitSpeed >= 95, na.rm = TRUE)
      
      list(pitch_type = pt, n = n_pitches, pitch_freq = n_pitches,
           swing_pct = if(n_pitches > 0) 100 * n_swings / n_pitches else NA,
           woba = mean(pt_data$woba, na.rm = TRUE),
           damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA,
           chase_pct = if(n_out_zone > 0) 100 * n_chase / n_out_zone else NA,
           whiff_pct = if(n_swings > 0) 100 * n_whiffs / n_swings else NA,
           iz_whiff_pct = if(n_iz_swing > 0) 100 * n_iz_whiff / n_iz_swing else NA,
           iz_damage_pct = if(n_in_zone > 0) 100 * n_damage / pmax(1, n_bip) else NA)
    })
    
    # Add overall row
    n_pitches <- nrow(batter_data)
    n_swings <- sum(batter_data$is_swing, na.rm = TRUE)
    n_whiffs <- sum(batter_data$is_whiff, na.rm = TRUE)
    n_in_zone <- sum(batter_data$in_zone, na.rm = TRUE)
    n_out_zone <- sum(batter_data$out_of_zone, na.rm = TRUE)
    n_chase <- sum(batter_data$chase, na.rm = TRUE)
    n_iz_swing <- sum(batter_data$z_swing, na.rm = TRUE)
    n_iz_whiff <- sum(batter_data$in_zone_whiff, na.rm = TRUE)
    n_bip <- sum(batter_data$PitchCall == "InPlay", na.rm = TRUE)
    n_damage <- sum(batter_data$PitchCall == "InPlay" & batter_data$ExitSpeed >= 95, na.rm = TRUE)
    
    stats_list[[length(stats_list) + 1]] <- list(pitch_type = "Overall", n = n_pitches, pitch_freq = n_pitches,
         swing_pct = if(n_pitches > 0) 100 * n_swings / n_pitches else NA,
         woba = mean(batter_data$woba, na.rm = TRUE),
         damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA,
         chase_pct = if(n_out_zone > 0) 100 * n_chase / n_out_zone else NA,
         whiff_pct = if(n_swings > 0) 100 * n_whiffs / n_swings else NA,
         iz_whiff_pct = if(n_iz_swing > 0) 100 * n_iz_whiff / n_iz_swing else NA,
         iz_damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA)
    
    return(stats_list)
    
  } else if (split_type == "count_type") {
    count_types <- c("1P", "2K", "Ahead", "Behind")
    
    batter_data <- batter_data %>%
      mutate(CountType = case_when(
        Balls == 0 & Strikes == 0 ~ "1P",
        Strikes == 2 ~ "2K",
        Strikes > Balls ~ "Ahead",
        Balls > Strikes ~ "Behind",
        TRUE ~ "Even"
      ))
    
    stats_list <- lapply(count_types, function(ct) {
      ct_data <- batter_data %>% filter(CountType == ct)
      if (nrow(ct_data) < 5) {
        return(list(count_type = ct, n = nrow(ct_data), pitch_freq = nrow(ct_data),
                    swing_pct = NA, woba = NA, damage_pct = NA, chase_pct = NA,
                    whiff_pct = NA, iz_whiff_pct = NA, iz_damage_pct = NA))
      }
      
      n_pitches <- nrow(ct_data)
      n_swings <- sum(ct_data$is_swing, na.rm = TRUE)
      n_whiffs <- sum(ct_data$is_whiff, na.rm = TRUE)
      n_in_zone <- sum(ct_data$in_zone, na.rm = TRUE)
      n_out_zone <- sum(ct_data$out_of_zone, na.rm = TRUE)
      n_chase <- sum(ct_data$chase, na.rm = TRUE)
      n_iz_swing <- sum(ct_data$z_swing, na.rm = TRUE)
      n_iz_whiff <- sum(ct_data$in_zone_whiff, na.rm = TRUE)
      n_bip <- sum(ct_data$PitchCall == "InPlay", na.rm = TRUE)
      n_damage <- sum(ct_data$PitchCall == "InPlay" & ct_data$ExitSpeed >= 95, na.rm = TRUE)
      
      list(count_type = ct, n = n_pitches, pitch_freq = n_pitches,
           swing_pct = if(n_pitches > 0) 100 * n_swings / n_pitches else NA,
           woba = mean(ct_data$woba, na.rm = TRUE),
           damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA,
           chase_pct = if(n_out_zone > 0) 100 * n_chase / n_out_zone else NA,
           whiff_pct = if(n_swings > 0) 100 * n_whiffs / n_swings else NA,
           iz_whiff_pct = if(n_iz_swing > 0) 100 * n_iz_whiff / n_iz_swing else NA,
           iz_damage_pct = if(n_in_zone > 0) 100 * n_damage / pmax(1, n_bip) else NA)
    })
    
    # Add Last 15 row
    last15_dates <- get_last_n_games(tm_data, batter_name, 15)
    last15_data <- batter_data %>% filter(Date %in% last15_dates)
    
    if (nrow(last15_data) >= 5) {
      n_pitches <- nrow(last15_data)
      n_swings <- sum(last15_data$is_swing, na.rm = TRUE)
      n_whiffs <- sum(last15_data$is_whiff, na.rm = TRUE)
      n_in_zone <- sum(last15_data$in_zone, na.rm = TRUE)
      n_out_zone <- sum(last15_data$out_of_zone, na.rm = TRUE)
      n_chase <- sum(last15_data$chase, na.rm = TRUE)
      n_iz_swing <- sum(last15_data$z_swing, na.rm = TRUE)
      n_iz_whiff <- sum(last15_data$in_zone_whiff, na.rm = TRUE)
      n_bip <- sum(last15_data$PitchCall == "InPlay", na.rm = TRUE)
      n_damage <- sum(last15_data$PitchCall == "InPlay" & last15_data$ExitSpeed >= 95, na.rm = TRUE)
      
      stats_list[[length(stats_list) + 1]] <- list(count_type = "Last 15", n = n_pitches, pitch_freq = n_pitches,
           swing_pct = if(n_pitches > 0) 100 * n_swings / n_pitches else NA,
           woba = mean(last15_data$woba, na.rm = TRUE),
           damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA,
           chase_pct = if(n_out_zone > 0) 100 * n_chase / n_out_zone else NA,
           whiff_pct = if(n_swings > 0) 100 * n_whiffs / n_swings else NA,
           iz_whiff_pct = if(n_iz_swing > 0) 100 * n_iz_whiff / n_iz_swing else NA,
           iz_damage_pct = if(n_bip > 0) 100 * n_damage / n_bip else NA)
    } else {
      stats_list[[length(stats_list) + 1]] <- list(count_type = "Last 15", n = 0, pitch_freq = 0,
           swing_pct = NA, woba = NA, damage_pct = NA, chase_pct = NA,
           whiff_pct = NA, iz_whiff_pct = NA, iz_damage_pct = NA)
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

  /* Matchup Matrices Tab Styles */
  .mm-controls-card {
    background: white; border-radius: 12px; padding: 20px; margin-bottom: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05); border: 1px solid rgba(0,111,113,.2);
  }
  .mm-controls-card label { font-weight: 700; color: #006F71; font-size: 0.85rem; }
  .mm-status-bar {
    background: #e8f5f4; border-radius: 8px; padding: 10px 16px;
    font-size: 0.85rem; color: #006F71; margin-bottom: 15px;
  }
  .mm-results-card {
    background: white; border-radius: 12px; padding: 20px; margin-bottom: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05); border: 1px solid rgba(0,111,113,.2);
  }
  .mm-results-card h3 {
    color: #006F71; font-weight: 800; font-size: 1.1rem;
    border-bottom: 2px solid #006F71; padding-bottom: 8px; margin: 0 0 16px;
  }
  .mm-empty-state {
    text-align: center; padding: 60px 20px; color: #666;
  }
  .mm-empty-state .icon { font-size: 3rem; color: #006F71; margin-bottom: 16px; }
  .btn-mm-analyze {
    background: linear-gradient(135deg, #006F71, #004d4e); color: #fff;
    border: none; border-radius: 8px; padding: 12px 28px; font-weight: 700;
    font-size: 0.95rem; cursor: pointer; transition: all 0.2s;
  }
  .btn-mm-analyze:hover { transform: translateY(-2px); box-shadow: 0 6px 16px rgba(0,111,113,0.3); }
  .btn-mm-download {
    background: linear-gradient(135deg, #CD853F, #8b6914);
    color: #fff; border: none; border-radius: 6px;
    padding: 8px 16px; font-weight: 600; font-size: 0.85rem;
    cursor: pointer; transition: all 0.2s;
  }
  .btn-mm-download:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(205, 133, 63, 0.3);
  }
  .mm-download-row { display: flex; gap: 10px; flex-wrap: wrap; }
  .mm-main-tabs > .nav-pills > li > a {
    color: #006F71; font-weight: 700; font-size: 15px;
    border: 2px solid transparent; border-radius: 25px;
    padding: 8px 20px; transition: all 0.2s;
  }
  .mm-main-tabs > .nav-pills > li > a:hover {
    background: #e8f5f4; border-color: #006F71;
  }
  .mm-main-tabs > .nav-pills > li.active > a {
    background: #006F71 !important; color: white !important;
    border-color: #006F71;
  }
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
      div(div(class = "header-title", "Coastal Carolina Baseball Analytics"), div(class = "header-subtitle", "Opposing Hitter Scouting & Matchup Analysis"))),
  
  div(class = "mm-main-tabs",
    tabsetPanel(id = "main_tabs", type = "pills",
      
      # ====== TAB 1: SCOUTING CARDS (existing functionality) ======
      tabPanel("Scouting Cards",
        div(style = "padding-top: 15px;",
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
        )
      ),
      
      # ====== TAB 2: MATCHUP MATRICES ======
      tabPanel("Matchup Matrices",
        div(style = "padding: 15px; max-width: 1400px; margin: 0 auto;",
          
          # Controls Card
          div(class = "mm-controls-card",
            fluidRow(
              column(4,
                tags$label("Hitters"),
                selectizeInput("mm_hitters", NULL, choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Select hitters...", maxItems = 12))
              ),
              column(4,
                tags$label("Pitchers"),
                selectizeInput("mm_pitchers", NULL, choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Select pitchers...", maxItems = 8))
              ),
              column(2,
                tags$label("View"),
                selectInput("mm_view_mode", NULL, choices = c(
                  "Matrix (Overall)" = "matrix_overall",
                  "Matrix (by Split)" = "matrix_split",
                  "Hitter Profiles" = "hitter_profiles",
                  "Pitcher Profiles" = "pitcher_profiles"
                ))
              ),
              column(2,
                div(style = "margin-top: 22px;",
                  actionButton("mm_analyze", "Analyze", class = "btn-mm-analyze"))
              )
            ),
            
            # Split selection (conditional on matrix_split view)
            conditionalPanel("input.mm_view_mode == 'matrix_split'",
              div(style = "margin-top: 16px;",
                tags$label(style = "font-weight: 700; color: #006F71; font-size: 0.85rem;", "Select Split"),
                selectInput("mm_selected_split", NULL,
                  choices = c(setNames(names(MM_ALL_SPLITS), sapply(MM_ALL_SPLITS, `[[`, "name"))),
                  width = "300px")
              )
            ),
            
            # Split group checkboxes for profiles
            conditionalPanel("input.mm_view_mode == 'hitter_profiles' || input.mm_view_mode == 'pitcher_profiles'",
              div(style = "margin-top: 16px;",
                tags$label(style = "font-weight: 700; color: #006F71; font-size: 0.85rem;", "Display Splits"),
                checkboxGroupInput("mm_split_groups", NULL, inline = TRUE,
                  choices = names(MM_DISPLAY_GROUPS),
                  selected = c("Movement (RHP)", "Movement (LHP)"))
              )
            ),
            
            # PDF Report Options
            div(style = "margin-top: 20px; padding-top: 16px; border-top: 1px solid #e0e0e0;",
              tags$label(style = "font-weight: 700; color: #CD853F; font-size: 0.9rem;", "PDF Report Options"),
              div(style = "margin-top: 12px;",
                textInput("mm_pdf_title", "Report Title", value = "Matchup Matrix Report", width = "100%")
              ),
              div(style = "margin-top: 8px;",
                textAreaInput("mm_pdf_best_matchups", "Best Matchups",
                  placeholder = "e.g., Smith vs Johnson - favorable on breaking balls",
                  rows = 2, width = "100%")
              ),
              div(style = "margin-top: 8px;",
                textAreaInput("mm_pdf_worst_matchups", "Worst Matchups",
                  placeholder = "e.g., Davis vs Miller - struggles with high velo",
                  rows = 2, width = "100%")
              ),
              div(style = "margin-top: 8px;",
                textAreaInput("mm_pdf_notes", "Additional Notes",
                  placeholder = "Game notes, scouting observations, etc.",
                  rows = 3, width = "100%")
              )
            )
          ),
          
          # Status Bar
          div(class = "mm-status-bar", textOutput("mm_status")),
          
          # Results (shown after analyze)
          conditionalPanel("input.mm_analyze > 0",
            div(class = "mm-results-card",
              div(style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
                h3(textOutput("mm_results_title"), style = "margin: 0;"),
                div(class = "mm-download-row",
                  downloadButton("mm_download_png", "Download PNG", class = "btn-mm-download"),
                  downloadButton("mm_download_pdf", "Download All (PDF)", class = "btn-mm-download")
                )
              ),
              div(style = "margin-top: 16px;",
                gt_output("mm_results_table")
              )
            )
          ),
          
          # Empty state
          conditionalPanel("input.mm_analyze == 0",
            div(class = "mm-empty-state",
              div(class = "icon", icon("baseball")),
              p("Select hitters and pitchers, then click Analyze")
            )
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
  
  # Initialize all selectize inputs AFTER login
  observeEvent(logged_in(), {
    if (logged_in()) {
      updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_pitchers", choices = all_pitchers, server = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$scout_hitters, {
    hitters <- input$scout_hitters
    current_data <- scout_data()
    for (h in hitters) {
      if (is.null(current_data[[h]])) {
        profile <- calculate_hitter_profile(h, tm_data, gm)
        if (!is.null(profile)) current_data[[h]] <- list(profile = profile, lhp_plan = "", rhp_plan = "", overall_notes = "", pitcher_matchup_notes = "")
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
          div(tags$span(class = "player-name", format_short_name(h_name)), tags$span(style = "color:#666; margin-left:8px;", format_batter_hand(profile$hand))),
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
              div(class = "hitter-info", 
                  h3(paste0(format_short_name(h_name), " ", format_batter_hand(profile$hand))), 
                  div(class = "hitter-meta", paste0("PA: ", profile$n_pa, " | Pitches: ", profile$n))),
              div(class = "grade-row", style = "margin-left: auto;",
                  create_grade_box(profile$overall_grade, "Overall"),
                  create_grade_box(profile$game_power_grade, "GamePwr"),
                  create_grade_box(profile$contact_grade, "Contact"),
                  create_grade_box(profile$avoid_k_grade, "AvoidK"), 
                  create_grade_box(profile$swing_dec_grade, "SwDec")))),
      
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
      # ADVANCED HEAT MAPS SECTION - Grid Layout with Stat & Sample in Each Cell
      # ============================================================================
      fluidRow(
        column(12,
               div(class = "detail-section", 
                   div(class = "section-header section-header-orange", "Advanced Heat Maps"),
                   p(style = "font-size: 11px; color: #666; margin-bottom: 10px;", 
                     "Each heatmap cell shows the stat value and sample size. Rows = Pitch Type or Count, Columns = Stats."),
                   
                   # Toggle to show/hide advanced heat maps
                   checkboxInput(paste0("show_adv_heatmaps_", hitter_id), "Show Advanced Heat Maps", value = FALSE),
                   
                   conditionalPanel(
                     condition = paste0("input.show_adv_heatmaps_", hitter_id, " == true"),
                     
                     # Tab panel for vs RHP and vs LHP
                     tabsetPanel(id = paste0("adv_heatmap_tabs_", hitter_id), type = "pills",
                       
                       # vs RHP Tab
                       tabPanel("vs RHP",
                         div(style = "margin-top: 15px;",
                             # Pitch Type Grid vs RHP
                             div(style = "font-weight: bold; margin-bottom: 5px; text-align: center; font-size: 14px; color: #006F71;", 
                                 "Pitch Type Performance vs RHP"),
                             uiOutput(paste0("adv_hm_grid_pitch_rhp_", hitter_id)),
                             hr(),
                             # Count Type Grid vs RHP
                             div(style = "font-weight: bold; margin-bottom: 5px; margin-top: 15px; text-align: center; font-size: 14px; color: #006F71;", 
                                 "Count Performance vs RHP"),
                             uiOutput(paste0("adv_hm_grid_count_rhp_", hitter_id))
                         )
                       ),
                       
                       # vs LHP Tab
                       tabPanel("vs LHP",
                         div(style = "margin-top: 15px;",
                             # Pitch Type Grid vs LHP
                             div(style = "font-weight: bold; margin-bottom: 5px; text-align: center; font-size: 14px; color: #C62828;", 
                                 "Pitch Type Performance vs LHP"),
                             uiOutput(paste0("adv_hm_grid_pitch_lhp_", hitter_id)),
                             hr(),
                             # Count Type Grid vs LHP  
                             div(style = "font-weight: bold; margin-bottom: 5px; margin-top: 15px; text-align: center; font-size: 14px; color: #C62828;", 
                                 "Count Performance vs LHP"),
                             uiOutput(paste0("adv_hm_grid_count_lhp_", hitter_id))
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
            column(3, 
                   tags$label(style = "color: #C62828; font-weight: bold; font-size: 13px;", "vs LHP Plan:"),
                   textAreaInput(paste0("lhp_plan_", hitter_id), NULL, value = h_data$lhp_plan, rows = 3, placeholder = "Attack plan vs lefties")),
            column(3, 
                   tags$label(style = "color: black; font-weight: bold; font-size: 13px;", "vs RHP Plan:"),
                   textAreaInput(paste0("rhp_plan_", hitter_id), NULL, value = h_data$rhp_plan, rows = 3, placeholder = "Attack plan vs righties")),
            column(3, 
                   tags$label(style = "color: #1565C0; font-weight: bold; font-size: 13px;", "Overall Notes:"),
                   textAreaInput(paste0("notes_", hitter_id), NULL, value = h_data$overall_notes, rows = 3, placeholder = "General observations")),
            column(3, 
                   tags$label(style = "color: #6A1B9A; font-weight: bold; font-size: 13px;", "Pitcher Matchup Notes:"),
                   textAreaInput(paste0("pitcher_matchup_", hitter_id), NULL, value = if(!is.null(h_data$pitcher_matchup_notes)) h_data$pitcher_matchup_notes else "", rows = 3, placeholder = "Notes for specific pitchers"))),
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
    # ADVANCED HEAT MAPS RENDERING - Grid Layout with Stat & Sample in Each Cell
    # ============================================================================
    
    # Helper function to create heatmap grid HTML table with embedded heatmaps
    create_heatmap_grid_html <- function(stats_list, split_col_name, h_raw, pitcher_hand, hitter_id, split_type) {
      stat_cols <- c("Pitch Freq", "Swing%", "wOBA", "Damage", "Chase%", "Whiff%", "IZ Whiff%", "IZ Damage%")
      stat_types <- c("pitch_freq", "swing", "woba", "damage", "chase", "whiff", "iz_whiff", "iz_damage")
      
      # Create header row with column names
      header_row <- tags$tr(
        tags$th(style = "padding: 6px; background: #006F71; color: white; text-align: left; width: 70px; font-size: 11px;", split_col_name),
        lapply(stat_cols, function(col) {
          tags$th(style = "padding: 4px; background: #006F71; color: white; text-align: center; font-size: 10px; width: 80px;", col)
        })
      )
      
      # Create data rows - each row is a pitch type or count type
      data_rows <- lapply(stats_list, function(row_data) {
        row_name <- if (!is.null(row_data$pitch_type)) row_data$pitch_type else row_data$count_type
        n_sample <- if (!is.null(row_data$n)) row_data$n else row_data$pitch_freq
        
        # Get stat values
        stat_values <- list(
          pitch_freq = n_sample,
          swing = row_data$swing_pct,
          woba = row_data$woba,
          damage = row_data$damage_pct,
          chase = row_data$chase_pct,
          whiff = row_data$whiff_pct,
          iz_whiff = row_data$iz_whiff_pct,
          iz_damage = row_data$iz_damage_pct
        )
        
        # Format stat values
        format_stat <- function(val, stat_type) {
          if (is.na(val) || is.null(val)) return("-")
          if (stat_type == "pitch_freq") return(sprintf("%.0f", val))
          if (stat_type == "woba") return(sprintf("%.3f", val))
          return(sprintf("%.1f%%", val))
        }
        
        # Color coding helper for cell background
        get_cell_bg <- function(val, stat_type) {
          if (is.na(val)) return("#f5f5f5")
          benchmarks <- list(pitch_freq = NA, swing = 48, woba = 0.320, damage = 35, chase = 28, whiff = 25, iz_whiff = 15, iz_damage = 35)
          higher_better <- list(pitch_freq = TRUE, swing = TRUE, woba = TRUE, damage = TRUE, chase = FALSE, whiff = FALSE, iz_whiff = FALSE, iz_damage = TRUE)
          
          bm <- benchmarks[[stat_type]]
          hb <- higher_better[[stat_type]]
          if (is.na(bm)) return("#f8f8f8")
          
          pct_diff <- if (hb) (val - bm) / abs(bm) * 100 else (bm - val) / abs(bm) * 100
          if (pct_diff > 10) return("#C8E6C9")
          if (pct_diff > 0) return("#DCEDC8") 
          if (pct_diff >= -10) return("#FFF9C4")
          return("#FFCDD2")
        }
        
        # Create cells for each stat
        stat_cells <- lapply(seq_along(stat_types), function(s_idx) {
          st <- stat_types[s_idx]
          val <- stat_values[[st]]
          bg_col <- get_cell_bg(val, st)
          
          tags$td(style = paste0("padding: 4px; text-align: center; background: ", bg_col, "; border: 1px solid #ddd; vertical-align: middle;"),
            div(style = "font-size: 12px; font-weight: bold; color: #333;", format_stat(val, st)),
            div(style = "font-size: 9px; color: #666;", paste0("n=", n_sample))
          )
        })
        
        tags$tr(
          tags$td(style = "padding: 6px; font-weight: bold; background: #f5f5f5; font-size: 11px; border: 1px solid #ddd;", row_name),
          stat_cells
        )
      })
      
      # Combine table
      tags$table(style = "width: 100%; border-collapse: collapse; margin-bottom: 10px;",
        tags$thead(header_row),
        tags$tbody(data_rows)
      )
    }
    
    # Advanced Heat Map Grid: Pitch Type vs RHP
    output[[paste0("adv_hm_grid_pitch_rhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Right", "pitch_type")
      if (length(stats) == 0) return(div(style = "text-align: center; padding: 20px; color: #666;", "No data available"))
      create_heatmap_grid_html(stats, "Pitch Type", h_raw, "Right", hitter_id, "pitch_type")
    })
    
    # Advanced Heat Map Grid: Pitch Type vs LHP
    output[[paste0("adv_hm_grid_pitch_lhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Left", "pitch_type")
      if (length(stats) == 0) return(div(style = "text-align: center; padding: 20px; color: #666;", "No data available"))
      create_heatmap_grid_html(stats, "Pitch Type", h_raw, "Left", hitter_id, "pitch_type")
    })
    
    # Advanced Heat Map Grid: Count Type vs RHP
    output[[paste0("adv_hm_grid_count_rhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Right", "count_type")
      if (length(stats) == 0) return(div(style = "text-align: center; padding: 20px; color: #666;", "No data available"))
      create_heatmap_grid_html(stats, "Count", h_raw, "Right", hitter_id, "count_type")
    })
    
    # Advanced Heat Map Grid: Count Type vs LHP
    output[[paste0("adv_hm_grid_count_lhp_", hitter_id)]] <- renderUI({
      stats <- get_advanced_heatmap_stats(h_name, "Left", "count_type")
      if (length(stats) == 0) return(div(style = "text-align: center; padding: 20px; color: #666;", "No data available"))
      create_heatmap_grid_html(stats, "Count", h_raw, "Left", hitter_id, "count_type")
    })
    
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
        if (!is.null(input[[paste0("pitcher_matchup_", hitter_id)]])) data[[h_name]]$pitcher_matchup_notes <- input[[paste0("pitcher_matchup_", hitter_id)]]
        scout_data(data)
        showNotification(paste("Notes saved for", h_name), type = "message", duration = 2)
      }
    }, ignoreInit = TRUE)
  })
  
  # ============================================================================
  # MATCHUP MATRICES TAB - SERVER LOGIC
  # ============================================================================
  
  # Status text
  output$mm_status <- renderText({
    n_h <- length(input$mm_hitters)
    n_p <- length(input$mm_pitchers)
    paste0(format(nrow(tm_data), big.mark = ","), " pitches | ", 
           n_h, " hitter", ifelse(n_h != 1, "s", ""), " selected | ",
           n_p, " pitcher", ifelse(n_p != 1, "s", ""), " selected")
  })
  
  # Results title
  output$mm_results_title <- renderText({
    switch(input$mm_view_mode,
      "matrix_overall" = "Matchup Matrix (Overall Weighted RV/100)",
      "matrix_split" = paste("Matchup Matrix:", MM_ALL_SPLITS[[input$mm_selected_split]]$name),
      "hitter_profiles" = "Hitter Profiles (RV/100 by Split)",
      "pitcher_profiles" = "Pitcher Profiles (RV/100 by Split)"
    )
  })
  
  # Calculate all data on analyze
  mm_calc_data <- eventReactive(input$mm_analyze, {
    req(tm_data)
    
    hitter_list <- if (length(input$mm_hitters) > 0) {
      lapply(input$mm_hitters, function(h) {
        hd <- tm_data[tm_data$Batter == h, , drop = FALSE]
        if (nrow(hd) == 0) return(NULL)
        list(name = h, splits = mm_calc_all_splits(hd, "mean_DRE_bat", is_pitcher = FALSE), data = hd, n = nrow(hd))
      })
    } else NULL
    hitter_list <- Filter(Negate(is.null), hitter_list)
    if (length(hitter_list) > 0) names(hitter_list) <- sapply(hitter_list, `[[`, "name")
    
    pitcher_list <- if (length(input$mm_pitchers) > 0) {
      lapply(input$mm_pitchers, function(p) {
        pd <- tm_data[tm_data$Pitcher == p, , drop = FALSE]
        if (nrow(pd) == 0) return(NULL)
        list(name = p, splits = mm_calc_all_splits(pd, "mean_DRE_pit", is_pitcher = TRUE), data = pd, n = nrow(pd))
      })
    } else NULL
    pitcher_list <- Filter(Negate(is.null), pitcher_list)
    if (length(pitcher_list) > 0) names(pitcher_list) <- sapply(pitcher_list, `[[`, "name")
    
    list(hitters = hitter_list, pitchers = pitcher_list)
  })
  
  # Get display splits helper
  mm_get_display_splits <- function() {
    splits <- unlist(MM_DISPLAY_GROUPS[input$mm_split_groups], use.names = FALSE)
    splits[splits %in% names(MM_ALL_SPLITS)]
  }
  
  # Main results table
  output$mm_results_table <- render_gt({
    req(mm_calc_data())
    cd <- mm_calc_data()
    
    # Matrix Overall
    if (input$mm_view_mode == "matrix_overall") {
      req(cd$hitters, cd$pitchers)
      
      mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
      conf_mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
      rownames(mat) <- names(cd$hitters)
      colnames(mat) <- names(cd$pitchers)
      
      for (i in seq_along(cd$hitters)) {
        h <- cd$hitters[[i]]
        for (j in seq_along(cd$pitchers)) {
          p <- cd$pitchers[[j]]
          overall <- mm_calc_overall_matchup(h$splits, p$splits, p$data)
          mat[i, j] <- round(overall$rv, 2)
          conf_mat[i, j] <- overall$confidence
        }
      }
      
      as.data.frame(mat) %>%
        tibble::rownames_to_column("Hitter") %>%
        gt() %>%
        tab_header(title = "Overall Matchup Matrix", subtitle = "Positive = hitter advantage | Confidence-weighted") %>%
        data_color(columns = -Hitter, fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-10, -3, 0, 3, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(
          table.font.size = "14px",
          heading.subtitle.font.size = "12px",
          column_labels.font.weight = "bold",
          column_labels.background.color = "#e8f5f4"
        ) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    # Matrix by Split
    else if (input$mm_view_mode == "matrix_split") {
      req(cd$hitters, cd$pitchers, input$mm_selected_split)
      s <- input$mm_selected_split
      
      mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
      rownames(mat) <- names(cd$hitters)
      colnames(mat) <- names(cd$pitchers)
      
      for (i in seq_along(cd$hitters)) {
        h <- cd$hitters[[i]]
        for (j in seq_along(cd$pitchers)) {
          p <- cd$pitchers[[j]]
          m <- mm_calc_matchup(h$splits[[s]]$rv100, p$splits[[s]]$rv100, 
                              h$splits[[s]]$weight, p$splits[[s]]$weight)
          mat[i, j] <- round(m$weighted, 2)
        }
      }
      
      as.data.frame(mat) %>%
        tibble::rownames_to_column("Hitter") %>%
        gt() %>%
        tab_header(title = paste("Matchup:", MM_ALL_SPLITS[[s]]$name), subtitle = "Positive = hitter advantage | Confidence-weighted") %>%
        data_color(columns = -Hitter, fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-10, -3, 0, 3, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(
          table.font.size = "14px",
          heading.subtitle.font.size = "12px",
          column_labels.font.weight = "bold",
          column_labels.background.color = "#e8f5f4"
        ) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    # Hitter Profiles
    else if (input$mm_view_mode == "hitter_profiles") {
      req(cd$hitters)
      display_splits <- mm_get_display_splits()
      
      rows <- lapply(cd$hitters, function(h) {
        vals <- sapply(display_splits, function(s) round(h$splits[[s]]$rv100, 2))
        confs <- sapply(display_splits, function(s) h$splits[[s]]$weight)
        ns <- sapply(display_splits, function(s) h$splits[[s]]$n)
        c(Hitter = h$name, N = nrow(h$data), vals)
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      names(df) <- c("Hitter", "N", sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name))
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      df %>%
        gt() %>%
        tab_header(title = "Hitter Profiles", subtitle = "RV/100 (positive = good for hitter) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(
          table.font.size = "13px",
          heading.subtitle.font.size = "12px",
          column_labels.font.weight = "bold",
          column_labels.background.color = "#e8f5f4"
        ) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    # Pitcher Profiles
    else if (input$mm_view_mode == "pitcher_profiles") {
      req(cd$pitchers)
      display_splits <- mm_get_display_splits()
      
      rows <- lapply(cd$pitchers, function(p) {
        vals <- sapply(display_splits, function(s) round(p$splits[[s]]$rv100, 2))
        c(Pitcher = p$name, N = nrow(p$data), vals)
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      names(df) <- c("Pitcher", "N", sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name))
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      df %>%
        gt() %>%
        tab_header(title = "Pitcher Profiles", subtitle = "RV/100 (negative = good for pitcher) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#1e8449", "#27ae60", "#f5f5f5", "#e74c3c", "#c0392b"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(
          table.font.size = "13px",
          heading.subtitle.font.size = "12px",
          column_labels.font.weight = "bold",
          column_labels.background.color = "#e8f5f4"
        ) %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Pitcher"))
    }
  })
  
  # Helper function to generate a gt table for a specific view
  mm_generate_gt_table <- function(view_mode, split_name = NULL, custom_splits = NULL) {
    cd <- mm_calc_data()
    if (is.null(cd)) return(NULL)
    
    display_splits <- if (!is.null(custom_splits)) {
      custom_splits
    } else {
      mm_get_display_splits()
    }
    
    if (view_mode == "matrix_overall") {
      if (is.null(cd$hitters) || is.null(cd$pitchers)) return(NULL)
      
      mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
      rownames(mat) <- names(cd$hitters)
      colnames(mat) <- names(cd$pitchers)
      
      for (i in seq_along(cd$hitters)) {
        h <- cd$hitters[[i]]
        for (j in seq_along(cd$pitchers)) {
          p <- cd$pitchers[[j]]
          overall <- mm_calc_overall_matchup(h$splits, p$splits, p$data)
          mat[i, j] <- round(overall$rv, 2)
        }
      }
      
      as.data.frame(mat) %>%
        tibble::rownames_to_column("Hitter") %>%
        gt() %>%
        tab_header(title = "Overall Matchup Matrix", subtitle = "Positive = hitter advantage | Confidence-weighted") %>%
        data_color(columns = -Hitter, fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-10, -3, 0, 3, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "14px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    else if (view_mode == "matrix_split" && !is.null(split_name)) {
      if (is.null(cd$hitters) || is.null(cd$pitchers)) return(NULL)
      s <- split_name
      
      mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
      rownames(mat) <- names(cd$hitters)
      colnames(mat) <- names(cd$pitchers)
      
      for (i in seq_along(cd$hitters)) {
        h <- cd$hitters[[i]]
        for (j in seq_along(cd$pitchers)) {
          p <- cd$pitchers[[j]]
          m <- mm_calc_matchup(h$splits[[s]]$rv100, p$splits[[s]]$rv100, h$splits[[s]]$weight, p$splits[[s]]$weight)
          mat[i, j] <- round(m$weighted, 2)
        }
      }
      
      as.data.frame(mat) %>%
        tibble::rownames_to_column("Hitter") %>%
        gt() %>%
        tab_header(title = paste("Matchup:", MM_ALL_SPLITS[[s]]$name), subtitle = "Positive = hitter advantage | Confidence-weighted") %>%
        data_color(columns = -Hitter, fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-10, -3, 0, 3, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "14px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    else if (view_mode == "hitter_profiles") {
      if (is.null(cd$hitters) || length(display_splits) == 0) return(NULL)
      
      rows <- lapply(cd$hitters, function(h) {
        vals <- sapply(display_splits, function(s) round(h$splits[[s]]$rv100, 2))
        c(Hitter = h$name, N = nrow(h$data), vals)
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      names(df) <- c("Hitter", "N", sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name))
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      df %>%
        gt() %>%
        tab_header(title = "Hitter Profiles", subtitle = "RV/100 (positive = good for hitter) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "13px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
    
    else if (view_mode == "pitcher_profiles") {
      if (is.null(cd$pitchers) || length(display_splits) == 0) return(NULL)
      
      rows <- lapply(cd$pitchers, function(p) {
        vals <- sapply(display_splits, function(s) round(p$splits[[s]]$rv100, 2))
        c(Pitcher = p$name, N = nrow(p$data), vals)
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      names(df) <- c("Pitcher", "N", sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name))
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      df %>%
        gt() %>%
        tab_header(title = "Pitcher Profiles", subtitle = "RV/100 (negative = good for pitcher) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#1e8449", "#27ae60", "#f5f5f5", "#e74c3c", "#c0392b"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "13px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Pitcher"))
    }
    
    else {
      NULL
    }
  }
  
  # Download PNG of current view
  output$mm_download_png <- downloadHandler(
    filename = function() {
      paste0("matchup_", input$mm_view_mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      gt_tbl <- mm_generate_gt_table(input$mm_view_mode, input$mm_selected_split)
      if (!is.null(gt_tbl)) {
        gtsave(gt_tbl, file, vwidth = 1200)
      }
    }
  )
  
  # Download PDF with all views
  output$mm_download_pdf <- downloadHandler(
    filename = function() {
      paste0("matchup_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      temp_dir <- tempdir()
      img_files <- list()
      
      # Define pitch family and platoon splits
      family_platoon_splits <- c("fb_rhp", "bb_rhp", "os_rhp", "fb_lhp", "bb_lhp", "os_lhp", "vs_rhp", "vs_lhp", "overall")
      
      # Generate tables
      tables_to_generate <- list(
        list(name = "Overall Matrix", view = "matrix_overall", splits = NULL),
        list(name = "Hitter Profiles (Movement)", view = "hitter_profiles", splits = mm_get_display_splits()),
        list(name = "Pitcher Profiles (Movement)", view = "pitcher_profiles", splits = mm_get_display_splits()),
        list(name = "Hitter Profiles (Family/Platoon)", view = "hitter_profiles", splits = family_platoon_splits),
        list(name = "Pitcher Profiles (Family/Platoon)", view = "pitcher_profiles", splits = family_platoon_splits)
      )
      
      for (tbl_info in tables_to_generate) {
        gt_tbl <- mm_generate_gt_table(tbl_info$view, NULL, tbl_info$splits)
        if (!is.null(gt_tbl)) {
          img_path <- file.path(temp_dir, paste0(gsub("[^a-zA-Z0-9]", "_", tbl_info$name), ".png"))
          tryCatch({
            gtsave(gt_tbl, img_path, vwidth = 1400)
            img_files[[tbl_info$name]] <- img_path
          }, error = function(e) {
            message("Error saving ", tbl_info$name, ": ", e$message)
          })
        }
      }
      
      # Build HTML content with custom inputs
      report_title <- if (nchar(input$mm_pdf_title) > 0) input$mm_pdf_title else "Matchup Matrix Report"
      best_matchups <- input$mm_pdf_best_matchups
      worst_matchups <- input$mm_pdf_worst_matchups
      notes <- input$mm_pdf_notes
      
      html_content <- paste0(
        '<!DOCTYPE html><html><head>',
        '<style>',
        '@page { size: letter landscape; margin: 0.5in; }',
        'body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }',
        '.header { text-align: center; margin-bottom: 30px; padding-bottom: 20px; border-bottom: 3px solid #006F71; }',
        'h1 { color: #006F71; margin: 0 0 10px; font-size: 28px; }',
        '.timestamp { color: #666; font-size: 12px; }',
        '.notes-section { background: #f8f9fa; border-radius: 8px; padding: 20px; margin-bottom: 30px; }',
        '.notes-section h3 { color: #006F71; margin: 0 0 10px; font-size: 16px; }',
        '.notes-row { display: flex; gap: 20px; margin-bottom: 15px; }',
        '.notes-box { flex: 1; }',
        '.notes-box h4 { color: #CD853F; margin: 0 0 8px; font-size: 14px; }',
        '.notes-box p { margin: 0; font-size: 13px; line-height: 1.5; white-space: pre-wrap; }',
        '.matchups-good { border-left: 4px solid #27ae60; padding-left: 12px; }',
        '.matchups-bad { border-left: 4px solid #c0392b; padding-left: 12px; }',
        '.table-section { margin-bottom: 40px; page-break-inside: avoid; }',
        '.table-section h3 { color: #006F71; font-size: 16px; margin-bottom: 15px; }',
        'img { max-width: 100%; height: auto; }',
        '</style>',
        '</head><body>',
        '<div class="header">',
        '<h1>', htmltools::htmlEscape(report_title), '</h1>',
        '<p class="timestamp">Generated: ', format(Sys.time(), "%B %d, %Y at %I:%M %p"), '</p>',
        '</div>'
      )
      
      # Add notes section if any content provided
      if (nchar(best_matchups) > 0 || nchar(worst_matchups) > 0 || nchar(notes) > 0) {
        html_content <- paste0(html_content, '<div class="notes-section">')
        
        if (nchar(best_matchups) > 0 || nchar(worst_matchups) > 0) {
          html_content <- paste0(html_content, '<div class="notes-row">')
          
          if (nchar(best_matchups) > 0) {
            html_content <- paste0(html_content,
              '<div class="notes-box matchups-good">',
              '<h4>Best Matchups</h4>',
              '<p>', htmltools::htmlEscape(best_matchups), '</p>',
              '</div>')
          }
          
          if (nchar(worst_matchups) > 0) {
            html_content <- paste0(html_content,
              '<div class="notes-box matchups-bad">',
              '<h4>Worst Matchups</h4>',
              '<p>', htmltools::htmlEscape(worst_matchups), '</p>',
              '</div>')
          }
          
          html_content <- paste0(html_content, '</div>')
        }
        
        if (nchar(notes) > 0) {
          html_content <- paste0(html_content,
            '<div class="notes-box">',
            '<h4>Notes</h4>',
            '<p>', htmltools::htmlEscape(notes), '</p>',
            '</div>')
        }
        
        html_content <- paste0(html_content, '</div>')
      }
      
      # Add table images
      for (tbl_name in names(img_files)) {
        img_path <- img_files[[tbl_name]]
        if (file.exists(img_path)) {
          img_data <- base64enc::base64encode(img_path)
          html_content <- paste0(html_content,
            '<div class="table-section">',
            '<h3>', htmltools::htmlEscape(tbl_name), '</h3>',
            '<img src="data:image/png;base64,', img_data, '">',
            '</div>')
        }
      }
      
      html_content <- paste0(html_content, '</body></html>')
      
      html_file <- file.path(temp_dir, "report.html")
      writeLines(html_content, html_file)
      
      # Convert to PDF
      tryCatch({
        pagedown::chrome_print(html_file, file, wait = 15)
      }, error = function(e) {
        message("PDF generation error: ", e$message)
        # Fallback: copy first image
        if (length(img_files) > 0) {
          file.copy(img_files[[1]], file)
        }
      })
    }
  )
  
  # ============================================================================
  # END MATCHUP MATRICES TAB
  # ============================================================================

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
      ROW2_OFFSET <- 0.050      # Notes row (expanded)
      ROW3_OFFSET <- 0.105      # Diamonds row (adjusted for expanded notes)
      
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
      
      # Row 2 (Notes) settings - Expanded for more notes space
      ROW2_HEIGHT <- 0.042
      ROW2_BG <- "white"
      NOTES_FONT_SIZE <- 5.5
      LHP_COLOR <- "#C62828"
      RHP_COLOR <- "black"
      OVERALL_COLOR <- "#1565C0"
      MATCHUP_COLOR <- "#6A1B9A"
      LHP_X <- 0.025
      RHP_X <- 0.26
      OVERALL_X <- 0.50
      MATCHUP_X <- 0.75
      
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
          
          # Name (first initial + last name format)
          short_name <- format_short_name(h_name)
          grid::grid.text(short_name, x = NAME_X, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = NAME_FONT_SIZE, fontface = "bold"))
          
          # Hand label - abbreviated (R), (L), (B)
          hand_label <- format_batter_hand(profile$hand)
          hand_color <- get_hand_color(profile$hand)
          grid::grid.text(hand_label, x = HAND_X, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = HAND_FONT_SIZE, col = hand_color, fontface = "bold"))
          
          # Grade boxes - explicit X positions (Overall, GamePwr, Contact, AvoidK, SwDec)
          grades <- c(profile$overall_grade, profile$game_power_grade, profile$contact_grade, 
                     profile$avoid_k_grade, profile$swing_dec_grade)
          grade_labels <- c("Overall", "GamePwr", "Contact", "AvoidK", "SwDec")
          grade_x_positions <- c(0.18, 0.22, 0.26, 0.30, 0.34)
          
          for (g_idx in 1:5) {
            # Grade box with number
            grid::grid.rect(x = grade_x_positions[g_idx], y = row1_y + 0.004, 
                           width = GRADE_BOX_WIDTH, height = GRADE_BOX_HEIGHT,
                           gp = grid::gpar(fill = grade_color_light(grades[g_idx]), col = "gray50", lwd = 0.3))
            grid::grid.text(grades[g_idx], x = grade_x_positions[g_idx], y = row1_y + 0.004, 
                           gp = grid::gpar(fontsize = GRADE_FONT_SIZE, fontface = "bold"))
            # Label below the grade box
            grid::grid.text(grade_labels[g_idx], x = grade_x_positions[g_idx], y = row1_y - 0.008, 
                           gp = grid::gpar(fontsize = 4, col = "gray40"))
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
          
          # === ROW 2: NOTES (Expanded with more space) ===
          
          grid::grid.rect(x = 0.5, y = row2_y, width = PAGE_WIDTH, height = ROW2_HEIGHT,
                         gp = grid::gpar(fill = ROW2_BG, col = "gray80", lwd = 0.3))
          
          # Notes layout: LHP Plan | RHP Plan | Overall Notes | Matchup Notes
          # Each section gets more room for longer notes
          note_x_positions <- c(LHP_X, RHP_X, OVERALL_X, MATCHUP_X)  # LHP, RHP, Overall, Matchup
          
          # Two-line notes for each section
          row2_y_line1 <- row2_y + 0.008
          row2_y_line2 <- row2_y - 0.008
          
          if (nchar(notes$lhp_plan) > 0) {
            lhp_text_l1 <- paste0("LHP: ", substr(notes$lhp_plan, 1, 38))
            lhp_text_l2 <- if(nchar(notes$lhp_plan) > 38) substr(notes$lhp_plan, 39, 75) else ""
            grid::grid.text(lhp_text_l1, x = note_x_positions[1], y = row2_y_line1, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = LHP_COLOR, fontface = "bold"))
            if (nchar(lhp_text_l2) > 0) {
              grid::grid.text(lhp_text_l2, x = note_x_positions[1], y = row2_y_line2, just = "left",
                             gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = LHP_COLOR))
            }
          }
          
          if (nchar(notes$rhp_plan) > 0) {
            rhp_text_l1 <- paste0("RHP: ", substr(notes$rhp_plan, 1, 38))
            rhp_text_l2 <- if(nchar(notes$rhp_plan) > 38) substr(notes$rhp_plan, 39, 75) else ""
            grid::grid.text(rhp_text_l1, x = note_x_positions[2], y = row2_y_line1, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = RHP_COLOR, fontface = "bold"))
            if (nchar(rhp_text_l2) > 0) {
              grid::grid.text(rhp_text_l2, x = note_x_positions[2], y = row2_y_line2, just = "left",
                             gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = RHP_COLOR))
            }
          }
          
          if (nchar(notes$overall_notes) > 0) {
            overall_text_l1 <- paste0("Notes: ", substr(notes$overall_notes, 1, 38))
            overall_text_l2 <- if(nchar(notes$overall_notes) > 38) substr(notes$overall_notes, 39, 75) else ""
            grid::grid.text(overall_text_l1, x = note_x_positions[3], y = row2_y_line1, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = OVERALL_COLOR, fontface = "bold"))
            if (nchar(overall_text_l2) > 0) {
              grid::grid.text(overall_text_l2, x = note_x_positions[3], y = row2_y_line2, just = "left",
                             gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = OVERALL_COLOR))
            }
          }
          
          # Pitcher matchup notes (for specific pitcher matchup info)
          if (!is.null(notes$pitcher_matchup_notes) && nchar(notes$pitcher_matchup_notes) > 0) {
            matchup_text_l1 <- paste0("Matchup: ", substr(notes$pitcher_matchup_notes, 1, 35))
            matchup_text_l2 <- if(nchar(notes$pitcher_matchup_notes) > 35) substr(notes$pitcher_matchup_notes, 36, 70) else ""
            grid::grid.text(matchup_text_l1, x = note_x_positions[4], y = row2_y_line1, just = "left",
                           gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = MATCHUP_COLOR, fontface = "bold"))
            if (nchar(matchup_text_l2) > 0) {
              grid::grid.text(matchup_text_l2, x = note_x_positions[4], y = row2_y_line2, just = "left",
                             gp = grid::gpar(fontsize = NOTES_FONT_SIZE, col = MATCHUP_COLOR))
            }
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
      
      # ============================================================
      # ADVANCED HEATMAP PAGES - One page per hitter with stat/sample grid
      # ============================================================
      
      # Helper function to draw a mini heatmap cell with stat and sample overlay
      draw_heatmap_cell <- function(cx, cy, cell_width, cell_height, stat_val, n_sample, 
                                   stat_type = "whiff", highlight_color = "#FF6666") {
        # Draw cell background
        bg_color <- if(is.na(stat_val)) "#f5f5f5" else {
          # Color based on stat type and value
          benchmarks <- list(pitch_freq = NA, swing = 48, woba = 0.320, damage = 35, 
                            chase = 28, whiff = 25, iz_whiff = 15, iz_damage = 35)
          higher_better <- list(pitch_freq = TRUE, swing = TRUE, woba = TRUE, damage = TRUE, 
                               chase = FALSE, whiff = FALSE, iz_whiff = FALSE, iz_damage = TRUE)
          bm <- benchmarks[[stat_type]]
          hb <- higher_better[[stat_type]]
          if (is.na(bm)) "#f8f8f8" else {
            pct_diff <- if(hb) (stat_val - bm) / abs(bm) * 100 else (bm - stat_val) / abs(bm) * 100
            if (pct_diff > 10) "#C8E6C9" else if (pct_diff > 0) "#DCEDC8" else if (pct_diff >= -10) "#FFF9C4" else "#FFCDD2"
          }
        }
        
        # Cell background
        grid::grid.rect(x = cx, y = cy, width = cell_width, height = cell_height,
                       gp = grid::gpar(fill = bg_color, col = "gray60", lwd = 0.3))
        
        # Strike zone mini box
        zone_size <- min(cell_width, cell_height) * 0.4
        grid::grid.rect(x = cx, y = cy + cell_height * 0.1, width = zone_size, height = zone_size * 1.1,
                       gp = grid::gpar(fill = NA, col = "black", lwd = 0.5))
        
        # Stat value
        stat_text <- if(is.na(stat_val)) "-" else {
          if(stat_type == "pitch_freq") sprintf("%.0f", stat_val)
          else if(stat_type == "woba") sprintf("%.3f", stat_val)
          else sprintf("%.1f%%", stat_val)
        }
        grid::grid.text(stat_text, x = cx, y = cy + cell_height * 0.1, 
                       gp = grid::gpar(fontsize = 7, fontface = "bold", col = "#333"))
        
        # Sample size
        grid::grid.text(paste0("n=", n_sample), x = cx, y = cy - cell_height * 0.3,
                       gp = grid::gpar(fontsize = 5, col = "gray50"))
      }
      
      # Generate advanced heatmap page for each hitter
      for (h_name in names(data)) {
        grid::grid.newpage()
        
        profile <- data[[h_name]]$profile
        short_name <- format_short_name(h_name)
        hand_label <- format_batter_hand(profile$hand)
        
        # === PAGE HEADER ===
        grid::grid.rect(x = 0.5, y = 0.975, width = 1, height = 0.045, 
                       gp = grid::gpar(fill = "#006F71", col = NA))
        grid::grid.text(paste0("ADVANCED HEATMAPS: ", short_name, " ", hand_label), 
                       x = 0.5, y = 0.975, 
                       gp = grid::gpar(fontsize = 12, fontface = "bold", col = "white"))
        grid::grid.text(format(Sys.Date(), "%m/%d/%Y"), x = 0.95, y = 0.975, just = "right",
                       gp = grid::gpar(fontsize = 8, col = "white"))
        
        # === PITCH TYPE GRID (TOP HALF) ===
        grid::grid.text("vs RHP - By Pitch Type", x = 0.5, y = 0.92, 
                       gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#006F71"))
        
        # Column headers
        stat_cols <- c("Pitch Freq", "Swing%", "wOBA", "Damage", "Chase%", "Whiff%", "IZ Whiff%", "IZ Damage%")
        stat_types_pdf <- c("pitch_freq", "swing", "woba", "damage", "chase", "whiff", "iz_whiff", "iz_damage")
        col_width <- 0.1
        col_start_x <- 0.18
        
        for (s_idx in seq_along(stat_cols)) {
          grid::grid.text(stat_cols[s_idx], x = col_start_x + (s_idx - 0.5) * col_width, y = 0.90,
                         gp = grid::gpar(fontsize = 6, fontface = "bold", col = "#006F71"))
        }
        
        # Row labels and data for pitch types
        pitch_rows <- c("4S", "2S/Si", "SL/SW", "CB", "CH/Spl", "Overall")
        row_height <- 0.065
        row_start_y <- 0.85
        
        # Get stats from cache for RHP
        pitch_stats_rhp <- get_advanced_heatmap_stats(h_name, "Right", "pitch_type")
        
        for (r_idx in seq_along(pitch_rows)) {
          row_y <- row_start_y - (r_idx - 1) * row_height
          
          # Row label
          grid::grid.text(pitch_rows[r_idx], x = 0.08, y = row_y,
                         gp = grid::gpar(fontsize = 7, fontface = "bold"))
          
          # Get stats for this row
          row_stat <- NULL
          for (ps in pitch_stats_rhp) {
            if (!is.null(ps$pitch_type) && ps$pitch_type == pitch_rows[r_idx]) {
              row_stat <- ps
              break
            }
          }
          
          n_sample <- if(!is.null(row_stat)) row_stat$n else 0
          
          # Draw cells for each stat
          stat_values <- if(!is.null(row_stat)) {
            c(row_stat$n, row_stat$swing_pct, row_stat$woba, row_stat$damage_pct,
              row_stat$chase_pct, row_stat$whiff_pct, row_stat$iz_whiff_pct, row_stat$iz_damage_pct)
          } else rep(NA, 8)
          
          for (s_idx in seq_along(stat_types_pdf)) {
            cell_x <- col_start_x + (s_idx - 0.5) * col_width
            draw_heatmap_cell(cell_x, row_y, col_width * 0.9, row_height * 0.85, 
                             stat_values[s_idx], n_sample, stat_types_pdf[s_idx])
          }
        }
        
        # === COUNT TYPE GRID (BOTTOM HALF) ===
        grid::grid.text("vs RHP - By Count", x = 0.5, y = 0.42, 
                       gp = grid::gpar(fontsize = 10, fontface = "bold", col = "#006F71"))
        
        # Column headers for count grid
        for (s_idx in seq_along(stat_cols)) {
          grid::grid.text(stat_cols[s_idx], x = col_start_x + (s_idx - 0.5) * col_width, y = 0.40,
                         gp = grid::gpar(fontsize = 6, fontface = "bold", col = "#006F71"))
        }
        
        # Row labels and data for count types
        count_rows <- c("1P", "2K", "Ahead", "Behind", "Last 15")
        count_start_y <- 0.35
        
        # Get stats from cache for RHP count data
        count_stats_rhp <- get_advanced_heatmap_stats(h_name, "Right", "count_type")
        
        for (r_idx in seq_along(count_rows)) {
          row_y <- count_start_y - (r_idx - 1) * row_height
          
          # Row label
          grid::grid.text(count_rows[r_idx], x = 0.08, y = row_y,
                         gp = grid::gpar(fontsize = 7, fontface = "bold"))
          
          # Get stats for this row
          row_stat <- NULL
          for (cs in count_stats_rhp) {
            if (!is.null(cs$count_type) && cs$count_type == count_rows[r_idx]) {
              row_stat <- cs
              break
            }
          }
          
          n_sample <- if(!is.null(row_stat)) row_stat$n else 0
          
          # Draw cells for each stat
          stat_values <- if(!is.null(row_stat)) {
            c(row_stat$n, row_stat$swing_pct, row_stat$woba, row_stat$damage_pct,
              row_stat$chase_pct, row_stat$whiff_pct, row_stat$iz_whiff_pct, row_stat$iz_damage_pct)
          } else rep(NA, 8)
          
          for (s_idx in seq_along(stat_types_pdf)) {
            cell_x <- col_start_x + (s_idx - 0.5) * col_width
            draw_heatmap_cell(cell_x, row_y, col_width * 0.9, row_height * 0.85, 
                             stat_values[s_idx], n_sample, stat_types_pdf[s_idx])
          }
        }
        
        # Footer
        grid::grid.text("Data: TrackMan | Coastal Carolina Baseball Analytics", x = 0.5, y = 0.02,
                       gp = grid::gpar(fontsize = 6, col = "gray50", fontface = "italic"))
      }
      
      dev.off()
    }
  )
}
}

shinyApp(ui = ui, server = server)
