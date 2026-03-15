# Force Chrome flags BEFORE loading chromote
Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome-stable")

options(shiny.error = function() {
  message(geterrmessage())
})

Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")
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
library(patchwork)
library(webshot2)
library(chromote)
library(png)
library(htmltools)
library(reticulate)

# Configure chromote to run in container
set_default_chromote_object(
  Chromote$new(browser = Chrome$new(args = c("--no-sandbox", "--disable-gpu", "--headless", "--disable-dev-shm-usage")))
)

options(shiny.sanitize.errors = FALSE)

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

clean_bind <- function(data) {
  data <- data %>% 
    mutate(
      Date = as.Date(Date),
      across(any_of(c("Time", "Tilt", "UTCTime", "LocalDateTime", "UTCDateTime",
                      "CatchPositionX", "CatchPositionY", "CatchPositionZ",
                      "ThrowPositionX", "ThrowPositionY", "ThrowPositionZ",
                      "BasePositionX", "BasePositionY", "BasePositionZ")), as.character),
      across(starts_with("ThrowTrajectory"), as.character)
    )
  
  if ("UTCDate" %in% names(data)) data$UTCDate <- as.Date(data$UTCDate)
  
  data
}

download_master_dataset <- function(repo_id, folder, token = Sys.getenv("HF_Token")) {
  hf <- reticulate::import("huggingface_hub")
  api <- hf$HfApi()
  
  files <- api$list_repo_files(repo_id = repo_id, repo_type = "dataset", token = token)
  pq_files <- files[grepl(paste0("^", folder, "/.*\\.parquet$"), files)]
  
  if (length(pq_files) == 0) return(data.frame())
  
  all_data <- lapply(pq_files, function(f) {
    url <- paste0("https://huggingface.co/datasets/", repo_id, "/resolve/main/", f)
    resp <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", token)))
    if (httr::status_code(resp) == 200) {
      tmp <- tempfile(fileext = ".parquet")
      writeBin(httr::content(resp, as = "raw"), tmp)
      d <- arrow::read_parquet(tmp)
      file.remove(tmp)
      d
    } else NULL
  })
  
  combined <- bind_rows(Filter(Negate(is.null), lapply(all_data, clean_bind)))
  
  if ("PitchUID" %in% names(combined)) {
    combined <- combined %>% distinct(PitchUID, .keep_all = TRUE)
  }
  
  combined
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
data_2026 <- download_master_dataset("CoastalBaseball/2026MasterDataset", "pbp")

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

if (median(abs(data_2026$PlateLocSide), na.rm = TRUE) > 5) {
  data_2026 <- data_2026 %>% mutate(PlateLocSide = PlateLocSide / 12, PlateLocHeight = PlateLocHeight / 12)
}

data_2026 <- add_indicators(data_2026)

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

# Determine "in" side for a batter
get_in_side <- function(batter_side) {
  if (batter_side == "Right") return("negative")
  return("positive")
}

mm_fast_filter <- function(df, config, is_pitcher = FALSE, batter_side = NULL) {
  if (nrow(df) == 0) return(df)
  mask <- rep(TRUE, nrow(df))
  if (!is.null(config$pitch_types)) mask <- mask & df$TaggedPitchType %in% config$pitch_types
  if (!is.null(config$pitcher_hand)) {
    if (is_pitcher) {
      bs <- ifelse(config$pitcher_hand == "Right", "Right", "Left")
      if ("BatterSide" %in% names(df)) mask <- mask & df$BatterSide == bs
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
  if (!is.null(config$velo_max)) mask <- mask & !is.na(df$RelSpeed) & df$RelSpeed <= config$velo_max
  
  # --- LOCATION FILTERS ---
  zone_mid_height <- 2.5
  if (is.null(batter_side) && "BatterSide" %in% names(df) && sum(mask) > 0) {
    sides <- df$BatterSide[mask]; sides <- sides[!is.na(sides)]
    if (length(sides) > 0) batter_side <- names(sort(table(sides), decreasing = TRUE))[1]
  }
  if (!is.null(config$loc_up) && config$loc_up) mask <- mask & !is.na(df$PlateLocHeight) & df$PlateLocHeight >= zone_mid_height
  if (!is.null(config$loc_down) && config$loc_down) mask <- mask & !is.na(df$PlateLocHeight) & df$PlateLocHeight < zone_mid_height
  if (!is.null(config$loc_elevated) && config$loc_elevated) mask <- mask & !is.na(df$PlateLocHeight) & df$PlateLocHeight >= 2.8667
  if (!is.null(config$loc_heart) && config$loc_heart) {
    mask <- mask & !is.na(df$PlateLocSide) & !is.na(df$PlateLocHeight) & abs(df$PlateLocSide) <= 0.55 & df$PlateLocHeight >= 1.83 & df$PlateLocHeight <= 3.17
  }
  if (!is.null(config$loc_in) && config$loc_in && !is.null(batter_side)) {
    in_dir <- get_in_side(batter_side)
    if (in_dir == "negative") mask <- mask & !is.na(df$PlateLocSide) & df$PlateLocSide < 0
    else mask <- mask & !is.na(df$PlateLocSide) & df$PlateLocSide > 0
  }
  if (!is.null(config$loc_away) && config$loc_away && !is.null(batter_side)) {
    in_dir <- get_in_side(batter_side)
    if (in_dir == "negative") mask <- mask & !is.na(df$PlateLocSide) & df$PlateLocSide >= 0
    else mask <- mask & !is.na(df$PlateLocSide) & df$PlateLocSide <= 0
  }
  df[mask, , drop = FALSE]
}

mm_calc_rv_stats <- function(df, rv_col) {
  rv <- df[[rv_col]][!is.na(df[[rv_col]])]
  n <- length(rv)
  if (n == 0) return(list(rv100 = NA_real_, n = 0L))
  list(rv100 = 100 * mean(rv), n = n)
}

mm_calc_hierarchical_rv <- function(player_data, rv_col, split_name, is_pitcher = FALSE, batter_side = NULL) {
  config <- MM_ALL_SPLITS[[split_name]]
  if (is.null(config)) return(list(rv100 = 0, n = 0, weight = 0, raw_rv100 = NA_real_))
  filtered <- mm_fast_filter(player_data, config, is_pitcher, batter_side)
  stats <- mm_calc_rv_stats(filtered, rv_col)
  weight <- min(stats$n / config$threshold, 1)
  if (weight >= 1 || is.null(config$fallback)) {
    return(list(rv100 = ifelse(is.na(stats$rv100), 0, stats$rv100), n = stats$n, weight = weight, raw_rv100 = stats$rv100))
  }
  fallback <- mm_calc_hierarchical_rv(player_data, rv_col, config$fallback, is_pitcher, batter_side)
  this_rv <- ifelse(is.na(stats$rv100), 0, stats$rv100)
  blended <- weight * this_rv + (1 - weight) * fallback$rv100
  list(rv100 = blended, n = stats$n, weight = weight, raw_rv100 = stats$rv100)
}

mm_calc_all_splits <- function(player_data, rv_col, is_pitcher = FALSE, batter_side = NULL) {
  results <- lapply(names(MM_ALL_SPLITS), function(s) {
    res <- mm_calc_hierarchical_rv(player_data, rv_col, s, is_pitcher, batter_side)
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

mm_calc_overall_matchup <- function(h_splits, p_splits, p_data, include_location = FALSE) {
  total <- nrow(p_data)
  if (total == 0) return(list(rv = NA_real_, confidence = 0))
  split_configs <- MM_SPLIT_CONFIG
  if (include_location) split_configs <- c(split_configs, MM_LOCATION_CONFIG)
  wsum <- 0; csum <- 0; tweight <- 0
  for (s in names(split_configs)) {
    n_split <- nrow(mm_fast_filter(p_data, split_configs[[s]], is_pitcher = TRUE))
    usage <- n_split / total
    if (usage > 0 && s %in% names(h_splits) && s %in% names(p_splits)) {
      m <- mm_calc_matchup(h_splits[[s]]$rv100, p_splits[[s]]$rv100, h_splits[[s]]$weight, p_splits[[s]]$weight)
      wsum <- wsum + m$weighted * usage
      csum <- csum + m$confidence * usage
      tweight <- tweight + usage
    }
  }
  if (tweight > 0) list(rv = wsum / tweight, confidence = csum / tweight) else list(rv = NA_real_, confidence = 0)
}

# ============================================================================
# PITCHER-HITTER DETAILED MATCHUP ANALYSIS
# ============================================================================

identify_pitcher_arsenal <- function(pitcher_data) {
  if (nrow(pitcher_data) < 20) return(NULL)
  arsenal <- pitcher_data %>%
    filter(!is.na(TaggedPitchType)) %>%
    group_by(TaggedPitchType) %>%
    summarise(
      n = n(), pct = n() / nrow(pitcher_data) * 100,
      avg_velo = mean(RelSpeed, na.rm = TRUE),
      avg_ivb = mean(InducedVertBreak, na.rm = TRUE),
      avg_hb = mean(HorzBreak, na.rm = TRUE),
      .groups = "drop"
    ) %>% filter(pct >= 3) %>% arrange(desc(pct))
  
  arsenal <- arsenal %>% mutate(
    family = classify_pitch_family(TaggedPitchType),
    movement_tag = case_when(
      family == "FB" & avg_ivb >= 18 ~ "Ride",
      family == "FB" & avg_ivb < 9 & abs(avg_hb) >= 10 ~ "Sink",
      family == "FB" & avg_velo >= 92 ~ "Hard Velo",
      family == "FB" & avg_velo <= 88 ~ "Soft",
      family == "FB" ~ "FB",
      TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") & abs(avg_hb) >= 14 ~ "Sweep",
      TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") ~ "Slider",
      TaggedPitchType %in% c("Curveball", "CU", "CB", "KC") & avg_ivb <= -6 ~ "Downer",
      TaggedPitchType %in% c("Curveball", "CU", "CB", "KC") ~ "Curveball",
      TaggedPitchType %in% c("Slurve", "SV") ~ "Slurve",
      TaggedPitchType %in% c("Cutter", "FC") ~ "Cutter",
      family == "OS" ~ "CH/SPL",
      TRUE ~ "Other"
    )
  )
  arsenal
}

map_pitch_to_split_key <- function(movement_tag, pitcher_hand) {
  hand_suffix <- ifelse(pitcher_hand == "Right", "_rhp", "_lhp")
  switch(movement_tag,
    "Ride" = paste0("ride_fb", hand_suffix), "Sink" = paste0("sink_fb", hand_suffix),
    "Hard Velo" = paste0("hard_velo", hand_suffix), "Soft" = paste0("soft_fb", hand_suffix),
    "FB" = paste0("fb", hand_suffix), "Sweep" = paste0("sweep", hand_suffix),
    "Slider" = paste0("slider", hand_suffix), "Downer" = paste0("downer", hand_suffix),
    "Curveball" = paste0("cb", hand_suffix), "Slurve" = paste0("slurve", hand_suffix),
    "Cutter" = paste0("cut_fb", hand_suffix), "CH/SPL" = paste0("chspl", hand_suffix),
    "overall")
}

map_pitch_to_family_key <- function(family, pitcher_hand) {
  hand_suffix <- ifelse(pitcher_hand == "Right", "_rhp", "_lhp")
  switch(family, "FB" = paste0("fb", hand_suffix), "BB" = paste0("bb", hand_suffix), "OS" = paste0("os", hand_suffix), "overall")
}

calc_detailed_matchup <- function(hitter_data, pitcher_data, hitter_splits, pitcher_splits, pitcher_hand) {
  arsenal <- identify_pitcher_arsenal(pitcher_data)
  if (is.null(arsenal)) return(NULL)
  
  breakdown <- arsenal %>% rowwise() %>% mutate(
    split_key = map_pitch_to_split_key(movement_tag, pitcher_hand),
    family_key = map_pitch_to_family_key(family, pitcher_hand),
    h_rv = if (split_key %in% names(hitter_splits)) hitter_splits[[split_key]]$rv100 else NA_real_,
    h_n = if (split_key %in% names(hitter_splits)) hitter_splits[[split_key]]$n else 0L,
    h_weight = if (split_key %in% names(hitter_splits)) hitter_splits[[split_key]]$weight else 0,
    p_rv = if (split_key %in% names(pitcher_splits)) pitcher_splits[[split_key]]$rv100 else NA_real_,
    p_n = if (split_key %in% names(pitcher_splits)) pitcher_splits[[split_key]]$n else 0L,
    p_weight = if (split_key %in% names(pitcher_splits)) pitcher_splits[[split_key]]$weight else 0,
    h_fam_rv = if (family_key %in% names(hitter_splits)) hitter_splits[[family_key]]$rv100 else NA_real_,
    matchup_rv = {
      conf <- h_weight * p_weight
      raw <- if (!is.na(h_rv) && !is.na(p_rv)) h_rv - p_rv else 0
      conf * raw + (1 - conf) * 0
    },
    weighted_contribution = matchup_rv * (pct / 100)
  ) %>% ungroup()
  
  hand_suffix <- ifelse(pitcher_hand == "Right", "_rhp", "_lhp")
  platoon_key <- paste0("vs", hand_suffix)
  
  list(
    arsenal = breakdown,
    platoon_rv = if (platoon_key %in% names(hitter_splits)) hitter_splits[[platoon_key]]$rv100 else NA_real_,
    family_summary = list(
      fb = list(rv = if (paste0("fb", hand_suffix) %in% names(hitter_splits)) hitter_splits[[paste0("fb", hand_suffix)]]$rv100 else NA_real_),
      bb = list(rv = if (paste0("bb", hand_suffix) %in% names(hitter_splits)) hitter_splits[[paste0("bb", hand_suffix)]]$rv100 else NA_real_),
      os = list(rv = if (paste0("os", hand_suffix) %in% names(hitter_splits)) hitter_splits[[paste0("os", hand_suffix)]]$rv100 else NA_real_)
    ),
    overall_rv = sum(breakdown$weighted_contribution, na.rm = TRUE),
    pitcher_hand = pitcher_hand
  )
}

# ============================================================================
# MATCHUP MATRIX CONFIGURATIONS - EXPANDED
# ============================================================================

MM_SPLIT_CONFIG <- list(
  # Fastball movement splits
  ride_fb_rhp = list(name = "Ride FB (RHP)", pitch_types = c("Fastball", "Four-Seam"), ivb_min = 18, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 80),
  ride_fb_lhp = list(name = "Ride FB (LHP)", pitch_types = c("Fastball", "Four-Seam"), ivb_min = 18, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 80),
  sink_fb_rhp = list(name = "Sink FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_max = 9, hb_min = 10, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 60),
  sink_fb_lhp = list(name = "Sink FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_max = 9, hb_min = 10, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 60),
  hard_velo_rhp = list(name = "Hard Velo (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_min = 92, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 80),
  hard_velo_lhp = list(name = "Hard Velo (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_min = 92, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 80),
  soft_fb_rhp = list(name = "Soft FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_max = 88, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 60),
  soft_fb_lhp = list(name = "Soft FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), velo_max = 88, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 60),
  cut_fb_rhp = list(name = "Cutter (RHP)", pitch_types = c("Cutter", "FC"), pitcher_hand = "Right", fallback = "fb_rhp", threshold = 50),
  cut_fb_lhp = list(name = "Cutter (LHP)", pitch_types = c("Cutter", "FC"), pitcher_hand = "Left", fallback = "fb_lhp", threshold = 50),
  deadzone_fb_rhp = list(name = "Deadzone FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_min = 10, ivb_max = 16, hb_min = 8, hb_max = 15, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "fb_rhp", threshold = 60),
  deadzone_fb_lhp = list(name = "Deadzone FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker"), ivb_min = 10, ivb_max = 16, hb_min = 8, hb_max = 15, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "fb_lhp", threshold = 60),
  
  # Breaking ball movement splits - EXPANDED
  sweep_rhp = list(name = "Sweep (RHP)", pitch_types = c("Slider", "Sweeper", "SW", "SL"), hb_min = 14, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "slider_rhp", threshold = 60),
  sweep_lhp = list(name = "Sweep (LHP)", pitch_types = c("Slider", "Sweeper", "SW", "SL"), hb_min = 14, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "slider_lhp", threshold = 60),
  downer_rhp = list(name = "Downer (RHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Slurve"), ivb_max = -6, pitcher_hand = "Right", fallback = "cb_rhp", threshold = 60),
  downer_lhp = list(name = "Downer (LHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Slurve"), ivb_max = -6, pitcher_hand = "Left", fallback = "cb_lhp", threshold = 60),
  gyro_cut_rhp = list(name = "Gyro/Cut (RHP)", pitch_types = c("Cutter", "Slider"), ivb_min = -3, ivb_max = 6, hb_max = 8, use_abs_hb = TRUE, pitcher_hand = "Right", fallback = "slider_rhp", threshold = 50),
  gyro_cut_lhp = list(name = "Gyro/Cut (LHP)", pitch_types = c("Cutter", "Slider"), ivb_min = -3, ivb_max = 6, hb_max = 8, use_abs_hb = TRUE, pitcher_hand = "Left", fallback = "slider_lhp", threshold = 50),
  
  # Specific breaking ball type splits - NEW
  slider_rhp = list(name = "Slider (RHP)", pitch_types = c("Slider", "Sweeper", "SL", "SW"), pitcher_hand = "Right", fallback = "bb_rhp", threshold = 60),
  slider_lhp = list(name = "Slider (LHP)", pitch_types = c("Slider", "Sweeper", "SL", "SW"), pitcher_hand = "Left", fallback = "bb_lhp", threshold = 60),
  cb_rhp = list(name = "Curveball (RHP)", pitch_types = c("Curveball", "CU", "CB", "KC"), pitcher_hand = "Right", fallback = "bb_rhp", threshold = 50),
  cb_lhp = list(name = "Curveball (LHP)", pitch_types = c("Curveball", "CU", "CB", "KC"), pitcher_hand = "Left", fallback = "bb_lhp", threshold = 50),
  slurve_rhp = list(name = "Slurve (RHP)", pitch_types = c("Slurve", "SV"), pitcher_hand = "Right", fallback = "cb_rhp", threshold = 40),
  slurve_lhp = list(name = "Slurve (LHP)", pitch_types = c("Slurve", "SV"), pitcher_hand = "Left", fallback = "cb_lhp", threshold = 40),
  
  # Offspeed splits
  chspl_rhp = list(name = "CH/SPL (RHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter", "CH", "FS", "SP"), pitcher_hand = "Right", fallback = "os_rhp", threshold = 60),
  chspl_lhp = list(name = "CH/SPL (LHP)", pitch_types = c("ChangeUp", "Changeup", "Splitter", "CH", "FS", "SP"), pitcher_hand = "Left", fallback = "os_lhp", threshold = 60)
)

# --- LOCATION SPLITS (zone quadrants, batter-hand dependent for in/away) ---
MM_LOCATION_CONFIG <- list(
  up_rhp = list(name = "Up (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_up = TRUE, fallback = "vs_rhp", threshold = 80),
  up_lhp = list(name = "Up (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_up = TRUE, fallback = "vs_lhp", threshold = 80),
  down_rhp = list(name = "Down (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_down = TRUE, fallback = "vs_rhp", threshold = 80),
  down_lhp = list(name = "Down (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_down = TRUE, fallback = "vs_lhp", threshold = 80),
  in_rhp = list(name = "In (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_in = TRUE, fallback = "vs_rhp", threshold = 80),
  in_lhp = list(name = "In (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_in = TRUE, fallback = "vs_lhp", threshold = 80),
  away_rhp = list(name = "Away (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_away = TRUE, fallback = "vs_rhp", threshold = 80),
  away_lhp = list(name = "Away (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_away = TRUE, fallback = "vs_lhp", threshold = 80),
  elevated_rhp = list(name = "Elevated (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_elevated = TRUE, fallback = "up_rhp", threshold = 60),
  elevated_lhp = list(name = "Elevated (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_elevated = TRUE, fallback = "up_lhp", threshold = 60),
  heart_rhp = list(name = "Heart (RHP)", pitch_types = NULL, pitcher_hand = "Right", loc_heart = TRUE, fallback = "vs_rhp", threshold = 80),
  heart_lhp = list(name = "Heart (LHP)", pitch_types = NULL, pitcher_hand = "Left", loc_heart = TRUE, fallback = "vs_lhp", threshold = 80)
)

MM_FAMILY_CONFIG <- list(
  fb_rhp = list(name = "All FB (RHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker", "Cutter"), pitcher_hand = "Right", fallback = "vs_rhp", threshold = 100),
  fb_lhp = list(name = "All FB (LHP)", pitch_types = c("Fastball", "Four-Seam", "Sinker", "Cutter"), pitcher_hand = "Left", fallback = "vs_lhp", threshold = 100),
  bb_rhp = list(name = "All BB (RHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Cutter", "Slurve"), pitcher_hand = "Right", fallback = "vs_rhp", threshold = 80),
  bb_lhp = list(name = "All BB (LHP)", pitch_types = c("Slider", "Curveball", "Sweeper", "Cutter", "Slurve"), pitcher_hand = "Left", fallback = "vs_lhp", threshold = 80),
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

MM_ALL_SPLITS <- c(MM_SPLIT_CONFIG, MM_LOCATION_CONFIG, MM_FAMILY_CONFIG, MM_PLATOON_CONFIG, MM_OVERALL_CONFIG)

MM_DISPLAY_GROUPS <- list(
  "FB Movement (RHP)" = c("ride_fb_rhp", "sink_fb_rhp", "hard_velo_rhp", "soft_fb_rhp", "cut_fb_rhp", "deadzone_fb_rhp"),
  "FB Movement (LHP)" = c("ride_fb_lhp", "sink_fb_lhp", "hard_velo_lhp", "soft_fb_lhp", "cut_fb_lhp", "deadzone_fb_lhp"),
  "BB Movement (RHP)" = c("sweep_rhp", "downer_rhp", "gyro_cut_rhp"),
  "BB Movement (LHP)" = c("sweep_lhp", "downer_lhp", "gyro_cut_lhp"),
  "BB Type (RHP)" = c("slider_rhp", "cb_rhp", "slurve_rhp"),
  "BB Type (LHP)" = c("slider_lhp", "cb_lhp", "slurve_lhp"),
  "OS (RHP/LHP)" = c("chspl_rhp", "chspl_lhp"),
  "Pitch Family (RHP)" = c("fb_rhp", "bb_rhp", "os_rhp"),
  "Pitch Family (LHP)" = c("fb_lhp", "bb_lhp", "os_lhp"),
  "Location (RHP)" = c("up_rhp", "down_rhp", "in_rhp", "away_rhp", "elevated_rhp", "heart_rhp"),
  "Location (LHP)" = c("up_lhp", "down_lhp", "in_lhp", "away_lhp", "elevated_lhp", "heart_lhp"),
  "Platoon" = c("vs_rhp", "vs_lhp"),
  "Overall" = c("overall")
)

# ============================================================================
# MATCHUP MATRIX CORE FUNCTIONS
# ============================================================================

classify_location_zone <- function(plate_loc_side, plate_loc_height, batter_side = "Right") {
  # Returns a vector of zone labels
  # Priority: most specific first (heart, elevated), then quadrant (up/down/in/out)
  
  # Determine in/out based on batter side
  # For RHH: negative PlateLocSide = inside, positive = outside

  # For LHH: positive PlateLocSide = inside, negative = outside
  in_sign <- ifelse(batter_side == "Right", -1, 1)
  
  zone <- case_when(
    # Heart zone: inner 2/3 of zone, middle 2/3 of height
    abs(plate_loc_side) < 0.55 & plate_loc_height >= 1.83 & plate_loc_height <= 3.17 ~ "Heart",
    # Elevated: above the zone
    plate_loc_height > 3.5 ~ "Elevated",
    # Below zone
    plate_loc_height < 1.5 ~ "Below",
    # Up half of zone
    plate_loc_height >= 2.5 & plate_loc_height <= 3.5 ~ "Up",
    # Down half of zone
    plate_loc_height >= 1.5 & plate_loc_height < 2.5 ~ "Down",
    TRUE ~ "Middle"
  )
  
  # Add in/out dimension for non-heart, non-elevated, non-below pitches
  in_out <- case_when(
    zone %in% c("Heart", "Elevated", "Below") ~ zone,
    (plate_loc_side * in_sign) < -0.28 ~ paste0(zone, "-In"),
    (plate_loc_side * in_sign) > 0.28 ~ paste0(zone, "-Out"),
    TRUE ~ zone
  )
  
  in_out
}

# Simplified zone for the tendency card grid (fewer cells, more samples per cell)
classify_tendency_zone <- function(plate_loc_side, plate_loc_height, batter_side = "Right") {
  in_sign <- ifelse(batter_side == "Right", -1, 1)
  
  case_when(
    plate_loc_height > 3.5 ~ "Elevated",
    plate_loc_height < 1.5 & (plate_loc_side * in_sign) < 0 ~ "GloveDown",
    plate_loc_height < 1.5 & (plate_loc_side * in_sign) >= 0 ~ "ArmDown",
    abs(plate_loc_side) < 0.55 & plate_loc_height >= 1.83 & plate_loc_height <= 3.17 ~ "Heart",
    plate_loc_height >= 2.5 & (plate_loc_side * in_sign) < 0 ~ "UpIn",
    plate_loc_height >= 2.5 & (plate_loc_side * in_sign) >= 0 ~ "UpAway",
    plate_loc_height < 2.5 & (plate_loc_side * in_sign) < 0 ~ "DownIn",
    plate_loc_height < 2.5 & (plate_loc_side * in_sign) >= 0 ~ "DownAway",
    TRUE ~ "Heart"
  )
}

log5_rate <- function(hitter_rate, league_rate) {
  # Log5 formula: estimates true matchup rate given hitter rate and league baseline
  # This removes the "dominant pitcher drags everything" problem by normalizing
  # When used: compute hitter_rate for a cell, league_rate for same cell,
  # the adjusted rate tells you how this hitter deviates from average for that situation
  
  if (is.na(hitter_rate) || is.na(league_rate) || league_rate == 0 || league_rate == 1) {
    return(hitter_rate)
  }
  
  # Clamp to avoid division issues
  h <- pmax(0.001, pmin(0.999, hitter_rate))
  l <- pmax(0.001, pmin(0.999, league_rate))
  
  # Log5: p = (h * l) / (h * l + (1-h) * (1-l))
  # But for single-side (hitter only), we express as odds ratio vs league
  # Simplified: adjusted = h (we keep raw rate, but express the LOG5 MATCHUP
  # when overlaying pitcher later)
  # For the tendency card itself, we store both raw and the log5-ready odds ratio
  
  odds_ratio <- (h / (1 - h)) / (l / (1 - l))
  
  odds_ratio
}

log5_matchup_rate <- function(hitter_rate, pitcher_rate, league_rate) {
  # Full Log5: given hitter rate, pitcher rate, and league rate,
  # estimate the expected rate in this specific matchup
  
  if (is.na(hitter_rate) || is.na(pitcher_rate) || is.na(league_rate)) return(NA_real_)
  if (league_rate == 0 || league_rate == 1) return(hitter_rate)
  
  h <- pmax(0.001, pmin(0.999, hitter_rate))
  p <- pmax(0.001, pmin(0.999, pitcher_rate))
  l <- pmax(0.001, pmin(0.999, league_rate))
  
  # Log5 formula
  num <- h * p / l
  denom <- h * p / l + (1 - h) * (1 - p) / (1 - l)
  
  num / denom
}

# ---------------------------------------------------------------------------
# TENDENCY CARD CALCULATION
# ---------------------------------------------------------------------------

# Calculate league-wide rates for Log5 baseline
calc_league_baseline <- function(pool_data, min_n = 50) {
  # Pool data should already have indicators added
  
  shapes <- c("Ride", "Sink", "Sweep", "Downer", "Cutter", "CH/SPL", "Slider", "Curveball", "Overall")
  zones <- c("Elevated", "UpIn", "UpAway", "Heart", "DownIn", "DownAway", "GloveDown", "ArmDown", "Overall")
  counts <- c("first_pitch", "early", "two_strike", "ahead", "behind", "overall")
  
  # Overall league rates as fallback
  n_swings <- sum(pool_data$is_swing, na.rm = TRUE)
  n_ooz <- sum(pool_data$out_of_zone, na.rm = TRUE)
  n_bip <- sum(pool_data$PitchCall == "InPlay", na.rm = TRUE)
  
  list(
    whiff_rate = sum(pool_data$is_whiff, na.rm = TRUE) / pmax(1, n_swings),
    chase_rate = sum(pool_data$chase, na.rm = TRUE) / pmax(1, n_ooz),
    swing_rate = n_swings / pmax(1, nrow(pool_data)),
    damage_rate = sum(pool_data$PitchCall == "InPlay" & pool_data$ExitSpeed >= 95, na.rm = TRUE) / pmax(1, n_bip),
    iz_whiff_rate = sum(pool_data$in_zone_whiff, na.rm = TRUE) / pmax(1, sum(pool_data$z_swing, na.rm = TRUE)),
    woba = mean(pool_data$woba, na.rm = TRUE),
    n = nrow(pool_data)
  )
}

# Main tendency card builder
calc_hitter_tendency_card <- function(hitter_name, tm_data, league_baseline, min_n = 10) {
  
  h_data <- tm_data %>% filter(Batter == hitter_name)
  if (nrow(h_data) < 30) return(NULL)
  
  batter_side <- detect_batter_handedness(hitter_name, tm_data)
  
  # Add movement tag and tendency zone
  h_data <- h_data %>%
    mutate(
      movement_tag = case_when(
        PitchFamily == "FB" & InducedVertBreak >= 18 ~ "Ride",
        PitchFamily == "FB" & InducedVertBreak < 9 & abs(HorzBreak) >= 10 ~ "Sink",
        PitchFamily == "FB" ~ "FB-Other",
        TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") & abs(HorzBreak) >= 14 ~ "Sweep",
        TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") ~ "Slider",
        TaggedPitchType %in% c("Curveball", "CU", "CB", "KC") ~ "Curveball",
        TaggedPitchType %in% c("Curveball", "Slider", "Sweeper", "Slurve") & InducedVertBreak <= -6 ~ "Downer",
        TaggedPitchType %in% c("Cutter", "FC") ~ "Cutter",
        PitchFamily == "OS" ~ "CH/SPL",
        TRUE ~ "Other"
      ),
      tendency_zone = classify_tendency_zone(PlateLocSide, PlateLocHeight, batter_side),
      count_state = case_when(
        Balls == 0 & Strikes == 0 ~ "first_pitch",
        Strikes < 2 ~ "early",
        Strikes == 2 ~ "two_strike",
        Balls > Strikes ~ "behind",
        Strikes > Balls ~ "ahead",
        TRUE ~ "early"
      )
    )
  
  # Define groupings
  shape_groups <- list(
    "Ride FB" = c("Ride"),
    "Sink/Run" = c("Sink", "FB-Other"),
    "Sweep" = c("Sweep"),
    "Slider" = c("Slider"),
    "Curveball" = c("Curveball", "Downer"),
    "Cutter" = c("Cutter"),
    "CH/SPL" = c("CH/SPL"),
    "All FB" = c("Ride", "Sink", "FB-Other", "Cutter"),
    "All BB" = c("Sweep", "Slider", "Curveball", "Downer"),
    "All OS" = c("CH/SPL")
  )
  
  zone_groups <- c("Elevated", "UpIn", "UpAway", "Heart", "DownIn", "DownAway", "GloveDown", "ArmDown")
  count_groups <- c("first_pitch", "early", "two_strike", "ahead", "behind")
  
  # Calculate rates for each cell
  calc_cell_rates <- function(cell_data, baseline) {
    n <- nrow(cell_data)
    if (n < min_n) {
      return(list(
        n = n, swing_rate = NA, whiff_rate = NA, chase_rate = NA,
        damage_rate = NA, woba = NA, iz_whiff_rate = NA,
        swing_or = NA, whiff_or = NA, chase_or = NA, damage_or = NA,
        label = "", vulnerability = "unknown"
      ))
    }
    
    n_swings <- sum(cell_data$is_swing, na.rm = TRUE)
    n_ooz <- sum(cell_data$out_of_zone, na.rm = TRUE)
    n_iz <- sum(cell_data$in_zone, na.rm = TRUE)
    n_iz_swing <- sum(cell_data$z_swing, na.rm = TRUE)
    n_bip <- sum(cell_data$PitchCall == "InPlay", na.rm = TRUE)
    
    swing_rate <- n_swings / pmax(1, n)
    whiff_rate <- sum(cell_data$is_whiff, na.rm = TRUE) / pmax(1, n_swings)
    chase_rate <- if (n_ooz >= 5) sum(cell_data$chase, na.rm = TRUE) / n_ooz else NA
    damage_rate <- if (n_bip >= 3) sum(cell_data$PitchCall == "InPlay" & cell_data$ExitSpeed >= 95, na.rm = TRUE) / n_bip else NA
    iz_whiff_rate <- if (n_iz_swing >= 5) sum(cell_data$in_zone_whiff, na.rm = TRUE) / n_iz_swing else NA
    woba_val <- if (sum(!is.na(cell_data$woba)) >= 5) mean(cell_data$woba, na.rm = TRUE) else NA
    
    # Log5 odds ratios vs league
    swing_or <- log5_rate(swing_rate, baseline$swing_rate)
    whiff_or <- log5_rate(whiff_rate, baseline$whiff_rate)
    chase_or <- if (!is.na(chase_rate)) log5_rate(chase_rate, baseline$chase_rate) else NA
    damage_or <- if (!is.na(damage_rate)) log5_rate(damage_rate, baseline$damage_rate) else NA
    
    # Classify vulnerability
    vulnerability <- case_when(
      !is.na(whiff_or) && whiff_or > 1.5 && !is.na(chase_or) && chase_or > 1.3 ~ "high_vuln",
      !is.na(whiff_or) && whiff_or > 1.3 ~ "vuln_whiff",
      !is.na(chase_or) && chase_or > 1.4 ~ "vuln_chase",
      !is.na(damage_or) && damage_or > 1.5 ~ "strength",
      !is.na(damage_or) && damage_or > 1.2 && !is.na(woba_val) && woba_val > 0.370 ~ "strength",
      !is.na(whiff_or) && whiff_or < 0.7 && !is.na(damage_or) && damage_or > 1.2 ~ "strength",
      TRUE ~ "neutral"
    )
    
    list(
      n = n,
      swing_rate = round(swing_rate * 100, 1),
      whiff_rate = round(whiff_rate * 100, 1),
      chase_rate = if (!is.na(chase_rate)) round(chase_rate * 100, 1) else NA,
      damage_rate = if (!is.na(damage_rate)) round(damage_rate * 100, 1) else NA,
      iz_whiff_rate = if (!is.na(iz_whiff_rate)) round(iz_whiff_rate * 100, 1) else NA,
      woba = if (!is.na(woba_val)) round(woba_val, 3) else NA,
      swing_or = swing_or,
      whiff_or = whiff_or,
      chase_or = chase_or,
      damage_or = damage_or,
      vulnerability = vulnerability
    )
  }
  
  # Build the card: shape x count
  shape_count_card <- list()
  for (shape_name in names(shape_groups)) {
    shape_tags <- shape_groups[[shape_name]]
    shape_data <- h_data %>% filter(movement_tag %in% shape_tags)
    
    shape_count_card[[shape_name]] <- list()
    
    # Overall for this shape
    shape_count_card[[shape_name]][["overall"]] <- calc_cell_rates(shape_data, league_baseline)
    
    # By count
    for (ct in count_groups) {
      ct_data <- shape_data %>% filter(count_state == ct)
      shape_count_card[[shape_name]][[ct]] <- calc_cell_rates(ct_data, league_baseline)
    }
  }
  
  # Build the card: shape x zone
  shape_zone_card <- list()
  for (shape_name in names(shape_groups)) {
    shape_tags <- shape_groups[[shape_name]]
    shape_data <- h_data %>% filter(movement_tag %in% shape_tags)
    
    shape_zone_card[[shape_name]] <- list()
    for (zone in zone_groups) {
      zone_data <- shape_data %>% filter(tendency_zone == zone)
      shape_zone_card[[shape_name]][[zone]] <- calc_cell_rates(zone_data, league_baseline)
    }
  }
  
  # Build the card: zone x count (pitch-type agnostic)
  zone_count_card <- list()
  for (zone in zone_groups) {
    zone_data <- h_data %>% filter(tendency_zone == zone)
    zone_count_card[[zone]] <- list()
    zone_count_card[[zone]][["overall"]] <- calc_cell_rates(zone_data, league_baseline)
    for (ct in count_groups) {
      ct_data <- zone_data %>% filter(count_state == ct)
      zone_count_card[[zone]][[ct]] <- calc_cell_rates(ct_data, league_baseline)
    }
  }
  
  # Overall stats
  overall <- calc_cell_rates(h_data, league_baseline)
  
  # Identify top vulnerabilities and strengths
  vuln_cells <- list()
  strength_cells <- list()
  
  for (shape_name in names(shape_count_card)) {
    for (ct in names(shape_count_card[[shape_name]])) {
      cell <- shape_count_card[[shape_name]][[ct]]
      if (cell$n >= min_n && !is.na(cell$vulnerability)) {
        cell_label <- paste0(shape_name, " / ", ct)
        if (cell$vulnerability %in% c("high_vuln", "vuln_whiff", "vuln_chase")) {
          vuln_cells[[cell_label]] <- cell
        } else if (cell$vulnerability == "strength") {
          strength_cells[[cell_label]] <- cell
        }
      }
    }
  }
  
  list(
    name = hitter_name,
    batter_side = batter_side,
    n_total = nrow(h_data),
    overall = overall,
    shape_count = shape_count_card,
    shape_zone = shape_zone_card,
    zone_count = zone_count_card,
    vulnerabilities = vuln_cells,
    strengths = strength_cells,
    shape_groups = shape_groups
  )
}

# ---------------------------------------------------------------------------
# PITCHER OVERLAY — applies pitcher arsenal to hitter tendency card
# ---------------------------------------------------------------------------

overlay_pitcher_on_tendency <- function(tendency_card, pitcher_name, tm_data, league_baseline) {
  p_data <- tm_data %>% filter(Pitcher == pitcher_name)
  if (nrow(p_data) < 30 || is.null(tendency_card)) return(NULL)
  
  p_hand <- names(sort(table(p_data$PitcherThrows[!is.na(p_data$PitcherThrows)]), decreasing = TRUE))[1]
  batter_side <- tendency_card$batter_side
  
  # Get pitcher's arsenal with movement tags
  p_data <- p_data %>%
    mutate(
      movement_tag = case_when(
        classify_pitch_family(TaggedPitchType) == "FB" & InducedVertBreak >= 18 ~ "Ride",
        classify_pitch_family(TaggedPitchType) == "FB" & InducedVertBreak < 9 & abs(HorzBreak) >= 10 ~ "Sink",
        classify_pitch_family(TaggedPitchType) == "FB" ~ "FB-Other",
        TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") & abs(HorzBreak) >= 14 ~ "Sweep",
        TaggedPitchType %in% c("Slider", "Sweeper", "SL", "SW") ~ "Slider",
        TaggedPitchType %in% c("Curveball", "CU", "CB", "KC") ~ "Curveball",
        TaggedPitchType %in% c("Curveball", "Slider", "Sweeper", "Slurve") & InducedVertBreak <= -6 ~ "Downer",
        TaggedPitchType %in% c("Cutter", "FC") ~ "Cutter",
        classify_pitch_family(TaggedPitchType) == "OS" ~ "CH/SPL",
        TRUE ~ "Other"
      ),
      tendency_zone = classify_tendency_zone(PlateLocSide, PlateLocHeight, batter_side),
      count_state = case_when(
        Balls == 0 & Strikes == 0 ~ "first_pitch",
        Strikes < 2 ~ "early",
        Strikes == 2 ~ "two_strike",
        Balls > Strikes ~ "behind",
        Strikes > Balls ~ "ahead",
        TRUE ~ "early"
      )
    )
  
  # Pitcher arsenal profile
  arsenal <- p_data %>%
    filter(!is.na(movement_tag), movement_tag != "Other") %>%
    group_by(movement_tag) %>%
    summarise(
      n = n(),
      pct = n() / nrow(p_data) * 100,
      avg_velo = mean(RelSpeed, na.rm = TRUE),
      avg_ivb = mean(InducedVertBreak, na.rm = TRUE),
      avg_hb = mean(HorzBreak, na.rm = TRUE),
      # Pitcher's own rates
      whiff_rate = sum(is_whiff, na.rm = TRUE) / pmax(1, sum(is_swing, na.rm = TRUE)),
      zone_pct = sum(in_zone, na.rm = TRUE) / n(),
      .groups = "drop"
    ) %>%
    filter(pct >= 3)
  
  # Pitcher usage by count
  usage_by_count <- p_data %>%
    filter(!is.na(movement_tag), movement_tag != "Other") %>%
    group_by(count_state, movement_tag) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(count_state) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
  
  # Pitcher location tendencies by pitch shape
  location_by_shape <- p_data %>%
    filter(!is.na(movement_tag), movement_tag != "Other", !is.na(tendency_zone)) %>%
    group_by(movement_tag, tendency_zone) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(movement_tag) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
  
  # Match pitcher pitches to hitter tendency card
  # For each pitch the pitcher throws, find the hitter's tendency in that shape x zone x count
  shape_groups <- tendency_card$shape_groups
  
  matchup_recommendations <- list()
  
  count_labels <- c("first_pitch" = "First Pitch", "early" = "Early Count",
                    "two_strike" = "2 Strikes", "ahead" = "Ahead", "behind" = "Behind")
  
  for (ct in c("first_pitch", "early", "two_strike")) {
    ct_usage <- usage_by_count %>% filter(count_state == ct)
    if (nrow(ct_usage) == 0) next
    
    recs <- list()
    for (i in seq_len(nrow(ct_usage))) {
      pitch_tag <- ct_usage$movement_tag[i]
      pitch_pct <- ct_usage$pct[i]
      
      # Find which shape group this pitch belongs to
      matching_group <- NULL
      for (sg_name in names(shape_groups)) {
        if (pitch_tag %in% shape_groups[[sg_name]]) {
          matching_group <- sg_name
          break
        }
      }
      if (is.null(matching_group)) next
      
      # Get hitter's rates for this shape in this count
      h_cell <- tendency_card$shape_count[[matching_group]][[ct]]
      if (is.null(h_cell) || h_cell$n < 5) {
        h_cell <- tendency_card$shape_count[[matching_group]][["overall"]]
      }
      
      # Get pitcher's whiff rate for this pitch
      p_pitch <- arsenal %>% filter(movement_tag == pitch_tag)
      p_whiff <- if (nrow(p_pitch) > 0) p_pitch$whiff_rate[1] else NA
      
      # Log5 matchup whiff rate
      matchup_whiff <- log5_matchup_rate(
        h_cell$whiff_rate / 100,
        if (!is.na(p_whiff)) p_whiff else league_baseline$whiff_rate,
        league_baseline$whiff_rate
      )
      
      # Best location for this pitch against this hitter
      loc_tend <- location_by_shape %>% filter(movement_tag == pitch_tag)
      best_zone <- NULL
      if (nrow(loc_tend) > 0) {
        # Find zones where pitcher locates AND hitter is vulnerable
        for (j in seq_len(nrow(loc_tend))) {
          zone <- loc_tend$tendency_zone[j]
          zone_pct <- loc_tend$pct[j]
          h_zone_cell <- tendency_card$shape_zone[[matching_group]][[zone]]
          if (!is.null(h_zone_cell) && h_zone_cell$n >= 5) {
            if (h_zone_cell$vulnerability %in% c("high_vuln", "vuln_whiff", "vuln_chase") && zone_pct >= 10) {
              best_zone <- list(zone = zone, pct = zone_pct, h_vuln = h_zone_cell$vulnerability,
                               h_whiff = h_zone_cell$whiff_rate, h_chase = h_zone_cell$chase_rate)
              break
            }
          }
        }
      }
      
      recs[[pitch_tag]] <- list(
        pitch = pitch_tag,
        shape_group = matching_group,
        usage_pct = pitch_pct,
        h_whiff = h_cell$whiff_rate,
        h_chase = h_cell$chase_rate,
        h_damage = h_cell$damage_rate,
        h_woba = h_cell$woba,
        h_vuln = h_cell$vulnerability,
        matchup_whiff = if (!is.na(matchup_whiff)) round(matchup_whiff * 100, 1) else NA,
        best_zone = best_zone
      )
    }
    
    matchup_recommendations[[ct]] <- recs
  }
  
  list(
    pitcher_name = pitcher_name,
    pitcher_hand = p_hand,
    hitter_name = tendency_card$name,
    arsenal = arsenal,
    usage_by_count = usage_by_count,
    location_by_shape = location_by_shape,
    recommendations = matchup_recommendations
  )
}

                
mm_calc_count_rv <- function(player_data, rv_col, count_type) {

  if (nrow(player_data) == 0) return(list(rv100 = NA_real_, n = 0L))
  
  filtered <- switch(count_type,
    "0-0" = player_data %>% filter(Balls == 0, Strikes == 0),
    "hitters" = player_data %>% filter(Balls > Strikes),
    "pitchers" = player_data %>% filter(Strikes > Balls),
    "2k" = player_data %>% filter(Strikes == 2),
    player_data
  )
  
  rv <- filtered[[rv_col]][!is.na(filtered[[rv_col]])]
  n <- length(rv)
  if (n == 0) return(list(rv100 = NA_real_, n = 0L))
  list(rv100 = 100 * mean(rv), n = n)
}

# Calculate all count splits for a player, optionally filtered by pitcher hand
mm_calc_count_splits <- function(player_data, rv_col, pitcher_hand = NULL, is_pitcher = FALSE) {
  if (nrow(player_data) == 0) return(list())
  
  filtered <- player_data
  if (!is.null(pitcher_hand)) {
    if (is_pitcher) {
      batter_side <- ifelse(pitcher_hand == "Right", "Right", "Left")
      if ("BatterSide" %in% names(filtered)) {
        filtered <- filtered %>% filter(BatterSide == batter_side)
      }
    } else {
      filtered <- filtered %>% filter(PitcherThrows == pitcher_hand)
    }
  }
  
  count_types <- c("0-0", "hitters", "pitchers", "2k")
  count_labels <- c("0-0" = "0-0", "hitters" = "Hitter Ct", "pitchers" = "Pitcher Ct", "2k" = "2 Strikes")
  
  results <- lapply(count_types, function(ct) {
    res <- mm_calc_count_rv(filtered, rv_col, ct)
    res$name <- count_labels[[ct]]
    res$count_type <- ct
    res
  })
  names(results) <- count_types
  results
}
  
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


# Helper function to classify pitches into detailed categories for heatmaps
classify_detailed_pitch <- function(pitch_type) {
  case_when(
    pitch_type %in% c("Fastball", "Four-Seam", "FourSeamFastBall", "FF", "FB") ~ "4S",
    pitch_type %in% c("TwoSeamFastBall", "Sinker", "SI", "FT") ~ "2S/Si",
    pitch_type %in% c("Slider", "Sweeper", "SL", "SW") ~ "SL/SW",
    pitch_type %in% c("Curveball", "Slurve", "CU", "CB", "KC", "SV") ~ "CB",
    pitch_type %in% c("Changeup", "ChangeUp", "Splitter", "CH", "FS", "SP") ~ "CH/Spl",
    pitch_type %in% c("Cutter", "FC") ~ "SL/SW",  # Group cutters with sliders
    TRUE ~ "Other"
  )
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
.mm-main-tabs > .nav-pills > li.active > a {
    background: #006F71 !important; color: white !important;
    border-color: #006F71;
  }

  /* Tendency Cards Tab */
  .tc-controls { background: white; border-radius: 12px; padding: 20px; margin-bottom: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05); border: 1px solid rgba(0,111,113,.2); }
  .tc-card { background: white; border-radius: 12px; padding: 16px; margin-bottom: 15px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05); border: 1px solid rgba(0,111,113,.2); }
  .tc-card h3 { color: #006F71; font-weight: 800; font-size: 1.1rem;
    border-bottom: 2px solid #006F71; padding-bottom: 8px; margin: 0 0 12px; }
  .tc-grid-table { width: 100%; border-collapse: collapse; font-size: 11px; }
  .tc-grid-table th { background: #006F71; color: white; padding: 6px 8px; text-align: center;
    font-weight: 600; font-size: 10px; }
  .tc-grid-table td { padding: 5px 6px; text-align: center; border: 1px solid #e0e0e0; font-size: 11px; }
  .tc-grid-table tr:nth-child(even) { background: #f9f9f9; }
  .tc-grid-table .row-label { text-align: left; font-weight: 700; background: #f0f0f0;
    color: #333; min-width: 70px; }
  .tc-vuln { background: #FFCDD2 !important; color: #B71C1C; font-weight: 700; }
  .tc-high-vuln { background: #EF9A9A !important; color: #B71C1C; font-weight: 800; }
  .tc-strength { background: #C8E6C9 !important; color: #1B5E20; font-weight: 700; }
  .tc-neutral { background: #FFF9C4; color: #F57F17; }
  .tc-empty { background: #f5f5f5; color: #bbb; }
  .tc-badge { display: inline-block; padding: 2px 8px; border-radius: 12px; font-size: 10px;
    font-weight: 600; margin: 1px; }
  .tc-badge-vuln { background: #FFCDD2; color: #C62828; }
  .tc-badge-strength { background: #C8E6C9; color: #2E7D32; }
  .tc-badge-neutral { background: #E0E0E0; color: #666; }
  .tc-rec-card { background: #f8fafa; border-radius: 8px; padding: 10px 14px; margin-bottom: 8px;
    border-left: 4px solid #006F71; }
  .tc-rec-card.rec-good { border-left-color: #2E7D32; }
  .tc-rec-card.rec-bad { border-left-color: #C62828; }
  .tc-rec-card.rec-neutral { border-left-color: #F57F17; }
  .tc-rec-pitch { font-weight: 700; font-size: 13px; }
  .tc-rec-detail { font-size: 11px; color: #555; margin-top: 3px; }
  .tc-summary-box { background: linear-gradient(135deg, #006F71, #004d4e); color: white;
    border-radius: 10px; padding: 14px 18px; margin-bottom: 15px; }
  .tc-summary-box h4 { margin: 0 0 8px; font-size: 15px; }
  .tc-summary-box p { margin: 4px 0; font-size: 12px; opacity: 0.95; }
  .tc-overlay-header { background: linear-gradient(135deg, #37474F, #546E7A); color: white;
    padding: 10px 16px; border-radius: 8px; margin-bottom: 12px; }
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

  div(style = "padding: 8px 20px; background: #f8f9fa; border-bottom: 1px solid #ddd; display: flex; align-items: center; gap: 15px;",
      tags$label(style = "font-weight: 700; color: #006F71; margin: 0;", "Data Source:"),
      checkboxGroupInput("data_source", NULL,
                         choices = c("2025" = "2025", "2026" = "2026"),
                         selected = "2026", inline = TRUE),
      actionButton("refresh_data", "Refresh", icon = icon("refresh"),
                   class = "btn-info btn-sm"),
      textOutput("data_source_info")
  ),
  
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

      # ====== TAB 3: PITCHER-HITTER MATCHUPS ======
      tabPanel("Pitcher-Hitter Matchups",
        div(style = "padding: 15px; max-width: 1400px; margin: 0 auto;",
          div(class = "mm-controls-card",
            fluidRow(
              column(3,
                tags$label("Mode"),
                radioButtons("phm_mode", NULL, 
                  choices = c("One Hitter vs Pitchers" = "one_hitter", "One Pitcher vs Hitters" = "one_pitcher"),
                  selected = "one_hitter", inline = TRUE)
              ),
              column(4,
                conditionalPanel("input.phm_mode == 'one_hitter'",
                  tags$label("Select Hitter"),
                  selectizeInput("phm_hitter", NULL, choices = NULL, multiple = FALSE,
                    options = list(placeholder = "Select one hitter..."))
                ),
                conditionalPanel("input.phm_mode == 'one_pitcher'",
                  tags$label("Select Hitters"),
                  selectizeInput("phm_hitters_multi", NULL, choices = NULL, multiple = TRUE,
                    options = list(placeholder = "Select hitters...", maxItems = 15))
                )
              ),
              column(4,
                conditionalPanel("input.phm_mode == 'one_hitter'",
                  tags$label("Select Pitchers"),
                  selectizeInput("phm_pitchers_multi", NULL, choices = NULL, multiple = TRUE,
                    options = list(placeholder = "Select pitchers...", maxItems = 15))
                ),
                conditionalPanel("input.phm_mode == 'one_pitcher'",
                  tags$label("Select Pitcher"),
                  selectizeInput("phm_pitcher", NULL, choices = NULL, multiple = FALSE,
                    options = list(placeholder = "Select one pitcher..."))
                )
              ),
              column(1,
                div(style = "margin-top: 22px;",
                  actionButton("phm_analyze", "Analyze", class = "btn-mm-analyze"))
              )
            )
          ),
          
          div(class = "mm-status-bar", textOutput("phm_status")),
          
          conditionalPanel("input.phm_analyze > 0",
            div(class = "mm-results-card",
              h3(textOutput("phm_results_title")),
              div(style = "margin-top: 16px;", gt_output("phm_arsenal_table")),
              hr(),
              div(style = "margin-top: 16px;", gt_output("phm_breakdown_table"))
            )
          ),
          
          conditionalPanel("input.phm_analyze == 0",
            div(class = "mm-empty-state",
              div(class = "icon", icon("baseball")),
              p("Select a hitter and pitchers (or vice versa), then click Analyze"),
              p(style = "font-size: 12px; color: #999;", 
                "This tab identifies each pitcher's arsenal and shows how the hitter performs against each specific pitch shape")
            )
          )
        )
      ),

      tabPanel("Tendency Cards",
  div(style = "padding: 15px; max-width: 1400px; margin: 0 auto;",
    
    # Controls
    div(class = "tc-controls",
      fluidRow(
        column(4,
          tags$label(style = "font-weight: 700; color: #006F71;", "Select Hitter"),
          selectizeInput("tc_hitter", NULL, choices = NULL, multiple = FALSE,
            options = list(placeholder = "Search hitter..."))
        ),
        column(3,
          tags$label(style = "font-weight: 700; color: #006F71;", "Overlay Pitcher (Optional)"),
          selectizeInput("tc_pitcher", NULL, choices = NULL, multiple = FALSE,
            options = list(placeholder = "Select pitcher to overlay..."))
        ),
        column(2,
          tags$label(style = "font-weight: 700; color: #006F71;", "Primary Metric"),
          selectInput("tc_metric", NULL,
            choices = c("Whiff%" = "whiff_rate", "Chase%" = "chase_rate",
                        "Damage%" = "damage_rate", "wOBA" = "woba",
                        "Swing%" = "swing_rate"),
            selected = "whiff_rate")
        ),
        column(1,
          div(style = "margin-top: 22px;",
            actionButton("tc_build", "Build Card", class = "btn-mm-analyze"))
        ),
        column(2,
          div(style = "margin-top: 22px;",
            downloadButton("tc_download_pdf", "Download PDF", class = "btn-mm-download"))
        )
      )
    ),
    
    # Results
    conditionalPanel("input.tc_build > 0",
      
      # Summary box
      uiOutput("tc_summary"),
      
      fluidRow(
        # Left column: Shape x Count grid
        column(6,
          div(class = "tc-card",
            h3("Pitch Shape × Count Tendencies"),
            div(style = "overflow-x: auto;", uiOutput("tc_shape_count_grid"))
          )
        ),
        # Right column: Shape x Zone grid
        column(6,
          div(class = "tc-card",
            h3("Pitch Shape × Zone Tendencies"),
            div(style = "overflow-x: auto;", uiOutput("tc_shape_zone_grid"))
          )
        )
      ),
      
      fluidRow(
        # Zone x Count grid
        column(6,
          div(class = "tc-card",
            h3("Zone × Count (All Pitch Types)"),
            div(style = "overflow-x: auto;", uiOutput("tc_zone_count_grid"))
          )
        ),
        # Vulnerabilities & Strengths summary
        column(6,
          div(class = "tc-card",
            h3("Key Vulnerabilities & Strengths"),
            uiOutput("tc_vuln_summary")
          )
        )
      ),
      
      # Pitcher overlay section (conditional)
      conditionalPanel("input.tc_pitcher != ''",
        div(class = "tc-card",
          h3(textOutput("tc_overlay_title")),
          uiOutput("tc_pitcher_overlay")
        )
      )
    ),
    
    # Empty state
    conditionalPanel("input.tc_build == 0",
      div(class = "mm-empty-state",
        div(class = "icon", icon("id-card")),
        p("Select a hitter and click Build Card to generate their tendency profile"),
        p(style = "font-size: 12px; color: #999;",
          "Tendency cards show hitter rates by pitch shape, zone, and count — Log5 adjusted against league baselines. ",
          "Optionally overlay a pitcher's arsenal to get targeted sequence recommendations.")
      )
    )
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
                  options = list(placeholder = "Select hitters...", maxItems = 15))
              ),
              column(4,
                tags$label("Pitchers"),
                selectizeInput("mm_pitchers", NULL, choices = NULL, multiple = TRUE,
                  options = list(placeholder = "Select pitchers...", maxItems = 15))
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
              fluidRow(
                column(3,
                  textAreaInput("mm_pdf_best_matchups", "Good Matchups for Coastal",
                    placeholder = "e.g., Smith vs Johnson - favorable on breaking balls",
                    rows = 4, width = "100%")
                ),
                column(3,
                  textAreaInput("mm_pdf_worst_matchups", "Bad Matchups for Coastal",
                    placeholder = "e.g., Davis vs Miller - struggles with high velo",
                    rows = 4, width = "100%")
                ),
                column(3,
                  textAreaInput("mm_pdf_avail_rhp", "Available RHP",
                    placeholder = "e.g., Horn, Doran, Appelman, Parker",
                    rows = 4, width = "100%")
                ),
                column(3,
                  textAreaInput("mm_pdf_avail_lhp", "Available LHP",
                    placeholder = "e.g., Richardson, Bosch, Winer, Karr",
                    rows = 4, width = "100%")
                )
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

  data_2026_rv <- reactiveVal(data_2026)
  
  output$page <- renderUI({
    if (logged_in()) {
      app_ui
    } else {
      login_ui
    }
  })


  active_data <- reactive({
    src <- input$data_source
    if (is.null(src) || length(src) == 0) return(tm_data)
    if (all(c("2025", "2026") %in% src)) {
      bind_rows(tm_data %>% mutate(Date = as.Date(Date),
                                   Time = as.character(Time),
                                   UTCDate = as.Date(UTCDate),
                                   UTCTime = as.character(UTCTime),
                                   LocalDateTime = as.character(LocalDateTime),
                                   UTCDateTime = as.character(UTCDateTime),
                                   Tilt = as.character(Tilt),
                                   HomeTeamForeignID = as.character(HomeTeamForeignID),
                                   AwayTeamForeignID = as.character(AwayTeamForeignID),
                                   PositionAt110X = as.character(PositionAt110X),
                                   PositionAt110Y = as.character(PositionAt110Y),
                                   PositionAt110Z = as.character(PositionAt110Z),
                                   across(starts_with(c("PitchTrajectory", "HitTrajectory", "HitSpinAxis", 
                                                        "CatchPosition", "ThrowPosition", "BasePosition",
                                                       "ThrowTrajectory")), as.numeric)),
                data_2026 %>% mutate(Date = as.Date(Date),
                                   Time = as.character(Time),
                                  UTCDate = as.Date(UTCDate),
                                  UTCTime = as.character(UTCTime),
                                  LocalDateTime = as.character(LocalDateTime),
                                  UTCDateTime = as.character(UTCDateTime),
                                  Tilt = as.character(Tilt),
                                  HomeTeamForeignID = as.character(HomeTeamForeignID),
                                  AwayTeamForeignID = as.character(AwayTeamForeignID),
                                  PositionAt110X = as.character(PositionAt110X),
                                  PositionAt110Y = as.character(PositionAt110Y),
                                  PositionAt110Z = as.character(PositionAt110Z),
                                  across(starts_with(c("PitchTrajectory", "HitTrajectory", "HitSpinAxis", 
                                                        "CatchPosition", "ThrowPosition", "BasePosition",
                                                       "ThrowTrajectory")), as.numeric)) 
                        %>% distinct())
    } else if ("2026" %in% src) {
      data_2026_rv()
    } else {
      tm_data
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

output$data_source_info <- renderText({
  src <- input$data_source
  if (is.null(src) || length(src) == 0) return("No data source selected")
  d <- active_data()
  label <- paste(src, collapse = " + ")
  paste0(label, ": ", nrow(d), " rows | ", length(unique(d$Batter)), " hitters | ", length(unique(d$Pitcher)), " pitchers")
})
  
# Update dropdowns when data source changes
observeEvent(input$data_source, {
  d <- active_data()
  updateSelectizeInput(session, "scout_hitters", choices = sort(unique(d$Batter)), server = TRUE)
  updateSelectizeInput(session, "mm_hitters", choices = sort(unique(d$Batter)), server = TRUE)
  updateSelectizeInput(session, "mm_pitchers", choices = sort(unique(d$Pitcher)), server = TRUE)
  updateSelectizeInput(session, "tc_hitter", choices = sort(unique(d$Batter)), server = TRUE)
  updateSelectizeInput(session, "tc_pitcher", choices = c("", sort(unique(d$Pitcher))), server = TRUE)
  scout_data(list())
  selected_hitter(NULL)
}, ignoreInit = TRUE)

  
  
scout_data <- reactiveVal(list())
  selected_hitter <- reactiveVal(NULL)
  
  # Initialize all selectize inputs AFTER login
  observeEvent(logged_in(), {
    if (logged_in()) {
      updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_pitchers", choices = all_pitchers, server = TRUE)
      updateSelectizeInput(session, "tc_hitter", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "tc_pitcher", choices = c("", all_pitchers), server = TRUE)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$scout_hitters, {
    hitters <- input$scout_hitters
    current_data <- scout_data()
    for (h in hitters) {
      if (is.null(current_data[[h]])) {
        profile <- tryCatch(
          calculate_hitter_profile(h, active_data(), gm),
          error = function(e) {
            cat("ERROR calculating profile for", h, ":", e$message, "\n")
            NULL
          }
        )
        if (!is.null(profile)) {
          current_data[[h]] <- list(
            profile = profile, lhp_plan = "", rhp_plan = "",
            overall_notes = "", pitcher_matchup_notes = ""
          )
        } else {
          cat("Skipping", h, "- insufficient data\n")
        }
      }
    }
    current_data <- current_data[names(current_data) %in% hitters]
    scout_data(current_data)
    
    # Notify user if some hitters were dropped
    n_selected <- length(hitters)
    n_loaded <- length(current_data)
    if (n_loaded < n_selected) {
      showNotification(
        paste0(n_loaded, " of ", n_selected, " hitters loaded (",
               n_selected - n_loaded, " had insufficient data)"),
        type = "warning", duration = 5
      )
    }
    
    if (is.null(selected_hitter()) && length(names(current_data)) > 0) {
      selected_hitter(names(current_data)[1])
    }
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
    h_raw <- active_data() %>% filter(Batter == h_name)
    cs <- list(fp = calc_count_stats(h_raw %>% filter(count_cat == "First Pitch")),
               ahead = calc_count_stats(h_raw %>% filter(count_cat == "Ahead")),
               behind = calc_count_stats(h_raw %>% filter(count_cat == "Behind")),
               ts = calc_count_stats(h_raw %>% filter(two_strike == TRUE)),
               early = calc_count_stats(h_raw %>% filter(Strikes < 2)))
    
    # Calculate LHP/RHP splits
    rhp_profile <- calculate_hitter_profile(h_name, active_data(), gm, "Right")
    lhp_profile <- calculate_hitter_profile(h_name, active_data(), gm, "Left")
    
    # Last 15 games stats
    last15_dates <- get_last_n_games(active_data(), h_name, 15)
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
    h_raw <- active_data() %>% filter(Batter == h_name)
    
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
    output[[paste0("ho_rhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "rhp_hits") }, bg = "transparent")
    output[[paste0("ho_rhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "rhp_outs") }, bg = "transparent")
    output[[paste0("ho_lhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "lhp_hits") }, bg = "transparent")
    output[[paste0("ho_lhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "lhp_outs") }, bg = "transparent")
    
    # Hit/Out charts by count (0-1 strikes and 2 strikes)
    output[[paste0("ho_early_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "early_hits") }, bg = "transparent")
    output[[paste0("ho_early_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "early_outs") }, bg = "transparent")
    output[[paste0("ho_2k_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "2k_hits") }, bg = "transparent")
    output[[paste0("ho_2k_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(active_data(), h_name, "2k_outs") }, bg = "transparent")
    
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
    date_range <- get_batter_date_range(h_name, active_data())
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
      
      create_trend_chart(h_name, active_data(), selected_stats, start_date, end_date, rolling_games = 5)
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
    paste0(format(nrow(active_data()), big.mark = ","), " pitches | ", 
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
    req(active_data())
    
    hitter_list <- if (length(input$mm_hitters) > 0) {
      lapply(input$mm_hitters, function(h) {
        hd <- active_data()[active_data()$Batter == h, , drop = FALSE]
        if (nrow(hd) == 0) return(NULL)
        list(name = h, splits = mm_calc_all_splits(hd, "mean_DRE_bat", is_pitcher = FALSE), data = hd, n = nrow(hd))
      })
    } else NULL
    hitter_list <- Filter(Negate(is.null), hitter_list)
    if (length(hitter_list) > 0) names(hitter_list) <- sapply(hitter_list, `[[`, "name")
    
    pitcher_list <- if (length(input$mm_pitchers) > 0) {
      lapply(input$mm_pitchers, function(p) {
        pd <- active_data()[active_data()$Pitcher == p, , drop = FALSE]
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
    
   else if (input$mm_view_mode == "hitter_profiles") {
      req(cd$hitters)
      display_splits <- mm_get_display_splits()
      req(length(display_splits) > 0)
      
      rows <- lapply(cd$hitters, function(h) {
        vals <- sapply(display_splits, function(s) {
          v <- h$splits[[s]]$rv100
          if (is.null(v) || is.na(v)) NA_real_ else round(v, 2)
        })
        data.frame(Hitter = h$name, N = nrow(h$data), t(vals), stringsAsFactors = FALSE, check.names = FALSE)
      })
      
      df <- do.call(rbind, rows)
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
      req(length(display_splits) > 0)
      
      rows <- lapply(cd$pitchers, function(p) {
        vals <- sapply(display_splits, function(s) {
          v <- p$splits[[s]]$rv100
          if (is.null(v) || is.na(v)) NA_real_ else round(v, 2)
        })
        data.frame(Pitcher = p$name, N = nrow(p$data), t(vals), stringsAsFactors = FALSE, check.names = FALSE)
      })
      
      df <- do.call(rbind, rows)
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

  tc_league_baseline <- reactive({
    calc_league_baseline(active_data())
  })

  tc_card_data <- eventReactive(input$tc_build, {
    req(input$tc_hitter, input$tc_hitter != "")

    showNotification("Building tendency card...", type = "message", duration = 2)
    baseline <- tc_league_baseline()
    card <- calc_hitter_tendency_card(input$tc_hitter, active_data(), baseline)

    if (is.null(card)) {
      showNotification("Insufficient data for this hitter", type = "warning")
      return(NULL)
    }

    overlay <- NULL
    if (!is.null(input$tc_pitcher) && input$tc_pitcher != "") {
      overlay <- overlay_pitcher_on_tendency(card, input$tc_pitcher, active_data(), baseline)
    }

    list(card = card, overlay = overlay)
  })

  # ---------- Summary box ----------

  output$tc_summary <- renderUI({
    req(tc_card_data())
    cd <- tc_card_data()
    card <- cd$card
    if (is.null(card)) return(NULL)

    n_vuln <- length(card$vulnerabilities)
    n_str <- length(card$strengths)

    div(class = "tc-summary-box",
      h4(paste0(format_short_name(card$name), " ", format_batter_hand(card$batter_side),
                " — Tendency Card")),
      p(paste0("Total pitches: ", card$n_total,
               " | Vulnerabilities: ", n_vuln,
               " | Strengths: ", n_str)),
      p(paste0("Overall: Whiff ", card$overall$whiff_rate, "% | Chase ",
               card$overall$chase_rate, "% | Damage ", card$overall$damage_rate,
               "% | wOBA ", card$overall$woba))
    )
  })

  # ---------- Helper: build HTML grid from tendency card data ----------

  tc_build_grid <- function(card_section, row_names, col_names, metric) {

    vuln_class <- function(cell) {
      if (is.null(cell) || is.na(cell$vulnerability) || cell$vulnerability == "unknown") return("tc-empty")
      switch(cell$vulnerability,
        "high_vuln" = "tc-high-vuln",
        "vuln_whiff" = "tc-vuln",
        "vuln_chase" = "tc-vuln",
        "strength" = "tc-strength",
        "tc-neutral"
      )
    }

    format_cell <- function(cell, metric) {
      if (is.null(cell) || cell$n < 10) {
        return(tags$td(class = "tc-empty", paste0("(", if(is.null(cell)) 0 else cell$n, ")")))
      }
      val <- cell[[metric]]
      if (is.na(val)) return(tags$td(class = "tc-empty", paste0("n=", cell$n)))

      display <- if (metric == "woba") sprintf("%.3f", val) else sprintf("%.1f%%", val)

      tags$td(class = vuln_class(cell),
        div(style = "font-weight: 600;", display),
        div(style = "font-size: 9px; color: #888;", paste0("n=", cell$n))
      )
    }

    header <- tags$tr(
      tags$th(""),
      lapply(col_names, function(cn) tags$th(cn))
    )

    rows <- lapply(row_names, function(rn) {
      tags$tr(
        tags$td(class = "row-label", rn),
        lapply(col_names, function(cn) {
          col_key <- switch(cn,
            "First Pitch" = "first_pitch", "Early" = "early",
            "2 Strike" = "two_strike", "Ahead" = "ahead", "Behind" = "behind",
            "Overall" = "overall",
            cn
          )
          cell <- card_section[[rn]][[col_key]]
          format_cell(cell, metric)
        })
      )
    })

    tags$table(class = "tc-grid-table",
      tags$thead(header),
      tags$tbody(rows)
    )
  }

  # ---------- Shape x Count grid ----------

  output$tc_shape_count_grid <- renderUI({
    req(tc_card_data())
    card <- tc_card_data()$card
    if (is.null(card)) return(NULL)

    metric <- input$tc_metric
    row_names <- c("Ride FB", "Sink/Run", "Cutter", "Sweep", "Slider", "Curveball",
                   "CH/SPL", "All FB", "All BB", "All OS")
    row_names <- row_names[row_names %in% names(card$shape_count)]
    col_names <- c("Overall", "First Pitch", "Early", "2 Strike", "Ahead", "Behind")

    tc_build_grid(card$shape_count, row_names, col_names, metric)
  })

  # ---------- Shape x Zone grid ----------

  output$tc_shape_zone_grid <- renderUI({
    req(tc_card_data())
    card <- tc_card_data()$card
    if (is.null(card)) return(NULL)

    metric <- input$tc_metric
    row_names <- c("Ride FB", "Sink/Run", "Cutter", "Sweep", "Slider", "Curveball",
                   "CH/SPL", "All FB", "All BB", "All OS")
    row_names <- row_names[row_names %in% names(card$shape_zone)]
    col_names <- c("Elevated", "UpIn", "UpAway", "Heart", "DownIn", "DownAway", "GloveDown", "ArmDown")

    tc_build_grid(card$shape_zone, row_names, col_names, metric)
  })

  # ---------- Zone x Count grid ----------

  output$tc_zone_count_grid <- renderUI({
    req(tc_card_data())
    card <- tc_card_data()$card
    if (is.null(card)) return(NULL)

    metric <- input$tc_metric
    row_names <- c("Elevated", "UpIn", "UpAway", "Heart", "DownIn", "DownAway", "GloveDown", "ArmDown")
    row_names <- row_names[row_names %in% names(card$zone_count)]
    col_names <- c("Overall", "First Pitch", "Early", "2 Strike", "Ahead", "Behind")

    tc_build_grid(card$zone_count, row_names, col_names, metric)
  })

  # ---------- Vulnerability / Strength summary ----------

  output$tc_vuln_summary <- renderUI({
    req(tc_card_data())
    card <- tc_card_data()$card
    if (is.null(card)) return(NULL)

    vuln_ui <- if (length(card$vulnerabilities) > 0) {
      lapply(names(card$vulnerabilities), function(label) {
        cell <- card$vulnerabilities[[label]]
        details <- paste0(
          "Whiff: ", if(!is.na(cell$whiff_rate)) paste0(cell$whiff_rate, "%") else "-",
          " | Chase: ", if(!is.na(cell$chase_rate)) paste0(cell$chase_rate, "%") else "-",
          " | n=", cell$n
        )
        div(class = "tc-rec-card rec-bad",
          div(class = "tc-rec-pitch", paste0("\u26A0 ", label)),
          div(class = "tc-rec-detail", details)
        )
      })
    } else {
      list(p(style = "color: #999; font-size: 12px;", "No significant vulnerabilities detected"))
    }

    str_ui <- if (length(card$strengths) > 0) {
      lapply(names(card$strengths), function(label) {
        cell <- card$strengths[[label]]
        details <- paste0(
          "Damage: ", if(!is.na(cell$damage_rate)) paste0(cell$damage_rate, "%") else "-",
          " | wOBA: ", if(!is.na(cell$woba)) cell$woba else "-",
          " | n=", cell$n
        )
        div(class = "tc-rec-card rec-good",
          div(class = "tc-rec-pitch", paste0("\u2705 ", label)),
          div(class = "tc-rec-detail", details)
        )
      })
    } else {
      list(p(style = "color: #999; font-size: 12px;", "No significant strengths detected"))
    }

    tagList(
      div(style = "margin-bottom: 12px;",
        tags$label(style = "font-weight: 700; color: #C62828; font-size: 13px;", "Vulnerabilities (exploit these):"),
        vuln_ui
      ),
      div(
        tags$label(style = "font-weight: 700; color: #2E7D32; font-size: 13px;", "Strengths (avoid these):"),
        str_ui
      )
    )
  })

  # ---------- Pitcher overlay title ----------

  output$tc_overlay_title <- renderText({
    req(tc_card_data())
    overlay <- tc_card_data()$overlay
    if (is.null(overlay)) return("No pitcher overlay")
    paste0("Pitcher Overlay: ", overlay$pitcher_name, " (", overlay$pitcher_hand, "HP)",
           " vs ", overlay$hitter_name)
  })

  # ---------- Pitcher overlay content ----------

  output$tc_pitcher_overlay <- renderUI({
    req(tc_card_data())
    overlay <- tc_card_data()$overlay
    if (is.null(overlay)) return(div(style = "color: #999;", "Select a pitcher and rebuild"))

    arsenal_ui <- tags$table(class = "tc-grid-table",
      tags$thead(tags$tr(
        tags$th("Pitch"), tags$th("Usage%"), tags$th("Velo"), tags$th("IVB"), tags$th("HB"),
        tags$th("P Whiff%"), tags$th("Zone%")
      )),
      tags$tbody(
        lapply(seq_len(nrow(overlay$arsenal)), function(i) {
          row <- overlay$arsenal[i, ]
          tags$tr(
            tags$td(class = "row-label", row$movement_tag),
            tags$td(sprintf("%.1f%%", row$pct)),
            tags$td(sprintf("%.1f", row$avg_velo)),
            tags$td(sprintf("%.1f", row$avg_ivb)),
            tags$td(sprintf("%.1f", row$avg_hb)),
            tags$td(sprintf("%.1f%%", row$whiff_rate * 100)),
            tags$td(sprintf("%.1f%%", row$zone_pct * 100))
          )
        })
      )
    )

    rec_ui <- lapply(c("first_pitch", "early", "two_strike"), function(ct) {
      recs <- overlay$recommendations[[ct]]
      if (is.null(recs) || length(recs) == 0) return(NULL)

      ct_label <- c("first_pitch" = "First Pitch", "early" = "Early Count", "two_strike" = "2 Strikes")[[ct]]

      vuln_order <- c("high_vuln" = 1, "vuln_whiff" = 2, "vuln_chase" = 3, "neutral" = 4, "strength" = 5)
      recs_sorted <- recs[order(sapply(recs, function(r) {
        if (is.null(r$h_vuln) || is.na(r$h_vuln)) 4 else vuln_order[r$h_vuln]
      }))]

      rec_cards <- lapply(recs_sorted, function(r) {
        card_class <- switch(r$h_vuln,
          "high_vuln" = "rec-good", "vuln_whiff" = "rec-good", "vuln_chase" = "rec-good",
          "strength" = "rec-bad", "rec-neutral"
        )

        action_label <- switch(r$h_vuln,
          "high_vuln" = "\u2705 ATTACK",
          "vuln_whiff" = "\u2705 Go-to pitch",
          "vuln_chase" = "\u2705 Chase bait",
          "strength" = "\u274C AVOID",
          "\u2796 Neutral"
        )

        detail_parts <- c(
          paste0("Usage: ", sprintf("%.0f%%", r$usage_pct)),
          if (!is.na(r$h_whiff)) paste0("H-Whiff: ", r$h_whiff, "%"),
          if (!is.na(r$h_chase)) paste0("H-Chase: ", r$h_chase, "%"),
          if (!is.na(r$h_damage)) paste0("H-Dmg: ", r$h_damage, "%"),
          if (!is.na(r$matchup_whiff)) paste0("Log5 Whiff: ", r$matchup_whiff, "%")
        )

        zone_note <- if (!is.null(r$best_zone)) {
          paste0(" \u2192 Target: ", r$best_zone$zone,
                 " (pitcher hits it ", sprintf("%.0f%%", r$best_zone$pct), " of the time, ",
                 "hitter ", r$best_zone$h_vuln, " there)")
        } else ""

        div(class = paste("tc-rec-card", card_class),
          div(class = "tc-rec-pitch", paste0(action_label, " — ", r$shape_group, " (", r$pitch, ")")),
          div(class = "tc-rec-detail", paste(detail_parts, collapse = " | ")),
          if (nchar(zone_note) > 0) div(class = "tc-rec-detail", style = "font-style: italic;", zone_note)
        )
      })

      div(style = "margin-bottom: 15px;",
        div(class = "tc-overlay-header", ct_label),
        rec_cards
      )
    })

    tagList(
      div(style = "margin-bottom: 15px;", arsenal_ui),
      hr(),
      tags$label(style = "font-weight: 700; color: #006F71; font-size: 14px;", "Sequence Recommendations"),
      rec_ui
    )
  })

  # ---------- PDF Download ----------

  output$tc_download_pdf <- downloadHandler(
    filename = function() {
      h <- if (!is.null(input$tc_hitter)) gsub(" ", "_", input$tc_hitter) else "hitter"
      paste0("tendency_card_", h, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      req(tc_card_data())
      cd <- tc_card_data()
      card <- cd$card
      if (is.null(card)) {
        showNotification("No card data", type = "warning")
        return()
      }

      metric <- input$tc_metric

      shape_names <- c("Ride FB", "Sink/Run", "Cutter", "Sweep", "Slider", "Curveball", "CH/SPL")
      shape_names <- shape_names[shape_names %in% names(card$shape_count)]
      count_cols <- c("overall", "first_pitch", "early", "two_strike", "ahead", "behind")
      count_labels <- c("Overall", "1st Pitch", "Early", "2K", "Ahead", "Behind")

      rows <- lapply(shape_names, function(sn) {
        vals <- sapply(count_cols, function(cc) {
          cell <- card$shape_count[[sn]][[cc]]
          if (is.null(cell) || cell$n < 10 || is.na(cell[[metric]])) NA_real_
          else cell[[metric]]
        })
        c(Shape = sn, setNames(vals, count_labels))
      })

      df <- do.call(rbind, rows) %>% as.data.frame(stringsAsFactors = FALSE)
      for (col in count_labels) df[[col]] <- as.numeric(df[[col]])

      gt_tbl <- df %>%
        gt() %>%
        tab_header(
          title = paste0("Tendency Card: ", card$name, " ", format_batter_hand(card$batter_side)),
          subtitle = paste0("Metric: ", metric, " | n=", card$n_total, " pitches | Log5 adjusted")
        ) %>%
        data_color(columns = all_of(count_labels), fn = scales::col_numeric(
          palette = c("#1e8449", "#f5f5f5", "#c0392b"),
          domain = if (metric == "woba") c(0.150, 0.320, 0.500) else c(10, 25, 50),
          na.color = "#eee"
        )) %>%
        cols_align(align = "center") %>%
        sub_missing(missing_text = "-") %>%
        tab_options(table.font.size = px(12), column_labels.font.weight = "bold",
                   column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Shape"))

      gtsave(gt_tbl, file)
    }
  )
  
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
      
      # Check if count columns should be added (for pitch group/platoon views)
      add_counts <- isTRUE(input$mm_show_counts)
      
      rows <- lapply(cd$hitters, function(h) {
        vals <- sapply(display_splits, function(s) round(h$splits[[s]]$rv100, 2))
        base_row <- c(Hitter = h$name, N = nrow(h$data), vals)
        
        if (add_counts) {
          # Calculate count-based RV for this hitter
          count_splits <- mm_calc_count_splits(h$data, "mean_DRE_bat", pitcher_hand = NULL, is_pitcher = FALSE)
          count_vals <- sapply(c("0-0", "hitters", "pitchers", "2k"), function(ct) {
            if (!is.null(count_splits[[ct]])) round(count_splits[[ct]]$rv100, 2) else NA_real_
          })
          names(count_vals) <- c("0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")
          base_row <- c(base_row, count_vals)
        }
        base_row
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      split_names <- sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name)
      if (add_counts) {
        names(df) <- c("Hitter", "N", split_names, "0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")
      } else {
        names(df) <- c("Hitter", "N", split_names)
      }
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      tbl <- df %>%
        gt() %>%
        tab_header(title = "Hitter Profiles", subtitle = "RV/100 (positive = good for hitter) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "13px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
      
      # Add spanner for count columns if present
      if (add_counts) {
        tbl <- tbl %>%
          tab_spanner(label = "By Count (RV/100)", columns = c("0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes"))
      }
      
      tbl
    }
    
    else if (view_mode == "pitcher_profiles") {
      if (is.null(cd$pitchers) || length(display_splits) == 0) return(NULL)
      
      add_counts <- isTRUE(input$mm_show_counts)
      
      rows <- lapply(cd$pitchers, function(p) {
        vals <- sapply(display_splits, function(s) round(p$splits[[s]]$rv100, 2))
        base_row <- c(Pitcher = p$name, N = nrow(p$data), vals)
        
        if (add_counts) {
          count_splits <- mm_calc_count_splits(p$data, "mean_DRE_pit", pitcher_hand = NULL, is_pitcher = TRUE)
          count_vals <- sapply(c("0-0", "hitters", "pitchers", "2k"), function(ct) {
            if (!is.null(count_splits[[ct]])) round(count_splits[[ct]]$rv100, 2) else NA_real_
          })
          names(count_vals) <- c("0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")
          base_row <- c(base_row, count_vals)
        }
        base_row
      })
      
      df <- do.call(rbind, rows) %>% as.data.frame()
      split_names <- sapply(display_splits, function(s) MM_ALL_SPLITS[[s]]$name)
      if (add_counts) {
        names(df) <- c("Pitcher", "N", split_names, "0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")
      } else {
        names(df) <- c("Pitcher", "N", split_names)
      }
      df$N <- as.integer(df$N)
      for (col in names(df)[3:ncol(df)]) df[[col]] <- as.numeric(df[[col]])
      
      tbl <- df %>%
        gt() %>%
        tab_header(title = "Pitcher Profiles", subtitle = "RV/100 (negative = good for pitcher) | Hierarchically regressed") %>%
        data_color(columns = 3:ncol(df), fn = scales::col_numeric(
          palette = c("#1e8449", "#27ae60", "#f5f5f5", "#e74c3c", "#c0392b"),
          domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        tab_options(table.font.size = "13px", column_labels.font.weight = "bold", column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Pitcher"))
      
      if (add_counts) {
        tbl <- tbl %>%
          tab_spanner(label = "By Count (RV/100)", columns = c("0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes"))
      }
      
      tbl
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

  observeEvent(input$refresh_data, {
    tryCatch({
      showNotification("Refreshing data from HuggingFace...", type = "message", duration = 3)
      new_data <- download_private_parquet("CoastalBaseball/2026MasterDataset", "pbp_2026_master.parquet")
      new_data <- process_pitcher_data(new_data, bio_heights, "data_ind")$data
      data_2026_rv(new_data)
      
      new_hitters <- sort(unique(new_data$Batter[!is.na(new_data$Batter) & new_data$Batter != ""]))
      new_pitchers <- sort(unique(new_data$Pitcher[!is.na(new_data$Pitcher) & new_data$Pitcher != ""]))
      all_hitters <<- sort(unique(c(all_hitters_2025, new_hitters)))
      all_pitchers <<- sort(unique(c(all_pitchers_2025, new_pitchers)))
      
      updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_hitters", choices = all_hitters, server = TRUE)
      updateSelectizeInput(session, "mm_pitchers", choices = all_pitchers, server = TRUE)
      
      showNotification(paste0("Data refreshed! ", nrow(new_data), " rows loaded."), type = "message")
    }, error = function(e) {
      showNotification(paste("Refresh failed:", e$message), type = "error")
    })
  })                       

                          

# Download PNG of current view - uses gt::gtsave as self-contained HTML
  output$mm_download_png <- downloadHandler(
    filename = function() {
      paste0("matchup_", input$mm_view_mode, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
    },
    content = function(file) {
      gt_tbl <- mm_generate_gt_table(input$mm_view_mode, input$mm_selected_split)
      if (!is.null(gt_tbl)) {
        gtsave(gt_tbl, file)
      } else {
        writeLines("<html><body><h2>No data to display</h2></body></html>", file)
      }
    }
  )
  

output$mm_download_pdf <- downloadHandler(
    filename = function() {
      paste0("matchup_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      cd <- mm_calc_data()
      if (is.null(cd)) { showNotification("No data. Click Analyze first.", type = "warning"); return() }
      
      report_title <- if (nchar(input$mm_pdf_title) > 0) input$mm_pdf_title else "Matchup Matrix Report"
      best_matchups <- input$mm_pdf_best_matchups
      worst_matchups <- input$mm_pdf_worst_matchups
      notes_text <- input$mm_pdf_notes
      
      # Helper: render gt table to PNG via gtsave (native sizing, no manual webshot)
      gt_save_png <- function(gt_tbl) {
        if (is.null(gt_tbl)) return(NULL)
        tmp_png <- tempfile(fileext = ".png")
        tryCatch({
          gtsave(gt_tbl, tmp_png, zoom = 3, expand = 5)
          if (file.exists(tmp_png)) tmp_png else NULL
        }, error = function(e) { message("gtsave error: ", e$message); NULL })
      }
      
      # ---- Build GT tables ----
      
      # Table 1: Overall Matchup Matrix
      gt_overall <- NULL
      if (!is.null(cd$hitters) && !is.null(cd$pitchers) && length(cd$hitters) > 0 && length(cd$pitchers) > 0) {
        mat <- matrix(NA_real_, nrow = length(cd$hitters), ncol = length(cd$pitchers))
        rownames(mat) <- sapply(cd$hitters, function(h) format_short_name(h$name))
        colnames(mat) <- sapply(cd$pitchers, function(p) format_short_name(p$name))
        for (i in seq_along(cd$hitters)) {
          for (j in seq_along(cd$pitchers)) {
            overall <- mm_calc_overall_matchup(cd$hitters[[i]]$splits, cd$pitchers[[j]]$splits, cd$pitchers[[j]]$data)
            mat[i, j] <- round(overall$rv, 2)
          }
        }
        gt_overall <- as.data.frame(mat) %>%
          tibble::rownames_to_column("Hitter") %>%
          gt() %>%
          tab_header(title = "Overall Matchup Matrix (Weighted RV/100)",
                    subtitle = "Positive = hitter advantage") %>%
          data_color(columns = -Hitter, fn = scales::col_numeric(
            palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
            domain = c(-10, -3, 0, 3, 10), na.color = "#eee")) %>%
          cols_align(align = "center") %>%
          tab_options(table.font.size = px(12), column_labels.font.weight = "bold",
                     column_labels.background.color = "#e8f5f4",
                     data_row.padding = px(4)) %>%
          tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
      }
      
      # Table 2: Hitter Platoon + Pitch Group + Counts
      gt_pitch_group <- NULL
      if (!is.null(cd$hitters) && length(cd$hitters) > 0) {
        pg_splits <- c("vs_rhp", "vs_lhp", "fb_rhp", "bb_rhp", "os_rhp", "fb_lhp", "bb_lhp", "os_lhp")
        valid_splits <- pg_splits[pg_splits %in% names(MM_ALL_SPLITS)]
        
        rows <- lapply(cd$hitters, function(h) {
          vals <- sapply(valid_splits, function(s) {
            v <- h$splits[[s]]$rv100; if (is.null(v) || is.na(v)) NA_real_ else round(v, 2)
          })
          count_splits <- mm_calc_count_splits(h$data, "mean_DRE_bat", pitcher_hand = NULL, is_pitcher = FALSE)
          count_vals <- sapply(c("0-0", "hitters", "pitchers", "2k"), function(ct) {
            if (!is.null(count_splits[[ct]]) && !is.na(count_splits[[ct]]$rv100)) round(count_splits[[ct]]$rv100, 2) else NA_real_
          })
          c(Hitter = format_short_name(h$name), N = nrow(h$data), vals, count_vals)
        })
        
        df_pg <- do.call(rbind, rows) %>% as.data.frame(stringsAsFactors = FALSE)
        split_names <- sapply(valid_splits, function(s) MM_ALL_SPLITS[[s]]$name)
        names(df_pg) <- c("Hitter", "N", split_names, "0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")
        df_pg$N <- as.integer(df_pg$N)
        for (col in names(df_pg)[3:ncol(df_pg)]) df_pg[[col]] <- as.numeric(df_pg[[col]])
        
        gt_pitch_group <- df_pg %>%
          gt() %>%
          tab_header(title = "Hitter Profiles: Platoon / Pitch Group / Counts",
                    subtitle = "RV/100 — Positive = good for hitter") %>%
          tab_spanner(label = "Platoon", columns = any_of(c("vs RHP", "vs LHP"))) %>%
          tab_spanner(label = "vs RHP Pitch Group", columns = any_of(c("All FB (RHP)", "All BB (RHP)", "All OS (RHP)"))) %>%
          tab_spanner(label = "vs LHP Pitch Group", columns = any_of(c("All FB (LHP)", "All BB (LHP)", "All OS (LHP)"))) %>%
          tab_spanner(label = "By Count", columns = c("0-0", "Hitter Ct", "Pitcher Ct", "2 Strikes")) %>%
          data_color(columns = 3:ncol(df_pg), fn = scales::col_numeric(
            palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
            domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
          cols_align(align = "center") %>%
          sub_missing(missing_text = "-") %>%
          tab_options(table.font.size = px(11), column_labels.font.weight = "bold",
                     column_labels.font.size = px(10),
                     column_labels.background.color = "#e8f5f4",
                     data_row.padding = px(3)) %>%
          tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
      }
      
      # Table 3: Hitter Movement Splits
      gt_movement <- NULL
      if (!is.null(cd$hitters) && length(cd$hitters) > 0) {
        mvmt_splits <- c("ride_fb_rhp", "sink_fb_rhp", "hard_velo_rhp", "sweep_rhp", "downer_rhp", "gyro_cut_rhp", "chspl_rhp",
                         "ride_fb_lhp", "sink_fb_lhp", "hard_velo_lhp", "sweep_lhp", "downer_lhp", "gyro_cut_lhp", "chspl_lhp")
        valid_mvmt <- mvmt_splits[mvmt_splits %in% names(MM_ALL_SPLITS)]
        
        rows <- lapply(cd$hitters, function(h) {
          vals <- sapply(valid_mvmt, function(s) {
            v <- h$splits[[s]]$rv100; if (is.null(v) || is.na(v)) NA_real_ else round(v, 2)
          })
          c(Hitter = format_short_name(h$name), N = nrow(h$data), vals)
        })
        
        df_mvmt <- do.call(rbind, rows) %>% as.data.frame(stringsAsFactors = FALSE)
        split_names <- sapply(valid_mvmt, function(s) MM_ALL_SPLITS[[s]]$name)
        names(df_mvmt) <- c("Hitter", "N", split_names)
        df_mvmt$N <- as.integer(df_mvmt$N)
        for (col in names(df_mvmt)[3:ncol(df_mvmt)]) df_mvmt[[col]] <- as.numeric(df_mvmt[[col]])
        
        gt_movement <- df_mvmt %>%
          gt() %>%
          tab_header(title = "Hitter Profiles: Movement Splits",
                    subtitle = "RV/100 — Hierarchically regressed") %>%
          data_color(columns = 3:ncol(df_mvmt), fn = scales::col_numeric(
            palette = c("#c0392b", "#e74c3c", "#f5f5f5", "#27ae60", "#1e8449"),
            domain = c(-15, -5, 0, 5, 15), na.color = "#eee")) %>%
          cols_align(align = "center") %>%
          sub_missing(missing_text = "-") %>%
          tab_options(table.font.size = px(10), column_labels.font.weight = "bold",
                     column_labels.font.size = px(9),
                     column_labels.background.color = "#e8f5f4",
                     data_row.padding = px(3)) %>%
          tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
      }
      
      # ---- Render gt tables to PNGs ----
      png_overall <- gt_save_png(gt_overall)
      png_pitch_group <- gt_save_png(gt_pitch_group)
      png_movement <- gt_save_png(gt_movement)
      
      pdf(file, width = 8.5, height = 12, onefile = TRUE)
      
      page_w_in <- 8.5
      page_h_in <- 11
      margin_in <- 0.3
      usable_w_in <- page_w_in - 2 * margin_in  # 7.9 inches usable
      
      # Header banner
      grid::grid.rect(x = 0.5, y = 0.978, width = 1, height = 0.035,
                     gp = grid::gpar(fill = "#006F71", col = NA))
      grid::grid.text(report_title, x = 0.5, y = 0.978,
                     gp = grid::gpar(fontsize = 13, fontface = "bold", col = "white"))
      grid::grid.text(format(Sys.Date(), "%m/%d/%Y"), x = 0.95, y = 0.978, just = "right",
                     gp = grid::gpar(fontsize = 7, col = "white"))
      
      # ---- 4-Column Notes Table ----
      # Columns: Good Matchups | Bad Matchups | Available RHP | Available LHP
      # Uses input fields; falls back gracefully if new inputs don't exist yet
      cur_y_in <- page_h_in * 0.955
      
      best_text <- best_matchups
      worst_text <- worst_matchups
      avail_rhp <- tryCatch(input$mm_pdf_avail_rhp, error = function(e) "")
      avail_lhp <- tryCatch(input$mm_pdf_avail_lhp, error = function(e) "")
      if (is.null(avail_rhp)) avail_rhp <- ""
      if (is.null(avail_lhp)) avail_lhp <- ""
      # If no separate avail fields, use notes_text as fallback content
      notes_fallback <- if (nchar(avail_rhp) == 0 && nchar(avail_lhp) == 0 && nchar(notes_text) > 0) notes_text else ""
      
      has_any_notes <- nchar(best_text) > 0 || nchar(worst_text) > 0 || 
                       nchar(avail_rhp) > 0 || nchar(avail_lhp) > 0 || nchar(notes_fallback) > 0
      
      if (has_any_notes) {
        # Table dimensions in inches
        table_left <- 0.3
        table_w <- usable_w_in
        col_w <- table_w / 4
        header_h <- 0.22
        body_h <- 0.90  # generous space for content
        card_total_h <- header_h + body_h + 0.05  # padding
        
        # Convert to NPC for drawing
        npc_left <- table_left / page_w_in
        npc_w <- table_w / page_w_in
        npc_col_w <- col_w / page_w_in
        npc_header_h <- header_h / page_h_in
        npc_body_h <- body_h / page_h_in
        npc_total_h <- card_total_h / page_h_in
        
        card_top_npc <- cur_y_in / page_h_in
        
        # Background card
        grid::grid.rect(x = 0.5, y = card_top_npc - npc_total_h / 2, 
                       width = npc_w + 0.02, height = npc_total_h,
                       default.units = "npc",
                       gp = grid::gpar(fill = "#f8fafa", col = "#d0d0d0", lwd = 0.5))
        
        # Column headers
        col_headers <- c("Good Matchups for Coastal", "Bad Matchups for Coastal", 
                         "Available RHP", "Available LHP")
        col_colors <- c("#27ae60", "#c0392b", "#2962FF", "#c0392b")
        col_contents <- c(best_text, worst_text, 
                         if (nchar(avail_rhp) > 0) avail_rhp else notes_fallback, 
                         avail_lhp)
        
        header_y <- card_top_npc - 0.004
        
        for (i in 1:4) {
          col_x <- npc_left + (i - 1) * npc_col_w
          col_center_x <- col_x + npc_col_w / 2
          
          # Vertical accent bar on left of each column
          grid::grid.segments(x0 = col_x + 0.005, y0 = header_y + 0.002,
                             x1 = col_x + 0.005, y1 = header_y - npc_header_h - npc_body_h,
                             default.units = "npc", 
                             gp = grid::gpar(col = col_colors[i], lwd = 2.5))
          
          # Header text
          grid::grid.text(col_headers[i], x = col_x + 0.015, y = header_y,
                         just = "left", default.units = "npc",
                         gp = grid::gpar(fontsize = 6.5, fontface = "bold", col = col_colors[i]))
          
          # Content text — wrap manually into lines
          if (nchar(col_contents[i]) > 0) {
            content <- col_contents[i]
            # Split on newlines first, then wrap long lines
            raw_lines <- unlist(strsplit(content, "\n"))
            wrapped <- character()
            chars_per_line <- 35  # approximate chars that fit in column
            for (rl in raw_lines) {
              if (nchar(rl) <= chars_per_line) {
                wrapped <- c(wrapped, rl)
              } else {
                words <- unlist(strsplit(rl, " "))
                current_line <- ""
                for (w in words) {
                  test <- if (nchar(current_line) == 0) w else paste(current_line, w)
                  if (nchar(test) > chars_per_line && nchar(current_line) > 0) {
                    wrapped <- c(wrapped, current_line)
                    current_line <- w
                  } else {
                    current_line <- test
                  }
                }
                if (nchar(current_line) > 0) wrapped <- c(wrapped, current_line)
              }
            }
            
            # Draw each line
            line_h <- 0.011
            for (ln_idx in seq_along(wrapped)) {
              ln_y <- header_y - 0.015 - (ln_idx - 1) * line_h
              if (ln_y < card_top_npc - npc_total_h + 0.005) break  # don't overflow
              grid::grid.text(wrapped[ln_idx], x = col_x + 0.015, y = ln_y,
                             just = "left", default.units = "npc",
                             gp = grid::gpar(fontsize = 5.5, col = "#333"))
            }
          }
          
          # Vertical divider between columns (except last)
          if (i < 4) {
            div_x <- col_x + npc_col_w
            grid::grid.segments(x0 = div_x, y0 = card_top_npc - 0.002,
                               x1 = div_x, y1 = card_top_npc - npc_total_h + 0.005,
                               default.units = "npc",
                               gp = grid::gpar(col = "#d0d0d0", lwd = 0.4))
          }
        }
        
        cur_y_in <- (card_top_npc - npc_total_h) * page_h_in - 0.10
      }
      
      # ---- Place PNG tables scaled to fill page width, preserving aspect ratio ----
      # Tables render at native size via gtsave. We scale them to fill
      # the usable page width so they look crisp but not tiny.
      place_gt_png <- function(png_path, cur_y_in, max_w_in = usable_w_in, max_h_in = 4.0) {
        img <- png::readPNG(png_path)
        img_w_px <- ncol(img)
        img_h_px <- nrow(img)
        ar <- img_h_px / img_w_px  # aspect ratio
        
        # Scale to fill max width, height follows from aspect ratio
        img_w_in <- max_w_in
        img_h_in <- img_w_in * ar
        
        # Cap max height
        if (img_h_in > max_h_in) {
          img_h_in <- max_h_in
          img_w_in <- img_h_in / ar
        }
        
        center_y_in <- cur_y_in - img_h_in / 2
        
        grid::grid.raster(img,
          x = unit(page_w_in / 2, "inches"),
          y = unit(center_y_in, "inches"),
          width = unit(img_w_in, "inches"),
          height = unit(img_h_in, "inches"),
          interpolate = TRUE
        )
        
        cur_y_in - img_h_in - 0.15
      }
      
      # Table 1: Overall matchup matrix — limit width so it doesn't blow up
      if (!is.null(png_overall) && file.exists(png_overall)) {
        # Scale width based on number of pitcher columns to avoid oversized table
        n_pitchers <- if (!is.null(cd$pitchers)) length(cd$pitchers) else 3
        n_hitters <- if (!is.null(cd$hitters)) length(cd$hitters) else 6
        # Rough: ~0.8in per column + 1.2in for hitter name column
        ideal_w <- min(usable_w_in, 1.2 + n_pitchers * 0.85)
        ideal_w <- max(ideal_w, 3.0)  # minimum 3 inches
        # Also cap based on row count — if few rows, don't let it get too tall
        max_h <- min(4.0, 0.4 + n_hitters * 0.35)
        cur_y_in <- place_gt_png(png_overall, cur_y_in, max_w_in = ideal_w, max_h_in = max_h)
      }
      
      # Table 2: Pitch Group / Platoon / Counts
      if (!is.null(png_pitch_group) && file.exists(png_pitch_group)) {
        cur_y_in <- place_gt_png(png_pitch_group, cur_y_in)
      }
      
      # Table 3: Movement Splits
      if (!is.null(png_movement) && file.exists(png_movement)) {
        cur_y_in <- place_gt_png(png_movement, cur_y_in)
      }
      
      # Footer
      grid::grid.text("Data: TrackMan | Coastal Carolina Baseball Analytics", x = 0.5, y = 0.015,
                     gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
      
      dev.off()
      
      # Clean up temp PNGs
      for (f in c(png_overall, png_pitch_group, png_movement)) {
        if (!is.null(f) && file.exists(f)) unlink(f, force = TRUE)
      }
    }
  )

        # ============================================================================
  # PITCHER-HITTER MATCHUP TAB - SERVER LOGIC
  # ============================================================================
  
  output$phm_status <- renderText({
    mode <- input$phm_mode
    if (mode == "one_hitter") {
      h <- input$phm_hitter
      ps <- input$phm_pitchers_multi
      paste0("Mode: One Hitter vs Pitchers | Hitter: ", ifelse(is.null(h) || h == "", "None", h),
             " | ", length(ps), " pitcher(s) selected")
    } else {
      p <- input$phm_pitcher
      hs <- input$phm_hitters_multi
      paste0("Mode: One Pitcher vs Hitters | Pitcher: ", ifelse(is.null(p) || p == "", "None", p),
             " | ", length(hs), " hitter(s) selected")
    }
  })
  
  output$phm_results_title <- renderText({
    mode <- input$phm_mode
    if (mode == "one_hitter") {
      paste("Pitcher-Hitter Breakdown:", input$phm_hitter)
    } else {
      paste("Pitcher-Hitter Breakdown:", input$phm_pitcher)
    }
  })
  
  phm_calc_data <- eventReactive(input$phm_analyze, {
    d <- active_data()
    mode <- input$phm_mode
    
    if (mode == "one_hitter") {
      h_name <- input$phm_hitter
      p_names <- input$phm_pitchers_multi
      if (is.null(h_name) || h_name == "" || length(p_names) == 0) return(NULL)
      
      h_data <- d[d$Batter == h_name, , drop = FALSE]
      if (nrow(h_data) < 20) return(NULL)
      
      # Detect batter side
      batter_side <- detect_batter_handedness(h_name, d)
      h_splits <- mm_calc_all_splits(h_data, "mean_DRE_bat", is_pitcher = FALSE, batter_side = batter_side)
      
      results <- lapply(p_names, function(p_name) {
        p_data <- d[d$Pitcher == p_name, , drop = FALSE]
        if (nrow(p_data) < 20) return(NULL)
        p_hand <- names(sort(table(p_data$PitcherThrows[!is.na(p_data$PitcherThrows)]), decreasing = TRUE))[1]
        p_splits <- mm_calc_all_splits(p_data, "mean_DRE_pit", is_pitcher = TRUE)
        detail <- calc_detailed_matchup(h_data, p_data, h_splits, p_splits, p_hand)
        if (is.null(detail)) return(NULL)
        detail$pitcher_name <- p_name
        detail$hitter_name <- h_name
        detail
      })
      results <- Filter(Negate(is.null), results)
      list(mode = "one_hitter", hitter = h_name, batter_side = batter_side, results = results)
      
    } else {
      p_name <- input$phm_pitcher
      h_names <- input$phm_hitters_multi
      if (is.null(p_name) || p_name == "" || length(h_names) == 0) return(NULL)
      
      p_data <- d[d$Pitcher == p_name, , drop = FALSE]
      if (nrow(p_data) < 20) return(NULL)
      p_hand <- names(sort(table(p_data$PitcherThrows[!is.na(p_data$PitcherThrows)]), decreasing = TRUE))[1]
      p_splits <- mm_calc_all_splits(p_data, "mean_DRE_pit", is_pitcher = TRUE)
      
      results <- lapply(h_names, function(h_name) {
        h_data <- d[d$Batter == h_name, , drop = FALSE]
        if (nrow(h_data) < 20) return(NULL)
        batter_side <- detect_batter_handedness(h_name, d)
        h_splits <- mm_calc_all_splits(h_data, "mean_DRE_bat", is_pitcher = FALSE, batter_side = batter_side)
        detail <- calc_detailed_matchup(h_data, p_data, h_splits, p_splits, p_hand)
        if (is.null(detail)) return(NULL)
        detail$pitcher_name <- p_name
        detail$hitter_name <- h_name
        detail$batter_side <- batter_side
        detail
      })
      results <- Filter(Negate(is.null), results)
      list(mode = "one_pitcher", pitcher = p_name, pitcher_hand = p_hand, results = results)
    }
  })
  
  # Arsenal table - shows pitch arsenal with movement profile
  output$phm_arsenal_table <- render_gt({
    req(phm_calc_data())
    cd <- phm_calc_data()
    if (is.null(cd) || length(cd$results) == 0) return(NULL)
    
    if (cd$mode == "one_hitter") {
      # Show each pitcher's arsenal side by side
      all_rows <- lapply(cd$results, function(res) {
        res$arsenal %>% 
          select(TaggedPitchType, movement_tag, n, pct, avg_velo, avg_ivb, avg_hb) %>%
          mutate(Pitcher = res$pitcher_name, .before = 1)
      })
      df <- bind_rows(all_rows)
    } else {
      # One pitcher - show their arsenal once
      res <- cd$results[[1]]
      df <- res$arsenal %>%
        select(TaggedPitchType, movement_tag, n, pct, avg_velo, avg_ivb, avg_hb) %>%
        mutate(Pitcher = res$pitcher_name, .before = 1)
    }
    
    df %>%
      mutate(
        pct = round(pct, 1),
        avg_velo = round(avg_velo, 1),
        avg_ivb = round(avg_ivb, 1),
        avg_hb = round(avg_hb, 1)
      ) %>%
      rename(Pitch = TaggedPitchType, Shape = movement_tag, N = n, `Usage%` = pct,
             Velo = avg_velo, IVB = avg_ivb, HB = avg_hb) %>%
      gt() %>%
      tab_header(title = "Pitcher Arsenal Profile",
                subtitle = "Pitch types with 3%+ usage, classified by movement shape") %>%
      cols_align(align = "center") %>%
      tab_options(table.font.size = "13px", column_labels.font.weight = "bold",
                 column_labels.background.color = "#e8f5f4") %>%
      tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Pitcher"))
  })
  
  # Breakdown table - shows hitter RV vs each pitch shape
  output$phm_breakdown_table <- render_gt({
    req(phm_calc_data())
    cd <- phm_calc_data()
    if (is.null(cd) || length(cd$results) == 0) return(NULL)
    
    if (cd$mode == "one_hitter") {
      # One hitter vs multiple pitchers
      all_rows <- lapply(cd$results, function(res) {
        breakdown <- res$arsenal %>%
          select(TaggedPitchType, movement_tag, pct, h_rv, p_rv, h_n, matchup_rv, weighted_contribution, family, h_fam_rv) %>%
          mutate(Pitcher = res$pitcher_name, .before = 1)
        
        # Add summary row
        summary_row <- tibble(
          Pitcher = res$pitcher_name,
          TaggedPitchType = "OVERALL",
          movement_tag = "",
          pct = 100,
          h_rv = res$platoon_rv,
          p_rv = NA_real_,
          h_n = NA_integer_,
          matchup_rv = res$overall_rv,
          weighted_contribution = res$overall_rv,
          family = "",
          h_fam_rv = NA_real_
        )
        bind_rows(breakdown, summary_row)
      })
      df <- bind_rows(all_rows)
      
      df %>%
        mutate(
          pct = round(pct, 1),
          h_rv = round(h_rv, 2),
          p_rv = round(p_rv, 2),
          h_fam_rv = round(h_fam_rv, 2),
          matchup_rv = round(matchup_rv, 2),
          weighted_contribution = round(weighted_contribution, 2)
        ) %>%
        rename(Pitch = TaggedPitchType, Shape = movement_tag, `Usage%` = pct,
               `H RV/100` = h_rv, `P RV/100` = p_rv, `H N` = h_n,
               `Family RV` = h_fam_rv,
               `Matchup RV` = matchup_rv, `Weighted` = weighted_contribution) %>%
        gt() %>%
        tab_header(
          title = paste("Matchup Breakdown:", cd$hitter),
          subtitle = "How each pitch shape contributes to the overall matchup score"
        ) %>%
        data_color(columns = c("Matchup RV"), fn = scales::col_numeric(
          palette = c("#c0392b", "#f5f5f5", "#1e8449"), domain = c(-8, 0, 8), na.color = "#eee")) %>%
        data_color(columns = c("H RV/100"), fn = scales::col_numeric(
          palette = c("#c0392b", "#f5f5f5", "#1e8449"), domain = c(-10, 0, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        sub_missing(missing_text = "-") %>%
        tab_style(style = list(cell_fill(color = "#e8f5f4"), cell_text(weight = "bold")),
                 locations = cells_body(rows = Pitch == "OVERALL")) %>%
        tab_options(table.font.size = "12px", column_labels.font.weight = "bold",
                   column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Pitcher"))
    } else {
      # One pitcher vs multiple hitters
      all_rows <- lapply(cd$results, function(res) {
        breakdown <- res$arsenal %>%
          select(TaggedPitchType, movement_tag, pct, h_rv, p_rv, h_n, matchup_rv, weighted_contribution, family, h_fam_rv) %>%
          mutate(Hitter = res$hitter_name, .before = 1)
        
        summary_row <- tibble(
          Hitter = res$hitter_name,
          TaggedPitchType = "OVERALL",
          movement_tag = "",
          pct = 100,
          h_rv = res$platoon_rv,
          p_rv = NA_real_,
          h_n = NA_integer_,
          matchup_rv = res$overall_rv,
          weighted_contribution = res$overall_rv,
          family = "",
          h_fam_rv = NA_real_
        )
        bind_rows(breakdown, summary_row)
      })
      df <- bind_rows(all_rows)
      
      df %>%
        mutate(
          pct = round(pct, 1),
          h_rv = round(h_rv, 2),
          p_rv = round(p_rv, 2),
          h_fam_rv = round(h_fam_rv, 2),
          matchup_rv = round(matchup_rv, 2),
          weighted_contribution = round(weighted_contribution, 2)
        ) %>%
        rename(Pitch = TaggedPitchType, Shape = movement_tag, `Usage%` = pct,
               `H RV/100` = h_rv, `P RV/100` = p_rv, `H N` = h_n,
               `Family RV` = h_fam_rv,
               `Matchup RV` = matchup_rv, `Weighted` = weighted_contribution) %>%
        gt() %>%
        tab_header(
          title = paste("Matchup Breakdown:", cd$pitcher),
          subtitle = paste0("Pitcher hand: ", cd$pitcher_hand, " | How each hitter performs vs this arsenal")
        ) %>%
        data_color(columns = c("Matchup RV"), fn = scales::col_numeric(
          palette = c("#1e8449", "#f5f5f5", "#c0392b"), domain = c(-8, 0, 8), na.color = "#eee")) %>%
        data_color(columns = c("H RV/100"), fn = scales::col_numeric(
          palette = c("#c0392b", "#f5f5f5", "#1e8449"), domain = c(-10, 0, 10), na.color = "#eee")) %>%
        cols_align(align = "center") %>%
        sub_missing(missing_text = "-") %>%
        tab_style(style = list(cell_fill(color = "#e8f5f4"), cell_text(weight = "bold")),
                 locations = cells_body(rows = Pitch == "OVERALL")) %>%
        tab_options(table.font.size = "12px", column_labels.font.weight = "bold",
                   column_labels.background.color = "#e8f5f4") %>%
        tab_style(style = cell_text(weight = "bold"), locations = cells_body(columns = "Hitter"))
    }
  })
            
  output$download_scout_pdf <- downloadHandler(
    filename = function() { paste0("scouting_report_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) {
      data <- scout_data()
      if (length(data) == 0) { showNotification("No hitters selected", type = "warning"); return() }
      
      pdf_title <- if (!is.null(input$pdf_custom_title) && nchar(trimws(input$pdf_custom_title)) > 0) {
        trimws(input$pdf_custom_title)
      } else {
        "HITTER SCOUTING REPORT"
      }
      
      pdf(file, width = 8.5, height = 12)  # Portrait tall
      
      MAX_HITTERS_PER_PAGE <- 6
      n_hitters <- length(data)
      n_pages <- ceiling(n_hitters / MAX_HITTERS_PER_PAGE)
      
      # ---- HELPER: Draw diamond ----
      draw_diamond <- function(cx, cy, size, filled = FALSE) {
        x_pts <- c(cx, cx + size, cx, cx - size, cx)
        y_pts <- c(cy + size * 1.0, cy, cy - size * 1.0, cy, cy + size * 1.0)
        fill_col <- if (filled) "#d4edda" else "white"
        grid::grid.polygon(x = x_pts, y = y_pts, default.units = "npc",
                          gp = grid::gpar(fill = fill_col, col = "gray40", lwd = 0.5))
      }
      
      # ---- HELPER: Draw strike zone (3x3 grid) next to diamond ----
      draw_zone <- function(cx, cy, w, h) {
        grid::grid.rect(x = cx, y = cy, width = w, height = h, default.units = "npc",
                       gp = grid::gpar(fill = "white", col = "gray40", lwd = 0.4))
        third_w <- w / 3; third_h <- h / 3
        for (i in 1:2) {
          grid::grid.segments(x0 = cx - w/2 + i * third_w, y0 = cy - h/2,
                             x1 = cx - w/2 + i * third_w, y1 = cy + h/2,
                             default.units = "npc", gp = grid::gpar(col = "gray75", lwd = 0.3))
          grid::grid.segments(x0 = cx - w/2, y0 = cy - h/2 + i * third_h,
                             x1 = cx + w/2, y1 = cy - h/2 + i * third_h,
                             default.units = "npc", gp = grid::gpar(col = "gray75", lwd = 0.3))
        }
      }
      
      # ---- HELPER: Draw one inning column (In/P lines, diamond, zone side by side) ----
      draw_inning_col <- function(cx, top_y, diamond_size, zone_w, zone_h) {
        # "In: ___" line
        grid::grid.text("In:", x = cx - 0.025, y = top_y, just = "left",
                       gp = grid::gpar(fontsize = 5, col = "gray30"))
        grid::grid.segments(x0 = cx + 0.0, y0 = top_y - 0.002,
                           x1 = cx + 0.045, y1 = top_y - 0.002,
                           default.units = "npc", gp = grid::gpar(col = "gray50", lwd = 0.3))
        # "P: ___" line
        grid::grid.text("P:", x = cx - 0.025, y = top_y - 0.012, just = "left",
                       gp = grid::gpar(fontsize = 5, col = "gray30"))
        grid::grid.segments(x0 = cx - 0.005, y0 = top_y - 0.014,
                           x1 = cx + 0.045, y1 = top_y - 0.014,
                           default.units = "npc", gp = grid::gpar(col = "gray50", lwd = 0.3))
        # Diamond (left) and Zone (right), side by side
        pair_y <- top_y - 0.038
        diamond_cx <- cx - 0.012
        zone_cx <- cx + 0.022
        draw_diamond(diamond_cx, pair_y, diamond_size, filled = TRUE)
        draw_zone(zone_cx, pair_y, zone_w, zone_h)
      }
      
      # ---- HELPER: Stat pill colors ----
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
      
      draw_stat_pill <- function(x, y, label, value, benchmark, higher_better = TRUE, width = 0.065) {
        colors <- get_stat_pill_color(value, benchmark, higher_better)
        grid::grid.roundrect(x = x, y = y, width = width, height = 0.012,
                            r = unit(0.3, "snpc"), default.units = "npc",
                            gp = grid::gpar(fill = colors$bg, col = "#888"))
        grid::grid.text(label, x = x, y = y, 
                       gp = grid::gpar(fontsize = 5, fontface = "bold"))
      }
      
      # ---- HELPER: Draw RHP/LHP note table ----
      draw_note_table <- function(x_start, y_top, width, hand_label, hand_color) {
        col_labels <- c("0-0", "Attack", "FB", "Offspeed", "2k")
        n_cols <- 5
        col_w <- width / n_cols
        banner_h <- 0.012
        header_h <- 0.010
        note_h <- 0.042
        
        # Banner bar
        grid::grid.rect(x = x_start + width/2, y = y_top, width = width, height = banner_h,
                       default.units = "npc",
                       gp = grid::gpar(fill = hand_color, col = NA))
        grid::grid.text(hand_label, x = x_start + width/2, y = y_top,
                       gp = grid::gpar(fontsize = 7, fontface = "bold", col = "white"))
        
        # Column headers
        header_y <- y_top - banner_h/2 - header_h/2
        for (i in 1:n_cols) {
          cx <- x_start + (i - 0.5) * col_w
          grid::grid.rect(x = cx, y = header_y, width = col_w, height = header_h,
                         default.units = "npc",
                         gp = grid::gpar(fill = "#f0f0f0", col = "gray60", lwd = 0.3))
          grid::grid.text(col_labels[i], x = cx, y = header_y,
                         gp = grid::gpar(fontsize = 5.5, fontface = "bold", col = "gray30"))
        }
        
        # Note cells (empty, to be filled in by hand when printed)
        note_y <- header_y - header_h/2 - note_h/2
        for (i in 1:n_cols) {
          cx <- x_start + (i - 0.5) * col_w
          grid::grid.rect(x = cx, y = note_y, width = col_w, height = note_h,
                         default.units = "npc",
                         gp = grid::gpar(fill = "white", col = "gray60", lwd = 0.3))
        }
        
        note_y - note_h/2
      }
      
      for (page in 1:n_pages) {
        if (page > 1) grid::grid.newpage()
        
        # Page header
        grid::grid.rect(x = 0.5, y = 0.982, width = 1, height = 0.028,
                       gp = grid::gpar(fill = "#006F71", col = NA))
        grid::grid.text(pdf_title, x = 0.5, y = 0.982,
                       gp = grid::gpar(fontsize = 12, fontface = "bold", col = "white"))
        grid::grid.text(format(Sys.Date(), "%m/%d/%Y"), x = 0.95, y = 0.982, just = "right",
                       gp = grid::gpar(fontsize = 6, col = "white"))
        grid::grid.text(paste0("Page ", page, " of ", n_pages), x = 0.05, y = 0.982, just = "left",
                       gp = grid::gpar(fontsize = 6, col = "white"))
        
        start_idx <- (page - 1) * MAX_HITTERS_PER_PAGE + 1
        end_idx <- min(page * MAX_HITTERS_PER_PAGE, n_hitters)
        page_hitters <- names(data)[start_idx:end_idx]
        
        # Each hitter gets ~0.155 of vertical space (6 per page)
        hitter_height <- 0.155
        
        for (h_idx in seq_along(page_hitters)) {
          h_name <- page_hitters[h_idx]
          profile <- data[[h_name]]$profile
          
          # Top of this hitter's row
          row_top <- 0.960 - (h_idx - 1) * hitter_height
          
          # === ROW 1: Header bar with name, grades, stats ===
          row1_y <- row_top - 0.005
          row1_bg <- if(h_idx %% 2 == 0) "#e8f5f4" else "#f0f0f0"
          grid::grid.rect(x = 0.5, y = row1_y, width = 0.97, height = 0.018,
                         gp = grid::gpar(fill = row1_bg, col = NA))
          
          # Name
          short_name <- format_short_name(h_name)
          grid::grid.text(short_name, x = 0.02, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = 8, fontface = "bold"))
          
          # Hand
          hand_label <- format_batter_hand(profile$hand)
          hand_color <- if (profile$hand == "Left") "#C62828" else if (profile$hand == "Switch") "#1565C0" else "black"
          grid::grid.text(hand_label, x = 0.10, y = row1_y, just = "left",
                         gp = grid::gpar(fontsize = 7, col = hand_color, fontface = "bold"))
          
          # Grade boxes (compact)
          grades <- c(profile$overall_grade, profile$game_power_grade, profile$contact_grade, 
                     profile$avoid_k_grade, profile$swing_dec_grade)
          grade_labels <- c("OVR", "GPwr", "Con", "AvK", "SwD")
          grade_x <- c(0.155, 0.195, 0.235, 0.275, 0.315)
          
          for (g_idx in 1:5) {
            grid::grid.roundrect(x = grade_x[g_idx], y = row1_y + 0.002,
                           width = 0.028, height = 0.014,
                           r = unit(0.3, "snpc"), default.units = "npc",
                           gp = grid::gpar(fill = grade_color_light(grades[g_idx]), col = "#888"))
            grid::grid.text(grades[g_idx], x = grade_x[g_idx], y = row1_y + 0.002,
                           gp = grid::gpar(fontsize = 5.5, fontface = "bold"))
            grid::grid.text(grade_labels[g_idx], x = grade_x[g_idx], y = row1_y - 0.007,
                           gp = grid::gpar(fontsize = 3.5, col = "gray40"))
          }
          
          # Stat pills (right side of header)
          h_raw_pdf <- active_data() %>% filter(Batter == h_name)
          fp_swing_pct <- if(sum(h_raw_pdf$Balls == 0 & h_raw_pdf$Strikes == 0, na.rm = TRUE) >= 10) {
            100 * sum(h_raw_pdf$is_swing[h_raw_pdf$Balls == 0 & h_raw_pdf$Strikes == 0], na.rm = TRUE) /
              sum(h_raw_pdf$Balls == 0 & h_raw_pdf$Strikes == 0, na.rm = TRUE)
          } else NA
          iz_swing_pct <- if(sum(h_raw_pdf$in_zone, na.rm = TRUE) >= 10) {
            100 * sum(h_raw_pdf$z_swing, na.rm = TRUE) / sum(h_raw_pdf$in_zone, na.rm = TRUE)
          } else NA
          twok_chase_pct <- if(sum(h_raw_pdf$Strikes == 2 & h_raw_pdf$out_of_zone == 1, na.rm = TRUE) >= 10) {
            100 * sum(h_raw_pdf$Strikes == 2 & h_raw_pdf$chase == 1, na.rm = TRUE) /
              sum(h_raw_pdf$Strikes == 2 & h_raw_pdf$out_of_zone == 1, na.rm = TRUE)
          } else NA
          
          stat_x <- c(0.375, 0.440, 0.505, 0.565, 0.625, 0.685, 0.745, 0.805, 0.870)
          pill_w <- 0.058
          
          rv_val <- if(!is.na(profile$rv100)) sprintf("%+.1f", profile$rv100) else "-"
          draw_stat_pill(stat_x[1], row1_y, paste0("RV:", rv_val), profile$rv100, 0, TRUE, pill_w)
          woba_val <- if(!is.na(profile$woba)) sprintf(".%03d", round(profile$woba * 1000)) else "-"
          draw_stat_pill(stat_x[2], row1_y, paste0("wOBA:", woba_val), profile$woba, 0.320, TRUE, 0.062)
          ev90_val <- if(!is.na(profile$ev90)) sprintf("%.0f", profile$ev90) else "-"
          draw_stat_pill(stat_x[3], row1_y, paste0("EV90:", ev90_val), profile$ev90, 95, TRUE, pill_w)
          k_val <- if(!is.na(profile$k_pct)) sprintf("%.0f%%", profile$k_pct) else "-"
          draw_stat_pill(stat_x[4], row1_y, paste0("K:", k_val), profile$k_pct, 22, FALSE, pill_w)
          whiff_val <- if(!is.na(profile$whiff_pct)) sprintf("%.0f%%", profile$whiff_pct) else "-"
          draw_stat_pill(stat_x[5], row1_y, paste0("Wh:", whiff_val), profile$whiff_pct, 25, FALSE, pill_w)
          fp_val <- if(!is.na(fp_swing_pct)) sprintf("%.0f%%", fp_swing_pct) else "-"
          draw_stat_pill(stat_x[6], row1_y, paste0("FPS:", fp_val), fp_swing_pct, 30, FALSE, pill_w)
          iz_val <- if(!is.na(iz_swing_pct)) sprintf("%.0f%%", iz_swing_pct) else "-"
          draw_stat_pill(stat_x[7], row1_y, paste0("IZS:", iz_val), iz_swing_pct, 68, TRUE, pill_w)
          twok_val <- if(!is.na(twok_chase_pct)) sprintf("%.0f%%", twok_chase_pct) else "-"
          draw_stat_pill(stat_x[8], row1_y, paste0("2KC:", twok_val), twok_chase_pct, 33, FALSE, pill_w)
          bb_val <- if(!is.na(profile$bb_pct)) sprintf("%.0f%%", profile$bb_pct) else "-"
          draw_stat_pill(stat_x[9], row1_y, paste0("BB:", bb_val), profile$bb_pct, 9, TRUE, pill_w)
          
          # === ROW 2: 5 Inning columns (In/P + Diamond + Zone each) + Game Notes box ===
          inning_top_y <- row1_y - 0.018
          diamond_size <- 0.012
          zone_w <- 0.022
          zone_h <- 0.024
          
          # 5 inning columns spread across left ~65%
          inning_x_positions <- seq(0.06, 0.58, length.out = 5)
          
          for (d_idx in 1:5) {
            draw_inning_col(inning_x_positions[d_idx], inning_top_y, diamond_size, zone_w, zone_h)
          }
          
          # Game Notes box on right side — extends to right edge
          notes_box_right <- 0.97
          notes_box_left <- 0.65
          notes_box_w <- notes_box_right - notes_box_left
          notes_box_x <- notes_box_left + notes_box_w / 2
          notes_box_y <- inning_top_y - 0.025
          notes_box_h <- 0.058
          grid::grid.rect(x = notes_box_x, y = notes_box_y, width = notes_box_w, height = notes_box_h,
                         default.units = "npc",
                         gp = grid::gpar(fill = "white", col = "gray50", lwd = 0.5))
          grid::grid.text("In Game Notes:", x = notes_box_left + 0.005, 
                         y = notes_box_y + notes_box_h/2 - 0.006, just = "left",
                         gp = grid::gpar(fontsize = 5.5, fontface = "bold", col = "gray40"))
          for (ln in 1:4) {
            ln_y <- notes_box_y + notes_box_h/2 - 0.006 - ln * 0.010
            grid::grid.segments(x0 = notes_box_left + 0.005, y0 = ln_y,
                               x1 = notes_box_right - 0.005, y1 = ln_y,
                               default.units = "npc", gp = grid::gpar(col = "gray85", lwd = 0.2))
          }
          
          # === ROW 3: RHP and LHP note tables side by side ===
          table_top_y <- inning_top_y - 0.060
          table_width <- 0.47
          
          draw_note_table(0.02, table_top_y, table_width, "RHP", "#2962FF")
          draw_note_table(0.51, table_top_y, table_width, "LHP", "#C62828")
          
          # Separator line between hitters
          sep_y <- row_top - hitter_height + 0.003
          if (h_idx < length(page_hitters)) {
            grid::grid.segments(x0 = 0.02, y0 = sep_y, x1 = 0.98, y1 = sep_y,
                               default.units = "npc", gp = grid::gpar(col = "gray70", lwd = 0.5, lty = 2))
          }
        }
        
        # Footer
        grid::grid.text("Data: TrackMan | Coastal Carolina Baseball Analytics", x = 0.5, y = 0.008,
                       gp = grid::gpar(fontsize = 5, col = "gray50", fontface = "italic"))
      }
      
      dev.off()
    }
  )
        
}  # end server function

shinyApp(ui = ui, server = server)
