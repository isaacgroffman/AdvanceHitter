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

# ============================================================================
# DATA LOADING
# ============================================================================

cat("Loading TrackMan data...\n")

# SEC Teams for percentile comparison pool
sec_teams <- c("ALA_CRI", "ARK_RAZ", "AUB_TIG", "FLA_GAT", "GEO_BUL", "KEN_WIL", "LSU_TIG", "OLE_REB",
               "MSU_BDG", "MIZ_TIG", "SOU_GAM", "TEN_VOL", "TEX_AGG", "VAN_COM", "OKL_SOO", "TEX_LON")

if (exists("TM2025")) {
  tm_data <- TM2025
} else if (exists("TM25")) {
  tm_data <- TM25
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
    xBA = c(rep(NA, n*0.75), runif(n*0.25, 0.1, 0.4)),
    totalbases = c(rep(NA, n*0.75), sample(0:4, n*0.25, replace = TRUE)),
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
  if (!"xBA" %in% names(df)) {
    df <- df %>% mutate(xBA = NA_real_)
  }
  if (!"totalbases" %in% names(df)) {
    df <- df %>% mutate(totalbases = NA_real_)
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

# Create SEC pool for percentile comparisons
sec_pool_data <- tm_data %>% 
  filter(BatterTeam %in% sec_teams)

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
# SEC PERCENTILE CHART FUNCTION (Overall Only)
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
  
  # K% lower is better for hitter, BB% higher is better for hitter
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
  
  # K% lower is better, BB% higher is better for hitter
  lower_better <- c("K%", "Whiff%", "Chase%", "O-Swing%")
  
  percentiles <- sapply(seq_along(metrics), function(i) {
    v <- batter_vals[i]; pop <- pool_lists[[i]]
    if (all(is.na(pop)) || is.na(v) || is.infinite(v)) return(NA_real_)
    if (metrics[i] %in% lower_better) {
      mean(pop >= v, na.rm = TRUE) * 100  # Lower value = higher percentile
    } else {
      mean(pop <= v, na.rm = TRUE) * 100  # Higher value = higher percentile
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
# GRADE METRICS CALCULATION (Z-score based)
# ============================================================================

calculate_grade_metrics <- function(tm_data) {
  batter_stats <- tm_data %>%
    filter(!is.na(TaggedPitchType)) %>%
    group_by(Batter) %>%
    summarise(
      n_pa = sum(is_pa, na.rm = TRUE),
      sdrv = 100 * mean(mean_DRE_bat, na.rm = TRUE),
      woba_avg = mean(woba, na.rm = TRUE),
      iso = (sum(totalbases, na.rm = TRUE) / pmax(1, sum(is_ab, na.rm = TRUE))) - 
            (sum(is_hit, na.rm = TRUE) / pmax(1, sum(is_ab, na.rm = TRUE))),
      xpf = sum(totalbases[PitchCall == "InPlay"], na.rm = TRUE) /
            pmax(1, sum(is_hit[PitchCall == "InPlay"], na.rm = TRUE)),
      zone_con = 100 * (1 - sum(in_zone_whiff, na.rm = TRUE) / pmax(1, sum(z_swing, na.rm = TRUE))),
      k_pct = 100 * sum(is_k, na.rm = TRUE) / pmax(1, sum(is_pa, na.rm = TRUE)),
      ev90 = as.numeric(quantile(ExitSpeed[PitchCall == "InPlay"], 0.9, na.rm = TRUE)),
      con2k = 100 * (1 - sum(is_whiff[Strikes == 2], na.rm = TRUE) / 
                     pmax(1, sum(is_swing[Strikes == 2], na.rm = TRUE))),
      .groups = "drop"
    ) %>% filter(n_pa >= 20)
  
  # Calculate means and SDs for z-score grading
  list(
    sdrv_mean = mean(batter_stats$sdrv, na.rm = TRUE),
    sdrv_sd = sd(batter_stats$sdrv, na.rm = TRUE),
    woba_mean = mean(batter_stats$woba_avg, na.rm = TRUE),
    woba_sd = sd(batter_stats$woba_avg, na.rm = TRUE),
    iso_mean = mean(batter_stats$iso, na.rm = TRUE),
    iso_sd = sd(batter_stats$iso, na.rm = TRUE),
    xpf_mean = mean(batter_stats$xpf, na.rm = TRUE),
    xpf_sd = sd(batter_stats$xpf, na.rm = TRUE),
    zone_con_mean = mean(batter_stats$zone_con, na.rm = TRUE),
    zone_con_sd = sd(batter_stats$zone_con, na.rm = TRUE),
    k_mean = mean(batter_stats$k_pct, na.rm = TRUE),
    k_sd = sd(batter_stats$k_pct, na.rm = TRUE),
    ev90_mean = mean(batter_stats$ev90, na.rm = TRUE),
    ev90_sd = sd(batter_stats$ev90, na.rm = TRUE),
    con2k_mean = mean(batter_stats$con2k, na.rm = TRUE),
    con2k_sd = sd(batter_stats$con2k, na.rm = TRUE)
  )
}

grade_metrics <- calculate_grade_metrics(sec_pool_data)

# Z-score to grade conversion
zscore_to_grade <- function(z) {
  grade <- z * 10 + 50
  round(pmax(20, pmin(80, grade)))
}

calculate_hitter_profile <- function(hitter_name, tm_data, grade_metrics, pitcher_hand = "All") {
  h_data <- tm_data %>% filter(Batter == hitter_name)
  if (nrow(h_data) < 20) return(NULL)
  
  if (pitcher_hand != "All") {
    h_data <- h_data %>% filter(PitcherThrows == pitcher_hand)
    if (nrow(h_data) < 10) return(NULL)
  }
  
  h_side <- names(which.max(table(h_data$BatterSide)))
  n_total <- nrow(h_data)
  n_pa <- sum(h_data$is_pa, na.rm = TRUE)
  n_ab <- sum(h_data$is_ab, na.rm = TRUE)
  
  # Calculate stats
  sdrv <- 100 * mean(h_data$mean_DRE_bat, na.rm = TRUE)
  # woba already calculated above, used for contact grade
  iso <- (sum(h_data$totalbases, na.rm = TRUE) / pmax(1, n_ab)) - 
         (sum(h_data$is_hit, na.rm = TRUE) / pmax(1, n_ab))
  xpf <- sum(h_data$totalbases[h_data$PitchCall == "InPlay"], na.rm = TRUE) /
         pmax(1, sum(h_data$is_hit[h_data$PitchCall == "InPlay"], na.rm = TRUE))
  zone_con <- 100 * (1 - sum(h_data$in_zone_whiff, na.rm = TRUE) / pmax(1, sum(h_data$z_swing, na.rm = TRUE)))
  k_pct <- 100 * sum(h_data$is_k, na.rm = TRUE) / pmax(1, n_pa)
  ev90 <- if(sum(h_data$PitchCall == "InPlay", na.rm = TRUE) >= 10) {
    as.numeric(quantile(h_data$ExitSpeed[h_data$PitchCall == "InPlay"], 0.9, na.rm = TRUE))
  } else NA
  con2k <- 100 * (1 - sum(h_data$is_whiff[h_data$Strikes == 2], na.rm = TRUE) / 
                  pmax(1, sum(h_data$is_swing[h_data$Strikes == 2], na.rm = TRUE)))
  
  rv100 <- sdrv
  woba <- mean(h_data$woba, na.rm = TRUE)
  wobacon <- mean(h_data$wobacon, na.rm = TRUE)
  slg <- mean(h_data$slg, na.rm = TRUE)
  bb_pct <- 100 * sum(h_data$is_walk, na.rm = TRUE) / pmax(1, n_pa)
  
  # Plate discipline
  n_swings <- sum(h_data$is_swing, na.rm = TRUE)
  n_ooz <- sum(h_data$out_of_zone, na.rm = TRUE)
  n_iz <- sum(h_data$in_zone, na.rm = TRUE)
  
  whiff_pct <- 100 * sum(h_data$is_whiff, na.rm = TRUE) / pmax(1, n_swings)
  chase_pct <- 100 * sum(h_data$chase, na.rm = TRUE) / pmax(1, n_ooz)
  zone_con_pct <- zone_con
  swing_pct <- 100 * n_swings / n_total
  z_swing_pct <- 100 * sum(h_data$z_swing, na.rm = TRUE) / pmax(1, n_iz)
  o_swing_pct <- chase_pct
  o_contact_pct <- if (sum(h_data$chase, na.rm = TRUE) >= 5) {
    chase_data <- h_data %>% filter(chase == 1)
    100 * (1 - sum(chase_data$is_whiff, na.rm = TRUE) / nrow(chase_data))
  } else NA
  
  put_away_pct <- if (sum(h_data$TwoStrikeInd, na.rm = TRUE) >= 10) {
    100 * sum(h_data$is_put_away[h_data$TwoStrikeInd == 1], na.rm = TRUE) / sum(h_data$TwoStrikeInd, na.rm = TRUE)
  } else NA
  
  # Batted ball data
  bip_data <- h_data %>% filter(PitchCall == "InPlay", !is.na(ExitSpeed))
  ev_mean <- if (nrow(bip_data) >= 5) mean(bip_data$ExitSpeed, na.rm = TRUE) else NA
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
  
  # Calculate grades using z-scores
  gm <- grade_metrics
  swing_dec_grade <- zscore_to_grade((sdrv - gm$sdrv_mean) / pmax(0.001, gm$sdrv_sd))
  
  game_power_z <- if(!is.na(xpf) && !is.na(iso) && !is.na(gm$xpf_sd) && !is.na(gm$iso_sd) && gm$xpf_sd > 0 && gm$iso_sd > 0) {
    ((xpf - gm$xpf_mean) / gm$xpf_sd + (iso - gm$iso_mean) / gm$iso_sd) / 2
  } else 0
  power_grade <- zscore_to_grade(game_power_z)
  
  raw_power_grade <- if(!is.na(ev90) && !is.na(gm$ev90_sd) && gm$ev90_sd > 0) {
    zscore_to_grade((ev90 - gm$ev90_mean) / gm$ev90_sd)
  } else 50
  
  # Avoid K: lower K% is better, higher con2k is better
  avoid_k_z <- if(!is.na(k_pct) && !is.na(con2k) && !is.na(gm$k_sd) && !is.na(gm$con2k_sd) && gm$k_sd > 0 && gm$con2k_sd > 0) {
    ((gm$k_mean - k_pct) / gm$k_sd + (con2k - gm$con2k_mean) / gm$con2k_sd) / 2
  } else 0
  avoid_k_grade <- zscore_to_grade(avoid_k_z)
  
  # Contact: higher zone_con is better, higher wOBA is better
  contact_z <- if(!is.na(zone_con) && !is.na(woba) && !is.na(gm$zone_con_sd) && !is.na(gm$woba_sd) && gm$zone_con_sd > 0 && gm$woba_sd > 0) {
    ((zone_con - gm$zone_con_mean) / gm$zone_con_sd + (woba - gm$woba_mean) / gm$woba_sd) / 2
  } else 0
  contact_grade <- zscore_to_grade(contact_z)
  
  overall_grade <- round(mean(c(power_grade, raw_power_grade, contact_grade, avoid_k_grade, swing_dec_grade), na.rm = TRUE))
  
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
  
  if (pitch_family != "All") data <- data %>% filter(PitchFamily == pitch_family)
  if (pitcher_hand != "All") data <- data %>% filter(PitcherThrows == pitcher_hand)
  
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
")

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(app_css),
    # Debounce script for text inputs
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        Shiny.inputBindings.getBindings().forEach(function(binding) {
          if (binding.name === 'shiny.textInput' || binding.name === 'shiny.textareaInput') {
            var originalGetValue = binding.binding.getValue;
          }
        });
      });
    "))
  ),
  div(class = "app-header",
      div(div(class = "header-title", "Hitter Scouting Cards"), div(class = "header-subtitle", "SEC Pool Comparison"))),
  
  div(class = "stats-header", h3("Enhanced Hitter Scouting Cards"), p("Grades based on SEC z-scores")),
  
  fluidRow(
    column(3,
           div(class = "chart-box",
               div(class = "chart-box-title", "Select Hitters"),
               selectizeInput("scout_hitters", "Add Hitters:", choices = NULL, multiple = TRUE, 
                              options = list(placeholder = "Search hitters...", maxItems = 30)),
               hr(),
               div(class = "scroll-container", style = "max-height: 500px;", uiOutput("hitter_list_ui")),
               hr(),
               downloadButton("download_scout_pdf", "Download Report (PDF)", class = "btn-bronze", style = "width: 100%;"))),
    column(9, uiOutput("hitter_detail_panel")))
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  scout_data <- reactiveVal(list())
  selected_hitter <- reactiveVal(NULL)
  
  # Debounced note values to prevent lag
  note_values <- reactiveValues()
  
  observe({ updateSelectizeInput(session, "scout_hitters", choices = all_hitters, server = TRUE) })
  
  observeEvent(input$scout_hitters, {
    hitters <- input$scout_hitters
    current_data <- scout_data()
    for (h in hitters) {
      if (is.null(current_data[[h]])) {
        profile <- calculate_hitter_profile(h, tm_data, grade_metrics)
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
    
    rhp_profile <- calculate_hitter_profile(h_name, tm_data, grade_metrics, "Right")
    lhp_profile <- calculate_hitter_profile(h_name, tm_data, grade_metrics, "Left")
    
    last15_dates <- get_last_n_games(tm_data, h_name, 15)
    last15_data <- h_raw %>% filter(Date %in% last15_dates)
    last15_stats <- calc_count_stats(last15_data)
    
    div(
      div(class = "chart-box",
          div(class = "hitter-header",
              div(class = "hitter-info", h3(h_name), div(class = "hitter-meta", paste0("Bats: ", profile$hand, " | PA: ", profile$n_pa, " | Pitches: ", profile$n))),
              div(class = "grade-row", create_grade_box(profile$overall_grade, "OVR"), create_grade_box(profile$power_grade, "PWR"),
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
                                tags$tr(tags$td(class = "splits-label", "Last 15 Games"), tags$td(last15_stats$n),
                                        tags$td(create_rv_pill(last15_stats$rv100, benchmarks$rv100, TRUE)), 
                                        tags$td(create_woba_pill(last15_stats$woba, benchmarks$woba, TRUE)),
                                        tags$td(create_pct_pill(last15_stats$whiff, benchmarks$whiff_pct, FALSE)), 
                                        tags$td(create_pct_pill(last15_stats$chase, benchmarks$chase_pct, FALSE)))))),
               
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
               
               div(class = "detail-section", div(class = "section-header", "Spray Charts"),
                   fluidRow(column(4, plotOutput(paste0("spray_all_", hitter_id), height = "130px")),
                            column(4, plotOutput(paste0("spray_rhp_", hitter_id), height = "130px")),
                            column(4, plotOutput(paste0("spray_lhp_", hitter_id), height = "130px"))),
                   div(class = "pitch-legend", span(class = "legend-item", span(class = "legend-dot", style = "background:#E41A1C;"), "HR"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#377EB8;"), "3B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#4DAF4A;"), "2B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:#984EA3;"), "1B"),
                       span(class = "legend-item", span(class = "legend-dot", style = "background:gray;"), "Out"))))),
      
      # Whiff Zones by Pitch Type and Pitcher Hand
      fluidRow(
        column(12,
               div(class = "detail-section", div(class = "section-header", "Whiff Zones by Pitch Type & Pitcher Hand"),
                   fluidRow(
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Fastballs")),
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Breaking")),
                     column(4, div(style = "text-align: center; font-weight: bold; margin-bottom: 5px;", "Offspeed"))),
                   fluidRow(
                     column(4, plotOutput(paste0("hm_whiff_fb_rhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_bb_rhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_os_rhp_", hitter_id), height = "100px"))),
                   fluidRow(
                     column(4, plotOutput(paste0("hm_whiff_fb_lhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_bb_lhp_", hitter_id), height = "100px")),
                     column(4, plotOutput(paste0("hm_whiff_os_lhp_", hitter_id), height = "100px")))))),
      
      fluidRow(
        column(6,
               div(class = "detail-section", div(class = "section-header", "Damage Zones"),
                   fluidRow(column(6, plotOutput(paste0("hm_damage_rhp_", hitter_id), height = "120px")),
                            column(6, plotOutput(paste0("hm_damage_lhp_", hitter_id), height = "120px"))))),
        column(6,
               div(class = "detail-section", div(class = "section-header", "Hits/Outs: vs RHP | vs LHP"),
                   fluidRow(column(3, plotOutput(paste0("ho_rhp_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_rhp_outs_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_lhp_hits_", hitter_id), height = "100px")),
                            column(3, plotOutput(paste0("ho_lhp_outs_", hitter_id), height = "100px")))))),
      
      # SEC Percentile Chart - OVERALL ONLY
      fluidRow(
        column(12,
               div(class = "detail-section", div(class = "section-header section-header-purple", "SEC Percentiles (Overall)"),
                   fluidRow(column(8, offset = 2, plotOutput(paste0("pct_overall_", hitter_id), height = "320px")))))),
      
      # Scouting Notes with debounced inputs
      div(class = "detail-section", div(class = "section-header", "Scouting Notes"),
          fluidRow(
            column(4, textAreaInput(paste0("lhp_plan_", hitter_id), "vs LHP Plan:", value = h_data$lhp_plan, rows = 3, placeholder = "Attack plan vs lefties")),
            column(4, textAreaInput(paste0("rhp_plan_", hitter_id), "vs RHP Plan:", value = h_data$rhp_plan, rows = 3, placeholder = "Attack plan vs righties")),
            column(4, textAreaInput(paste0("notes_", hitter_id), "General Notes:", value = h_data$overall_notes, rows = 3, placeholder = "Overall notes"))))
    )
  })
  
  observe({
    h_name <- selected_hitter()
    if (is.null(h_name)) return()
    hitter_id <- gsub("[^A-Za-z0-9]", "_", h_name)
    h_raw <- tm_data %>% filter(Batter == h_name)
    
    output[[paste0("spray_all_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "Overall", "all") }, bg = "transparent")
    output[[paste0("spray_rhp_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw, "vs RHP", "rhp") }, bg = "transparent")
    output[[paste0("spray_lhp_", hitter_id)]] <- renderPlot({ create_spray_chart(h_raw %>% filter(PitcherThrows == "Left"), "vs LHP", "all") }, bg = "transparent")
    
    output[[paste0("hm_whiff_fb_rhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "FB vs RHP", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "FB", "Right") }, bg = "transparent")
    output[[paste0("hm_whiff_bb_rhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "BB vs RHP", "whiff", c("white", "#E1BEE7", "#9C27B0", "#4A148C"), "BB", "Right") }, bg = "transparent")
    output[[paste0("hm_whiff_os_rhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "OS vs RHP", "whiff", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "OS", "Right") }, bg = "transparent")
    output[[paste0("hm_whiff_fb_lhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "FB vs LHP", "whiff", c("white", "#FFCCCC", "#FF6666", "#CC0000"), "FB", "Left") }, bg = "transparent")
    output[[paste0("hm_whiff_bb_lhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "BB vs LHP", "whiff", c("white", "#E1BEE7", "#9C27B0", "#4A148C"), "BB", "Left") }, bg = "transparent")
    output[[paste0("hm_whiff_os_lhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "OS vs LHP", "whiff", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "OS", "Left") }, bg = "transparent")
    
    output[[paste0("hm_damage_rhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "Damage vs RHP", "damage", c("white", "#C8E6C9", "#4CAF50", "#1B5E20"), "All", "Right") }, bg = "transparent")
    output[[paste0("hm_damage_lhp_", hitter_id)]] <- renderPlot({ create_heatmap(h_raw, "Damage vs LHP", "damage", c("white", "#BBDEFB", "#2196F3", "#0D47A1"), "All", "Left") }, bg = "transparent")
    
    output[[paste0("ho_rhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "rhp_hits") }, bg = "transparent")
    output[[paste0("ho_rhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "rhp_outs") }, bg = "transparent")
    output[[paste0("ho_lhp_hits_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "lhp_hits") }, bg = "transparent")
    output[[paste0("ho_lhp_outs_", hitter_id)]] <- renderPlot({ create_hit_out_chart(tm_data, h_name, "lhp_outs") }, bg = "transparent")
    
    # Only Overall percentile chart
    output[[paste0("pct_overall_", hitter_id)]] <- renderPlot({
      create_sec_percentile_chart(h_raw, sec_pool_data, h_name, "Overall")
    }, bg = "transparent")
  })
  
  # Debounced note saving - only update every 1 second
  observe({
    h_name <- selected_hitter()
    if (is.null(h_name)) return()
    hitter_id <- gsub("[^A-Za-z0-9]", "_", h_name)
    data <- scout_data()
    
    if (!is.null(data[[h_name]])) {
      invalidateLater(1000, session)  # Only check every 1 second
      isolate({
        lhp_val <- input[[paste0("lhp_plan_", hitter_id)]]
        rhp_val <- input[[paste0("rhp_plan_", hitter_id)]]
        notes_val <- input[[paste0("notes_", hitter_id)]]
        
        if (!is.null(lhp_val) && data[[h_name]]$lhp_plan != lhp_val) data[[h_name]]$lhp_plan <- lhp_val
        if (!is.null(rhp_val) && data[[h_name]]$rhp_plan != rhp_val) data[[h_name]]$rhp_plan <- rhp_val
        if (!is.null(notes_val) && data[[h_name]]$overall_notes != notes_val) data[[h_name]]$overall_notes <- notes_val
        
        scout_data(data)
      })
    }
  })
  
  # PDF DOWNLOAD - Portrait layout with compact rows for each hitter
  output$download_scout_pdf <- downloadHandler(
    filename = function() { paste0("scouting_report_", format(Sys.Date(), "%Y%m%d"), ".pdf") },
    content = function(file) {
      data <- scout_data()
      if (length(data) == 0) { showNotification("No hitters selected", type = "warning"); return() }
      
      # Portrait 8.5 x 11
      pdf(file, width = 8.5, height = 11)
      
      # Calculate row height based on number of hitters
      n_hitters <- length(data)
      row_height <- min(0.18, 0.85 / n_hitters)  # Max height per hitter row
      
      grid::grid.newpage()
      
      # Header
      grid::grid.rect(x = 0.5, y = 0.98, width = 1, height = 0.035, gp = grid::gpar(fill = "#6A1B9A", col = NA))
      grid::grid.text("HITTER SCOUTING REPORT", x = 0.5, y = 0.98, gp = grid::gpar(fontsize = 14, fontface = "bold", col = "white"))
      grid::grid.text(format(Sys.Date(), "%B %d, %Y"), x = 0.95, y = 0.98, just = "right", gp = grid::gpar(fontsize = 7, col = "white"))
      
      # Draw each hitter row
      for (idx in seq_along(names(data))) {
        h_name <- names(data)[idx]
        profile <- data[[h_name]]$profile
        notes <- data[[h_name]]
        
        # Calculate Y position for this hitter's row
        row_top <- 0.95 - (idx - 1) * row_height
        
        # Background for alternating rows
        if (idx %% 2 == 0) {
          grid::grid.rect(x = 0.5, y = row_top - row_height/2, width = 0.98, height = row_height, 
                         gp = grid::gpar(fill = "#f8f8f8", col = NA))
        }
        
        # Border around hitter section
        grid::grid.rect(x = 0.5, y = row_top - row_height/2, width = 0.98, height = row_height, 
                       gp = grid::gpar(fill = NA, col = "#006F71", lwd = 0.5))
        
        # ROW 1: Name, Hand, Grades, Spray %, Stats
        row1_y <- row_top - 0.015
        
        # Name and hand
        grid::grid.text(h_name, x = 0.03, y = row1_y, just = "left", gp = grid::gpar(fontsize = 9, fontface = "bold"))
        grid::grid.text(paste0("(", profile$hand, ")"), x = 0.15, y = row1_y, just = "left", gp = grid::gpar(fontsize = 7, col = "gray50"))
        
        # Grades
        grades <- c(profile$overall_grade, profile$power_grade, profile$raw_power_grade, 
                    profile$contact_grade, profile$avoid_k_grade, profile$swing_dec_grade)
        grade_labels <- c("OVR", "PWR", "RAW", "CON", "AvK", "SwD")
        for (g_idx in 1:6) {
          gx <- 0.22 + (g_idx - 1) * 0.055
          grid::grid.rect(x = gx, y = row1_y, width = 0.04, height = 0.018, 
                         gp = grid::gpar(fill = grade_color_light(grades[g_idx]), col = "gray60", lwd = 0.3))
          grid::grid.text(grades[g_idx], x = gx, y = row1_y, gp = grid::gpar(fontsize = 6, fontface = "bold"))
          grid::grid.text(grade_labels[g_idx], x = gx, y = row1_y - 0.012, gp = grid::gpar(fontsize = 4, col = "gray50"))
        }
        
        # Spray tendencies
        spray_x <- 0.58
        pull_txt <- if(!is.na(profile$pull_pct)) sprintf("%.0f%%", profile$pull_pct) else "-"
        mid_txt <- if(!is.na(profile$middle_pct)) sprintf("%.0f%%", profile$middle_pct) else "-"
        oppo_txt <- if(!is.na(profile$oppo_pct)) sprintf("%.0f%%", profile$oppo_pct) else "-"
        grid::grid.text(paste0("P:", pull_txt, " M:", mid_txt, " O:", oppo_txt), x = spray_x, y = row1_y, 
                       just = "left", gp = grid::gpar(fontsize = 6))
        
        # Key stats: RV/100, wOBA, EV90, K%, BB%
        stats_x <- 0.76
        grid::grid.text(sprintf("RV:%+.1f", profile$rv100), x = stats_x, y = row1_y, just = "left", gp = grid::gpar(fontsize = 6))
        grid::grid.text(sprintf("wOBA:%.3f", ifelse(is.na(profile$woba), 0, profile$woba)), x = stats_x + 0.07, y = row1_y, just = "left", gp = grid::gpar(fontsize = 6))
        grid::grid.text(sprintf("EV90:%.0f", ifelse(is.na(profile$ev90), 0, profile$ev90)), x = stats_x + 0.15, y = row1_y, just = "left", gp = grid::gpar(fontsize = 6))
        
        row1b_y <- row1_y - 0.018
        grid::grid.text(sprintf("K%%:%.0f%%", profile$k_pct), x = stats_x, y = row1b_y, just = "left", gp = grid::gpar(fontsize = 6))
        grid::grid.text(sprintf("BB%%:%.0f%%", profile$bb_pct), x = stats_x + 0.07, y = row1b_y, just = "left", gp = grid::gpar(fontsize = 6))
        
        # ROW 2: Notes (LHP Plan, RHP Plan, Overall)
        row2_y <- row1_y - 0.038
        grid::grid.rect(x = 0.5, y = row2_y, width = 0.94, height = 0.022, gp = grid::gpar(fill = "#f0f0f0", col = "gray70", lwd = 0.3))
        
        lhp_note <- if(nchar(notes$lhp_plan) > 0) paste0("LHP: ", substr(notes$lhp_plan, 1, 35)) else "LHP:"
        rhp_note <- if(nchar(notes$rhp_plan) > 0) paste0("RHP: ", substr(notes$rhp_plan, 1, 35)) else "RHP:"
        overall_note <- if(nchar(notes$overall_notes) > 0) paste0("Notes: ", substr(notes$overall_notes, 1, 40)) else "Notes:"
        
        grid::grid.text(lhp_note, x = 0.04, y = row2_y, just = "left", gp = grid::gpar(fontsize = 5))
        grid::grid.text(rhp_note, x = 0.35, y = row2_y, just = "left", gp = grid::gpar(fontsize = 5))
        grid::grid.text(overall_note, x = 0.66, y = row2_y, just = "left", gp = grid::gpar(fontsize = 5))
        
        # ROW 3: 5 Diamonds for in-game scoring with pitcher/inning spots
        row3_y <- row2_y - 0.035
        diamond_size <- 0.025
        for (d in 1:5) {
          dx <- 0.10 + (d - 1) * 0.18
          
          # Draw diamond (rotated square)
          diamond_pts_x <- c(dx, dx + diamond_size, dx, dx - diamond_size, dx)
          diamond_pts_y <- c(row3_y + diamond_size, row3_y, row3_y - diamond_size, row3_y, row3_y + diamond_size)
          grid::grid.polygon(x = diamond_pts_x, y = diamond_pts_y, 
                            gp = grid::gpar(fill = "white", col = "black", lwd = 0.5))
          
          # Small boxes for pitcher/inning below diamond
          grid::grid.rect(x = dx - 0.025, y = row3_y - diamond_size - 0.012, width = 0.04, height = 0.012, 
                         gp = grid::gpar(fill = "white", col = "gray50", lwd = 0.3))
          grid::grid.text("P:", x = dx - 0.045, y = row3_y - diamond_size - 0.012, just = "right", gp = grid::gpar(fontsize = 4))
          
          grid::grid.rect(x = dx + 0.025, y = row3_y - diamond_size - 0.012, width = 0.025, height = 0.012, 
                         gp = grid::gpar(fill = "white", col = "gray50", lwd = 0.3))
          grid::grid.text("Inn:", x = dx + 0.005, y = row3_y - diamond_size - 0.012, just = "right", gp = grid::gpar(fontsize = 4))
        }
        
        # In-game notes area
        grid::grid.rect(x = 0.90, y = row3_y, width = 0.12, height = 0.04, 
                       gp = grid::gpar(fill = "white", col = "gray50", lwd = 0.3))
        grid::grid.text("Game Notes:", x = 0.85, y = row3_y + 0.015, just = "left", gp = grid::gpar(fontsize = 4, col = "gray50"))
      }
      
      # Footer
      grid::grid.text("Generated by Hitter Scouting Cards App", x = 0.5, y = 0.01, gp = grid::gpar(fontsize = 6, col = "gray50"))
      
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)
