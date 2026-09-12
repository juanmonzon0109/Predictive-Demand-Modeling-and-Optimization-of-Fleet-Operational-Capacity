# ==============================================================================
# TFM - WEEKLY PLANT-LEVEL FORECAST
# ==============================================================================
# Objective: forecast weekly concrete demand per plant (Monday-Sunday weeks,
# horizon h=1). The resulting forecasts feed the plant-level fleet optimization
# model. Plants: 510, 511, 512, 514, 515, 710.
#
# Structure:
#   0   Setup and libraries
#   1   Data loading
#   2   Cleaning (exact dedup, physical volume filter, valid plants)
#   3   Per-plant regime diagnostic and validated corrections -> daily series
#   4   Weekly aggregation (Mon-Sun) + complete-calendar scaffold + boundary trim
#   5   Data-quality screening: closure and special-project weeks
#   6   Feature engineering (XGBoost candidate)
#   7   Model comparison (naive / seasonal-naive / XGBoost / SVR / ARIMA / ARIMAX)
#   8   Plant 710 ARIMA (short-history plant)
#   9   Residual diagnostics, all six plants
#   10  Final production forecast (h=1) with 80% scenarios
#
# MODEL DECISION (Section 7): ARIMA (non-seasonal), one order per plant, is adopted
# as the single model for all six plants. XGBoost and SVR were evaluated as ML
# candidates but a fair rolling h=1 comparison showed a parsimonious ARIMA beats
# both (and the naive benchmarks) on MAE, RMSE and MAPE, with white-noise residuals
# on five of six plants (710 has a documented residual limitation).
# ==============================================================================


# --- 0. Setup -----------------------------------------------------------------
setwd("C:/Users/USER/OneDrive/Desktop/TFM_FinalVersion")

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(xgboost)
library(forecast)

fig_folder <- "Figures/Forecast Weekly"
if (!dir.exists(fig_folder)) dir.create(fig_folder, recursive = TRUE)


# --- 1. Data loading ----------------------------------------------------------
# Same transactional source as the monthly chapter. Every CSV in Data/ is read;
# the new 2026 JUL.csv is picked up automatically. Mixed date format
# (M/D/YYYY H:MM vs YYYY-MM-DD HH:MM:SS) is parsed row-wise.
csv_files <- list.files("Data", pattern = "\\.csv$", full.names = TRUE)
df <- do.call(rbind, lapply(csv_files, read.csv))

df$order_date <- as.Date(ifelse(
  grepl("^\\d{1,2}/", df$order_date),
  format(as.Date(df$order_date, format = "%m/%d/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(df$order_date), "%Y-%m-%d")
))

cat("CSV files:", length(csv_files),
    "| Total rows:", nrow(df),
    "| NA dates:", sum(is.na(df$order_date)),
    "| Range:", format(min(df$order_date)), "->", format(max(df$order_date)), "\n")
# RESULT: 27 files, 589,066 rows, 0 parse failures, 2020-01-02 -> 2026-07-31.
# July 2026 present with ~8,984 rows, in line with prior months.


# --- 2. Cleaning (validated in the EDA chapter) -------------------------------
# Three decisions carried over from the monthly/EDA chapter:
#   distinct()          -> removes true exact duplicates
#   0 < u_Volumen <= 7  -> physical mixer capacity; also removes data-entry
#                          spikes (e.g. a single ticket logged at 6500 m3)
#   valid_plants        -> business-valid plants only (708 excluded)
valid_plants <- c(510, 511, 512, 514, 515, 710)
rows_raw <- nrow(df)

clean_data <- df %>%
  distinct() %>%
  filter(u_Volumen > 0, u_Volumen <= 7) %>%
  filter(ship_plant_code %in% valid_plants)

cat("Rows kept:", nrow(clean_data),
    sprintf("(%.2f%%) | removed: %d (%.2f%%)\n",
            100 * nrow(clean_data) / rows_raw,
            rows_raw - nrow(clean_data),
            100 * (rows_raw - nrow(clean_data)) / rows_raw))
# RESULT: 566,134 rows kept (96.11%). Of the 3.89% removed: ~89% are zero/negative
# volumes (empty or cancelled tickets), ~10% are volumes >7 m3 (physically
# impossible, data-entry errors, max 6500), ~0.5% are plant 708. No exact
# duplicates remain at this granularity. Removal is dominated by non-demand rows.


# --- 3. Per-plant regime diagnostic and validated corrections -----------------
# Before applying any date correction, inspect all six plants raw, so the data
# (not prior assumptions) justifies each cut.

daily_full <- clean_data %>%
  group_by(plant = ship_plant_code, date = order_date) %>%
  summarise(volume = sum(u_Volumen), .groups = "drop")

monthly_plant <- daily_full %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(plant, month) %>%
  summarise(volume = sum(volume), .groups = "drop")

p_diag <- ggplot(monthly_plant, aes(month, volume)) +
  geom_line(color = "steelblue") + geom_point(size = 0.6, color = "steelblue") +
  facet_wrap(~ plant, scales = "free_y", ncol = 2) +
  labs(title = "Monthly volume per plant - RAW (no corrections)",
       x = NULL, y = "Volume (m3)") +
  theme_minimal()
print(p_diag)
ggsave(file.path(fig_folder, "diag_monthly_per_plant_raw.png"),
       p_diag, width = 11, height = 7, dpi = 150)

gap_report <- monthly_plant %>%
  group_by(plant) %>%
  group_modify(~{
    full_seq <- seq(min(.x$month), max(.x$month), by = "month")
    missing  <- full_seq[!full_seq %in% .x$month]
    tibble(n_months_missing = length(missing),
           missing_range = if (length(missing) == 0) "-" else
             paste(format(min(missing)), "->", format(max(missing))))
  }) %>% ungroup()
print(gap_report)
# CONCLUSION:
#   510/511/512/515 -> full clean history 2020-2026, no monthly gaps.
#   514             -> no data before 2022-07-20; the plant enters operation then
#                      (there is NO 2020-2022 ramp-up to exclude).
#   710             -> 6 missing months; production shutdown (see 3c).

monthly_plant %>%
  filter(plant == 710, month >= as.Date("2023-01-01"),
         month <= as.Date("2024-12-31")) %>%
  arrange(month) %>% mutate(volume = round(volume)) %>% print(n = 24)
# RESULT: pre-shutdown median ~3,756 m3. Decline starts May 2023 (1,117), near-total
# shutdown Jun 2023-Jan 2024 (Jun/Jul/Aug and Oct/Nov/Dec missing; only 64 m3 in
# Sep 2023, i.e. NOT one continuous block), gradual restart Feb-Apr 2024, back to
# ~normal from May 2024 (3,186 ~= 85% of median).
# DECISION: model 710 from its restart (2024-05-01). Truncating the start keeps the
# series continuous (required for lag-based models), unlike excising a mid-series
# window. Criterion identical to 514: each plant is modelled from the start of its
# current stable operating regime.

plant_710_restart <- as.Date("2024-05-01")
weeks_preview <- daily_full %>%
  filter(!(plant == 710 & date < plant_710_restart)) %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(plant) %>%
  summarise(n_weeks = n_distinct(week),
            approx_yrs = round(n_distinct(week) / 52, 1), .groups = "drop")
print(weeks_preview)
# RESULT: 510/511/515 ~344 wk (6.6 yr), 512 ~343, 514 ~211 (4.1 yr), 710 ~118 (2.3 yr).
# Initial plan: >=3 annual cycles -> XGBoost candidate (510,511,512,514,515);
# 710 (2.3 cycles) -> statistical model. NOTE: Section 7 later overrides this,
# adopting ARIMA for all plants after a head-to-head comparison.

daily_series <- daily_full %>%
  filter(!(plant == 710 & date < plant_710_restart))


# --- 4. Weekly aggregation (Mon-Sun) + scaffold + boundary trim ---------------
# (a) daily -> weekly (Monday start => Mon-Sun weeks)
# (b) per-plant complete weekly calendar + left-join, so weeks with no deliveries
#     become explicit volume = 0 rows (needed for the section-5 closure screening)
# active_days = distinct delivery days in the week; separates real closures from
# partial boundary weeks.

weekly_raw <- daily_series %>%
  mutate(week = floor_date(date, unit = "week", week_start = 1)) %>%
  group_by(plant, week) %>%
  summarise(volume = sum(volume), active_days = n_distinct(date), .groups = "drop")

scaffold <- weekly_raw %>%
  group_by(plant) %>%
  summarise(first_week = min(week), last_week = max(week), .groups = "drop") %>%
  rowwise() %>%
  mutate(week = list(seq(first_week, last_week, by = "week"))) %>%
  tidyr::unnest(week) %>% select(plant, week)

weekly_series <- scaffold %>%
  left_join(weekly_raw, by = c("plant", "week")) %>%
  mutate(volume = tidyr::replace_na(volume, 0),
         active_days = tidyr::replace_na(active_days, 0)) %>%
  arrange(plant, week)

weekly_series %>%
  group_by(plant) %>%
  summarise(n_weeks = n(),
            expected = as.integer(difftime(max(week), min(week), units = "weeks")) + 1,
            zero_weeks = sum(volume == 0), .groups = "drop") %>%
  mutate(gapless = n_weeks == expected) %>% print()
# RESULT: gapless = TRUE for all six plants. Only one exact-zero week overall
# (512, 2020-04-13), confirmed as a real COVID closure (0 tickets in raw data),
# not a scaffold artefact. 710 has 0 zero-weeks post-restart -> clean continuous
# operation, validating the 2024-05 truncation.

weekly_series %>%
  filter(plant %in% c(510, 511, 512, 515),
         week >= as.Date("2020-03-16"), week <= as.Date("2020-05-11")) %>%
  mutate(volume = round(volume)) %>%
  tidyr::pivot_wider(id_cols = week, names_from = plant, values_from = volume) %>%
  arrange(week) %>% print()
# CONCLUSION: COVID is UNEVEN across plants. 512 collapses hard (6+ weeks <100 m3)
# and 515 partially (3 weeks deep); 510/511 barely dip (one week ~486/366, still
# real demand). No hardcoded COVID window is used (consistent with the monthly
# model). The deep weeks are caught by the section-5 closure flag; low-but-not-
# closed weeks are kept as genuine demand.

data_min <- min(daily_series$date)
data_max <- max(daily_series$date)

weekly_series <- weekly_series %>%
  filter(week >= data_min, week + 5 <= data_max)   # keep weeks whose Saturday is covered

weekly_series %>%
  group_by(plant) %>%
  summarise(first_week = min(week), last_week = max(week), n_weeks = n(),
            .groups = "drop") %>% print()
# RESULT: last_week = 2026-07-20 for all plants (the week of 2026-07-27 is dropped
# as it misses its Saturday). The final production forecast (Section 10) therefore
# targets the week of 2026-07-27, the first fully unobserved week.


# --- 5. Data-quality screening: closure and special-project weeks -------------
# Two operational flags, applied uniformly to all six plants:
#   es_cierre_planta     -> (near) no production: volume < 100 m3. Captures
#                           closures and COVID-collapse weeks (no hardcoded date).
#   es_proyecto_especial -> one-off upward spike: residual vs an 8-week centered
#                           rolling-median baseline exceeds a per-plant Tukey
#                           "far out" cut-off (Q3 + 3*IQR). Baseline uses OPEN
#                           weeks only, so a shutdown cannot drag it down.
# Low-but-not-closed weeks are deliberately NOT flagged: they are genuine demand.
CLOSURE_THRESHOLD <- 100
ROLL_WIDTH        <- 8
IQR_MULT          <- 3

weekly_flags <- weekly_series %>%
  arrange(plant, week) %>%
  group_by(plant) %>%
  mutate(
    es_cierre_planta = as.integer(volume < CLOSURE_THRESHOLD),
    vol_open = ifelse(es_cierre_planta == 1, NA_real_, volume),
    baseline = zoo::rollapply(vol_open, width = ROLL_WIDTH,
                              FUN = function(v) median(v, na.rm = TRUE),
                              align = "center", partial = TRUE),
    residual = volume - baseline
  ) %>%
  mutate(
    thr = quantile(residual[es_cierre_planta == 0], 0.75, na.rm = TRUE) +
      IQR_MULT * IQR(residual[es_cierre_planta == 0], na.rm = TRUE),
    es_proyecto_especial = as.integer(es_cierre_planta == 0 & residual > thr)
  ) %>%
  ungroup() %>%
  select(plant, week, volume, active_days, es_cierre_planta, es_proyecto_especial)

weekly_flags %>%
  group_by(plant) %>%
  summarise(n_weeks = n(), n_cierre = sum(es_cierre_planta),
            n_especial = sum(es_proyecto_especial), .groups = "drop") %>% print()
# RESULT: closures only in 512 (10, COVID Apr-Jun 2020) and 515 (4); 510/511/514/710
# have 0. Special-project weeks total 18 (~1%). KEY VALIDATION: the Dec 2024 special
# project (worth ~15,375 m3 in the monthly model) is flagged the same week
# (2024-12-16) across 510, 511, 512 and 514 -> the generic Tukey rule detects the
# real, coordinated event with no hardcoded date.

p_screen <- ggplot(weekly_flags, aes(week, volume)) +
  geom_line(color = "grey55", linewidth = 0.3) +
  geom_point(data = subset(weekly_flags, es_cierre_planta == 1),
             aes(color = "Closure"), size = 1.1) +
  geom_point(data = subset(weekly_flags, es_proyecto_especial == 1),
             aes(color = "Special project"), size = 1.1) +
  facet_wrap(~ plant, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Closure" = "red", "Special project" = "forestgreen"),
                     name = NULL) +
  labs(title = "Weekly screening: closure and special-project weeks",
       x = NULL, y = "Volume (m3)") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_screen)
ggsave(file.path(fig_folder, "weekly_screening_flags.png"),
       p_screen, width = 11, height = 7, dpi = 150)


# ==============================================================================
# --- 5c. Exogenous variable: weighted working days (Mexican holidays) ---------
# ==============================================================================
# Calendar effect discovered at monthly level and confirmed here at weekly level.
# Day weighting: Mon-Fri = 1, Sat = 0.5, Sun = 0; official Mexican public holidays
# (art. 74 LFT) override their day to 0. A full normal week = 5.5; a weekday
# holiday reduces it (typically to 4.5). The variable is DETERMINISTIC and known
# in advance, so at forecast time the target week's real value is supplied
# (leakage-safe) -- unlike the special-project flag.
mx_holidays <- as.Date(c(
  "2020-01-01","2021-01-01","2022-01-01","2023-01-01","2024-01-01","2025-01-01","2026-01-01",
  "2020-02-03","2021-02-01","2022-02-07","2023-02-06","2024-02-05","2025-02-03","2026-02-02",  # Constitution
  "2020-03-16","2021-03-15","2022-03-21","2023-03-20","2024-03-18","2025-03-17","2026-03-16",  # Juarez
  "2020-05-01","2021-05-01","2022-05-01","2023-05-01","2024-05-01","2025-05-01","2026-05-01",  # Labour
  "2020-09-16","2021-09-16","2022-09-16","2023-09-16","2024-09-16","2025-09-16","2026-09-16",  # Independence
  "2020-11-16","2021-11-15","2022-11-21","2023-11-20","2024-11-18","2025-11-17","2026-11-16",  # Revolution
  "2024-10-01",                                                                                   # power handover
  "2020-12-25","2021-12-25","2022-12-25","2023-12-25","2024-12-25","2025-12-25","2026-12-25"))   # Christmas

day_weight <- function(d) {
  wd <- as.integer(format(d, "%u"))                 # 1=Mon ... 7=Sun
  w  <- ifelse(wd <= 5, 1, ifelse(wd == 6, 0.5, 0))
  w[d %in% mx_holidays] <- 0
  w
}
week_working_days <- function(monday)
  sapply(monday, function(m) sum(day_weight(seq(m, m + 6, by = "day"))))

weekly_wd <- weekly_series %>% mutate(wdays = week_working_days(week))

cat("\nWeighted working days per week -- distribution:\n")
print(table(round(weekly_wd$wdays, 1)))
wd_open <- weekly_wd %>%
  left_join(weekly_flags %>% select(plant, week, es_cierre_planta, es_proyecto_especial),
            by = c("plant", "week")) %>%
  filter(es_cierre_planta == 0, es_proyecto_especial == 0)
cat("Mean weekly volume by working-day level (open non-special weeks):\n")
wd_open %>% group_by(wdays = round(wdays, 1)) %>%
  summarise(mean_vol = round(mean(volume)), n = n(), .groups = "drop") %>% print()
# RESULT: 87.3% of weeks are full (5.5); 12.7% have a weekday holiday (4.5 or 5).
# Mean volume: 5.5-day weeks 1,118 m3 vs 4.5-day weeks 900 m3 (~19% drop) -> plants
# do NOT fully recover holiday production; the calendar genuinely lowers weekly
# volume. Per-plant correlation with volume is positive (0.19-0.29).


# ==============================================================================
# --- 6. Feature engineering (XGBoost candidate: 510, 511, 512, 514, 515) ------
# ==============================================================================
# Target: weekly volume at week t (h=1). Leakage discipline: every feature for
# target week t uses ONLY information up to t-1 (lags on lag(volume,1) or deeper).
# Calendar features are the only "present" inputs and are legitimate (the target
# week's date is known in advance). Contemporaneous flags are NOT used (a future
# closure / special project is unknowable at forecast time); only their 1-week
# lags enter, as past facts.
xgb_plants <- c(510, 511, 512, 514, 515)

features <- weekly_flags %>%
  filter(plant %in% xgb_plants) %>%
  arrange(plant, week) %>%
  group_by(plant) %>%
  mutate(
    lag_1  = lag(volume, 1), lag_2 = lag(volume, 2),
    lag_3  = lag(volume, 3), lag_4 = lag(volume, 4),
    lag_52 = lag(volume, 52),                       # same week, previous year
    roll_mean_4 = zoo::rollapplyr(lag(volume, 1), 4, mean, fill = NA, partial = FALSE),
    roll_mean_8 = zoo::rollapplyr(lag(volume, 1), 8, mean, fill = NA, partial = FALSE),
    roll_sd_4   = zoo::rollapplyr(lag(volume, 1), 4, sd,   fill = NA, partial = FALSE),
    es_cierre_lag_1   = lag(es_cierre_planta, 1),
    es_proyecto_lag_1 = lag(es_proyecto_especial, 1),
    week_of_year = as.integer(lubridate::isoweek(week)),
    woy_sin = sin(2 * pi * week_of_year / 52),
    woy_cos = cos(2 * pi * week_of_year / 52),
    month   = as.integer(lubridate::month(week)),
    year    = as.integer(lubridate::year(week))
  ) %>%
  ungroup()

feature_cols <- c("lag_1","lag_2","lag_3","lag_4","lag_52",
                  "roll_mean_4","roll_mean_8","roll_sd_4",
                  "es_cierre_lag_1","es_proyecto_lag_1",
                  "week_of_year","woy_sin","woy_cos","month","year")

usable <- features %>% filter(if_all(all_of(feature_cols), ~ !is.na(.)))
# RESULT: usable rows after the 52-week lag_52 warm-up: 288 for 510/511/512/515,
# 155 for 514. Leakage spot-check confirmed lag_1 at row t == volume at row t-1.


# ==============================================================================
# --- 7. Model comparison (rolling h=1) ----------------------------------------
# ==============================================================================
# Temporal (never random) split: test = each plant's last 26 weeks. Rolling h=1:
# at each origin the model is retrained on all data up to that week and predicts
# the next one. Candidates evaluated on the SAME scheme: naive (lag_1), seasonal
# naive (lag_52), XGBoost, SVR, ARIMA, and an ARIMAX sensitivity variant.
# A model must beat the naive benchmarks to justify its use.
#
# OUTCOME (see 7f): ARIMA (non-seasonal, one order per plant) wins on all three
# metrics and is adopted as the single model for all six plants. XGBoost and SVR
# (two ML families) are retained as the rigorously-evaluated candidates it beat.
TEST_WEEKS <- 26
VAL_FRAC   <- 0.15

metric_block <- function(df, pred_col) {
  a <- df$actual; f <- df[[pred_col]]
  data.frame(MAE  = round(mean(abs(a - f)), 0),
             RMSE = round(sqrt(mean((a - f)^2)), 0),
             MAPE = round(mean(abs((a - f) / a)) * 100, 2))
}
mape <- function(a, f) round(mean(abs((a - f) / a)) * 100, 2)
mae  <- function(a, f) round(mean(abs(a - f)), 0)


# --- 7a. XGBoost (candidate) + naive / seasonal-naive benchmarks --------------
# Conservative XGBoost (shallow, low eta, regularized) to be defensible, not
# overfit. Early stopping uses a temporal validation holdout (last 15% of the
# current train window), never random -> no leakage.
xgb_params <- list(objective = "reg:squarederror", eval_metric = "rmse",
                   max_depth = 3, eta = 0.05, subsample = 0.8,
                   colsample_bytree = 0.8, min_child_weight = 3)

run_plant_xgb <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat)
  X <- as.matrix(dat[, feature_cols]); y <- dat$volume
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    val_start <- floor(train_end * (1 - VAL_FRAC)) + 1
    dfit <- xgb.DMatrix(X[1:(val_start - 1), , drop = FALSE], label = y[1:(val_start - 1)])
    dval <- xgb.DMatrix(X[val_start:train_end, , drop = FALSE], label = y[val_start:train_end])
    m <- xgb.train(params = xgb_params, data = dfit, nrounds = 500,
                   watchlist = list(val = dval), early_stopping_rounds = 20, verbose = 0)
    pred <- predict(m, xgb.DMatrix(X[target, , drop = FALSE]))
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 actual = y[target], xgb = as.numeric(pred),
                                 naive = dat$lag_1[target], snaive = dat$lag_52[target]))
  }
  out
}

set.seed(123)
roll_results <- do.call(rbind, lapply(split(usable, usable$plant), run_plant_xgb))
# RESULT: XGBoost MAPE 17.0% vs naive 18.0% vs seasonal naive 23.2%. XGBoost only
# marginally beats naive and loses/ties on 512 and 515 -> weak, inconsistent edge.
# Seasonal naive is worst everywhere -> annual seasonality is weak in this data.

p_fc <- ggplot(roll_results, aes(week)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.6) +
  geom_line(aes(y = xgb, color = "XGBoost"), linewidth = 0.6, linetype = "dashed") +
  facet_wrap(~ plant, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "XGBoost" = "blue"), name = NULL) +
  labs(title = "Rolling h=1 forecast vs actual (last 26 weeks)", x = NULL, y = "Volume (m3)") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_fc)
ggsave(file.path(fig_folder, "xgb_rolling_forecast.png"), p_fc, width = 11, height = 7, dpi = 150)


# --- 7b. XGBoost tuning attempt: drop lag_52, max_depth = 4 (documented) ------
# lag_52 was the weakest predictor; dropping it also recovers ~44 training weeks.
# Kept as documented exploration of whether XGBoost can be rescued.
feature_cols_v2 <- setdiff(feature_cols, "lag_52")
usable_v2 <- features %>% filter(if_all(all_of(feature_cols_v2), ~ !is.na(.)))
xgb_params_v2 <- modifyList(xgb_params, list(max_depth = 4))

run_plant_xgb_v2 <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat)
  X <- as.matrix(dat[, feature_cols_v2]); y <- dat$volume
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    val_start <- floor(train_end * (1 - VAL_FRAC)) + 1
    dfit <- xgb.DMatrix(X[1:(val_start - 1), , drop = FALSE], label = y[1:(val_start - 1)])
    dval <- xgb.DMatrix(X[val_start:train_end, , drop = FALSE], label = y[val_start:train_end])
    m <- xgb.train(params = xgb_params_v2, data = dfit, nrounds = 500,
                   watchlist = list(val = dval), early_stopping_rounds = 20, verbose = 0)
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 actual = y[target],
                                 xgb = as.numeric(predict(m, xgb.DMatrix(X[target, , drop = FALSE])))))
  }
  out
}

set.seed(123)
roll_v2 <- do.call(rbind, lapply(split(usable_v2, usable_v2$plant), run_plant_xgb_v2))
cat("\nXGBoost v2 (no lag_52, depth 4) overall MAPE:",
    round(mean(abs((roll_v2$actual - roll_v2$xgb)/roll_v2$actual))*100, 2), "%\n")
# CONCLUSION: v2 = 16.6% overall, essentially unchanged. Improves 512 but WORSENS
# 510/514/515 -> different, not better; higher variance, not a robust gain.


# --- 7b-bis. XGBoost hyperparameter tuning (temporal validation, no leakage) --
# Addresses the critique that XGBoost used fixed hyperparameters. Tuning is done
# on a VALIDATION block (26 weeks BEFORE the test), never on the test itself, so
# the test comparison against ARIMA stays fair (ARIMA also fixed its order on
# pre-test data). Small, defensible grid (12 combos, not exhaustive -> avoids
# p-hacking). Best combo per plant is chosen by validation MAPE, then evaluated
# ONCE on the test block. Same feature set as 7a to isolate the tuning effect.
VAL_WEEKS <- 26
xgb_grid <- expand.grid(max_depth = c(3, 5, 7), eta = c(0.05, 0.10),
                        min_child_weight = c(1, 3))

val_mape_for_combo <- function(dat, params) {
  dat <- dat %>% arrange(week); n <- nrow(dat)
  X <- as.matrix(dat[, feature_cols]); y <- dat$volume
  val_end <- n - TEST_WEEKS; val_start <- val_end - VAL_WEEKS + 1
  errs <- c()
  for (t in val_start:val_end) {
    tr_end <- t - 1
    iv <- floor(tr_end * (1 - VAL_FRAC)) + 1
    dfit <- xgb.DMatrix(X[1:(iv - 1), , drop = FALSE], label = y[1:(iv - 1)])
    dval <- xgb.DMatrix(X[iv:tr_end, , drop = FALSE], label = y[iv:tr_end])
    m <- xgb.train(params = c(params, objective = "reg:squarederror", eval_metric = "rmse",
                              subsample = 0.8, colsample_bytree = 0.8),
                   data = dfit, nrounds = 500, watchlist = list(val = dval),
                   early_stopping_rounds = 20, verbose = 0)
    errs <- c(errs, abs((y[t] - predict(m, xgb.DMatrix(X[t, , drop = FALSE]))) / y[t]))
  }
  mean(errs) * 100
}

set.seed(123)
best_params <- list()
for (pl in xgb_plants) {
  dat <- usable %>% filter(plant == pl)
  gr  <- sapply(1:nrow(xgb_grid), function(k) val_mape_for_combo(dat, as.list(xgb_grid[k, ])))
  best_params[[as.character(pl)]] <- as.list(xgb_grid[which.min(gr), ])
}

run_plant_xgb_tuned <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat)
  X <- as.matrix(dat[, feature_cols]); y <- dat$volume
  pr <- c(best_params[[as.character(dat$plant[1])]],
          objective = "reg:squarederror", eval_metric = "rmse",
          subsample = 0.8, colsample_bytree = 0.8)
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    val_start <- floor(train_end * (1 - VAL_FRAC)) + 1
    dfit <- xgb.DMatrix(X[1:(val_start - 1), , drop = FALSE], label = y[1:(val_start - 1)])
    dval <- xgb.DMatrix(X[val_start:train_end, , drop = FALSE], label = y[val_start:train_end])
    m <- xgb.train(params = pr, data = dfit, nrounds = 500,
                   watchlist = list(val = dval), early_stopping_rounds = 20, verbose = 0)
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 xgb_tuned = as.numeric(predict(m, xgb.DMatrix(X[target, , drop = FALSE])))))
  }
  out
}
set.seed(123)
xgb_tuned_results <- do.call(rbind, lapply(split(usable, usable$plant), run_plant_xgb_tuned))
tuned_chk <- roll_results %>% left_join(xgb_tuned_results, by = c("plant", "week"))
cat("\nXGBoost TUNED overall MAPE:", mape(tuned_chk$actual, tuned_chk$xgb_tuned), "%\n")
cat("Best params per plant:\n"); str(best_params)
# RESULT: tuning barely moves XGBoost -> 17.05% vs 17.17% default (0.12 pp), still
# ~2.1 pp above ARIMA (14.91%) and losing on all five plants. The search chose the
# conservative depth 3 on 4 of 5 plants (only 510 -> depth 7, and still 18.0% in
# test). CONCLUSION: ARIMA's edge is STRUCTURAL (short-run autoregressive signal at
# h=1), not an artefact of untuned hyperparameters. Validation-block tuning keeps
# the test set clean and the comparison with ARIMA fair. (See 7f for the merge.)


# --- 7c. SVR (second ML candidate, RBF kernel) --------------------------------
# SVR is robust on small datasets. lag_52 dropped to recover history. scale=TRUE
# is essential: the RBF kernel uses Euclidean distance, and features live on very
# different scales (volume ~1000s vs woy_sin in [-1,1]); svm() standardizes them.
library(e1071)
feature_cols_svr <- setdiff(feature_cols, "lag_52")
usable_svr <- features %>% filter(if_all(all_of(feature_cols_svr), ~ !is.na(.)))

run_plant_svr <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat)
  X <- as.matrix(dat[, feature_cols_svr]); y <- dat$volume
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    m_svr <- svm(x = X[1:train_end, , drop = FALSE], y = y[1:train_end],
                 type = "eps-regression", kernel = "radial",
                 scale = TRUE, cost = 1, epsilon = 0.1)
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 svr = as.numeric(predict(m_svr, X[target, , drop = FALSE]))))
  }
  out
}

set.seed(123)
svr_results <- do.call(rbind, lapply(split(usable_svr, usable_svr$plant), run_plant_svr))
# NOTE: usable_svr has ~46 more weeks/plant than `usable` (no lag_52 warm-up); the
# left_join below aligns only the shared 26 test weeks, so the comparison is fair.
# RESULT: SVR MAPE 16.8% / MAE 196 -> also loses to ARIMA. A SECOND ML family
# (kernels, after trees) is beaten by ARIMA, reinforcing the conclusion.


# --- 7d. ARIMA (non-seasonal) benchmark ---------------------------------------
# Non-seasonal on purpose: SARIMA freq=52 is unstable with ~2-3 annual cycles, and
# lag_52 already proved weak -> all models compete on short-run dynamics (what
# matters at h=1). Structure FIXED per plant: auto.arima runs ONCE on the pre-test
# window (no leakage), the order is frozen, only coefficients re-estimated in the
# loop (symmetric with XGBoost's fixed structure). ARIMA is univariate.
arima_orders <- list()
run_plant_arima <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat); y <- dat$volume
  fit0 <- auto.arima(ts(y[1:(n - TEST_WEEKS)], frequency = 1),
                     seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  ord <- arimaorder(fit0)
  arima_orders[[as.character(dat$plant[1])]] <<- ord
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    fit <- Arima(y[1:train_end], order = as.numeric(ord[c("p","d","q")]))
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 arima = as.numeric(forecast(fit, h = 1)$mean[1])))
  }
  out
}
arima_results <- do.call(rbind, lapply(split(usable, usable$plant), run_plant_arima))

compare <- roll_results %>%
  left_join(arima_results,      by = c("plant", "week")) %>%
  left_join(svr_results,        by = c("plant", "week")) %>%
  left_join(xgb_tuned_results,  by = c("plant", "week"))


# --- 7e. ARIMAX sensitivity variant: intervention regressor for flags ---------
# Responds to the heavy-tail concern: past special-project spikes distort AR/MA
# estimation. Instead of deleting real events (tsclean would contradict the
# monthly chapter's Dec-2024 decision), model them with an intervention regressor
# -> same approach as the monthly dummy. Flags already live in `usable`; each is
# included only if non-constant for that plant (es_cierre is all-zero here after
# the lag_52 warm-up, so only es_proyecto_especial enters). At the forecast point
# the regressor is 0 (normal-operation scenario).
usable_x <- usable
run_plant_arimax <- function(dat) {
  dat <- dat %>% arrange(week); n <- nrow(dat); y <- dat$volume
  xcols <- c()
  if (sum(dat$es_proyecto_especial) > 0) xcols <- c(xcols, "es_proyecto_especial")
  if (sum(dat$es_cierre_planta)     > 0) xcols <- c(xcols, "es_cierre_planta")
  X <- if (length(xcols)) as.matrix(dat[, xcols, drop = FALSE]) else NULL
  ptr <- 1:(n - TEST_WEEKS)
  fit0 <- auto.arima(ts(y[ptr], frequency = 1),
                     xreg = if (!is.null(X)) X[ptr, , drop = FALSE] else NULL,
                     seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
  ord <- arimaorder(fit0)
  out <- data.frame()
  for (i in TEST_WEEKS:1) {
    train_end <- n - i; target <- train_end + 1
    if (!is.null(X)) {
      fit  <- Arima(y[1:train_end], order = as.numeric(ord[c("p","d","q")]),
                    xreg = X[1:train_end, , drop = FALSE], method = "ML")
      newx <- matrix(0, nrow = 1, ncol = length(xcols), dimnames = list(NULL, xcols))
      fc   <- forecast(fit, h = 1, xreg = newx)
    } else {
      fit <- Arima(y[1:train_end], order = as.numeric(ord[c("p","d","q")]), method = "ML")
      fc  <- forecast(fit, h = 1)
    }
    out <- rbind(out, data.frame(plant = dat$plant[target], week = dat$week[target],
                                 arimax = as.numeric(fc$mean[1])))
  }
  out
}
arimax_results <- do.call(rbind, lapply(split(usable_x, usable_x$plant), run_plant_arimax))
compare <- compare %>% left_join(arimax_results, by = c("plant", "week"))
# RESULT: ARIMAX reduces in-sample residual tails (|max|/sd ~4.5 -> ~3.5) but does
# NOT improve out-of-sample: it ties RMSE and worsens MAE/MAPE (14.9 -> 15.3),
# because the added regressor induces more complex orders that generalize worse.
# CONCLUSION: univariate ARIMA kept. Lesson: better in-sample fit != better
# out-of-sample prediction. ARIMAX documented as a sensitivity analysis.


# --- 7f. Full metric comparison (MAE, RMSE, MAPE) — same as monthly chapter ---
cat("\n===== Overall metrics, all candidates (rolling h=1, five long plants) =====\n")
rbind(
  cbind(Model = "Naive (t-1)",      metric_block(compare, "naive")),
  cbind(Model = "SeasonalN (t-52)", metric_block(compare, "snaive")),
  cbind(Model = "XGBoost",          metric_block(compare, "xgb")),
  cbind(Model = "XGBoost (tuned)",  metric_block(compare, "xgb_tuned")),
  cbind(Model = "SVR",              metric_block(compare, "svr")),
  cbind(Model = "ARIMA",            metric_block(compare, "arima")),
  cbind(Model = "ARIMAX",           metric_block(compare, "arimax"))
) %>% print(row.names = FALSE)

cat("\n===== Per-plant MAPE: ARIMA vs the rest =====\n")
compare %>% group_by(plant) %>%
  summarise(Naive = mape(actual, naive), XGBoost = mape(actual, xgb),
            XGB_tuned = mape(actual, xgb_tuned),
            SVR = mape(actual, svr), ARIMA = mape(actual, arima),
            .groups = "drop") %>% print(row.names = FALSE)

cat("\nSelected ARIMA order per plant:\n")
for (p in names(arima_orders)) {
  o <- arima_orders[[p]]; cat(sprintf("  Plant %s: ARIMA(%d,%d,%d)\n", p, o["p"], o["d"], o["q"]))
}
# RESULT (overall): ARIMA best on all three metrics -> MAE 175, RMSE 236, MAPE 14.9%.
# Beats naive (206/271/18.1), seasonal naive (266/339/23.1), XGBoost (200/258/17.2),
# XGBoost tuned (198/-/17.1), SVR (196/256/16.8) and ARIMAX (179/237/15.4).
# ARIMA wins per-plant in all five. Orders: 510(1,1,1) 511(2,0,0) 512(0,1,3)
# 514(0,1,3) 515(1,0,1).
# CONCLUSION: at h=1 with near-stationary series, a parsimonious ARIMA capturing
# short-run autocorrelation beats two ML families -- and XGBoost stays behind even
# after validation-block hyperparameter tuning (7b-bis). ARIMA's edge is structural,
# not a tuning artefact. -> ARIMA is the single model.

p_arima_fc <- ggplot(compare, aes(week)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 0.6) +
  geom_line(aes(y = arima, color = "ARIMA"), linewidth = 0.6, linetype = "dashed") +
  facet_wrap(~ plant, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Actual" = "black", "ARIMA" = "darkred"), name = NULL) +
  labs(title = "Rolling h=1: Actual vs ARIMA (last 26 weeks)", x = NULL, y = "Volume (m3)") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_arima_fc)
ggsave(file.path(fig_folder, "arima_rolling_forecast.png"), p_arima_fc, width = 11, height = 7, dpi = 150)


# ==============================================================================
# --- 8. Plant 710 ARIMA (short-history plant, same framework) -----------------
# ==============================================================================
# 710 has only ~114 weeks (post-restart). Evaluated with ARIMA (no ML: too few
# observations for a tree/kernel model with annual seasonality).
y_710 <- weekly_series %>% filter(plant == 710) %>% arrange(week) %>% pull(volume)
wk_710 <- weekly_series %>% filter(plant == 710) %>% arrange(week) %>% pull(week)
n_710 <- length(y_710)

fit0_710 <- auto.arima(ts(y_710[1:(n_710 - TEST_WEEKS)], frequency = 1),
                       seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
ord_710 <- arimaorder(fit0_710)

out_710 <- data.frame()
for (i in TEST_WEEKS:1) {
  train_end <- n_710 - i; target <- train_end + 1
  fit <- Arima(y_710[1:train_end], order = as.numeric(ord_710[c("p","d","q")]))
  out_710 <- rbind(out_710, data.frame(
    week = wk_710[target], actual = y_710[target],
    arima = as.numeric(forecast(fit, h = 1)$mean[1]), naive = y_710[target - 1]))
}
cat(sprintf("\nPlant 710: ARIMA(%d,%d,%d) | MAPE ARIMA %.2f%% vs Naive %.2f%%\n",
            ord_710["p"], ord_710["d"], ord_710["q"],
            mape(out_710$actual, out_710$arima), mape(out_710$actual, out_710$naive)))
arima_orders[["710"]] <- ord_710   # add to master list for the final forecast
# RESULT: ARIMA(1,1,1), MAPE 18.1% vs naive 20.9%. ARIMA beats naive, but 710 is
# the least accurate plant (shortest history, recent restart). See 9 for its
# residual limitation.


# ==============================================================================
# --- 9. Residual diagnostics, all six plants (compact) ------------------------
# ==============================================================================
# checkresiduals() gives, in one call per plant: residual-vs-time, residual ACF,
# histogram + normal curve, and Ljung-Box. Two complements are kept because they
# proved decisive and checkresiduals() does not provide them:
#   (i)  residual PACF -> revealed the spikes in 515 and, critically, in 710;
#   (ii) multi-lag Ljung-Box (8..24) -> 710 PASSES at lag 10 (p=0.392) but is
#        REJECTED at lags 20/24 (p=0.038/0.018); a single default lag hides it.
get_series <- function(p) {
  if (p == "710") weekly_series %>% filter(plant == 710) %>% arrange(week) %>% pull(volume)
  else            usable %>% filter(plant == as.integer(p)) %>% arrange(week) %>% pull(volume)
}

for (p in names(arima_orders)) {
  ord <- as.numeric(arima_orders[[p]][c("p","d","q")])
  fit <- Arima(get_series(p), order = ord)
  cat(sprintf("\n----- Plant %s: ARIMA(%d,%d,%d) -----\n", p, ord[1], ord[2], ord[3]))
  print(checkresiduals(fit))
  rec <- recordPlot()
  png(file.path(fig_folder, paste0("arima_checkresiduals_", p, ".png")),
      width = 1000, height = 700, res = 130); replayPlot(rec); dev.off()
}

op <- par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
for (p in names(arima_orders)) {
  ord <- as.numeric(arima_orders[[p]][c("p","d","q")])
  pacf(residuals(Arima(get_series(p), order = ord)),
       main = paste("Plant", p, "resid PACF"), lag.max = 24)
}
par(op); rec <- recordPlot()
png(file.path(fig_folder, "arima_residual_pacf.png"), width = 1100, height = 650, res = 130)
replayPlot(rec); dev.off()

wn_check <- data.frame()
for (p in names(arima_orders)) {
  ord <- as.numeric(arima_orders[[p]][c("p","d","q")])
  r <- residuals(Arima(get_series(p), order = ord))
  fd <- ord[1] + ord[3]; n <- length(r); band <- 1.96 / sqrt(n)
  pv <- sapply(c(8, 10, 12, 16, 20, 24), function(L)
    Box.test(r, lag = L, type = "Ljung-Box", fitdf = fd)$p.value)
  aa <- acf(r,  plot = FALSE, lag.max = 24)$acf[-1]
  pp <- pacf(r, plot = FALSE, lag.max = 24)$acf
  wn_check <- rbind(wn_check, data.frame(
    plant = p, order = sprintf("(%d,%d,%d)", ord[1], ord[2], ord[3]), n = n,
    min_p = round(min(pv), 3),
    n_acf_out = sum(abs(aa) > band), n_pacf_out = sum(abs(pp) > band),
    tail_ratio = round(max(abs(r)) / sd(r), 1),
    verdict = ifelse(min(pv) > 0.05, "WHITE NOISE", "REJECTED")))
}
cat("\n===== White-noise verification, lags 8-24, all six plants =====\n")
print(wn_check, row.names = FALSE)
# RESULT: 510/511/512/514/515 -> WHITE NOISE (min_p 0.22-0.71; 0-2 band crossings
#   of 24, consistent with multiplicity). Tail_ratio 3.8-4.8 -> slightly heavy tails
#   (special-project weeks) -> extreme prediction-interval bounds are a reference.
# 710 -> REJECTED (min_p 0.030), the weakest diagnostic of the six: still some
#   residual structure, but with the fuller post-restart series (117 wk) it is much
#   milder than before (tail_ratio 2.6, 1 acf / 3 pacf crossings). No alternative
#   order improves AICc, so it is not capturable ARIMA structure.
#   LIMITATION (univariate): 710 rejects white noise here (p=0.030). This is
#   RESOLVED in 9d by adding the working-days regressor.


# ==============================================================================
# --- 9d. Enrich the selected ARIMA with the working-days regressor ------------
# ==============================================================================
# ARIMA won the model selection (7). Here it is enriched with the deterministic
# working-days regressor (5c). The comparison is isolated: SAME validated order
# per plant, the only change being the presence of the xreg (so any gain is
# attributable to the calendar variable, not to order re-selection). method="ML"
# for numerical stability at the stationarity edge. Full series per plant.
val_orders <- list("510"=c(1,1,1), "511"=c(2,0,0), "512"=c(0,1,3),
                   "514"=c(0,1,3), "515"=c(1,0,1), "710"=c(1,1,1))

iso <- data.frame()
for (pl in names(val_orders)) {
  d <- weekly_wd %>% filter(plant == as.integer(pl)) %>% arrange(week)
  y <- d$volume; xd <- d$wdays; n <- nrow(d); ord <- val_orders[[pl]]
  for (i in TEST_WEEKS:1) {
    te <- n - i; tg <- te + 1
    pu <- as.numeric(forecast(Arima(y[1:te], order = ord, method = "ML"), h = 1)$mean[1])
    px <- as.numeric(forecast(Arima(y[1:te], order = ord, xreg = xd[1:te], method = "ML"),
                              h = 1, xreg = xd[tg])$mean[1])
    iso <- rbind(iso, data.frame(plant = as.integer(pl), actual = y[tg],
                                 arima = pu, arima_wd = px))
  }
}
cat("\n===== ARIMA vs ARIMA + working days (same validated order) =====\n")
cat("Five long plants:\n")
iso5 <- iso %>% filter(plant != 710)
rbind(cbind(Model = "ARIMA",         metric_block(iso5, "arima")),
      cbind(Model = "ARIMA + wdays",  metric_block(iso5, "arima_wd"))
) %>% print(row.names = FALSE)
cat("All six plants:\n")
rbind(cbind(Model = "ARIMA",         metric_block(iso, "arima")),
      cbind(Model = "ARIMA + wdays",  metric_block(iso, "arima_wd"))
) %>% print(row.names = FALSE)
# RESULT: working days improve every metric on all six plants (five-plant MAPE
# 15.1 -> 13.9). Gain is isolated from order re-selection. Coefficient is positive
# and significant on all plants (see below), and 710's residual structure --
# previously unexplained (lags 7-12-17-22) -- is resolved: the ~5-week pattern was
# largely the holiday calendar. -> the calendar regressor is ADOPTED.

cat("\n===== Working-days coefficient + residual check (full series) =====\n")
for (pl in names(val_orders)) {
  d <- weekly_wd %>% filter(plant == as.integer(pl)) %>% arrange(week)
  y <- d$volume; xd <- d$wdays; ord <- val_orders[[pl]]
  fit <- Arima(y, order = ord, xreg = xd, method = "ML")
  cn <- names(fit$coef); idx <- which(cn %in% c("xreg","xd"))
  b <- fit$coef[idx]; se <- sqrt(diag(fit$var.coef))[idx]
  r <- residuals(fit); fd <- ord[1] + ord[3]
  minp <- min(sapply(c(10,16,20,24), function(L)
    Box.test(r, lag = L, type = "Ljung-Box", fitdf = fd)$p.value))
  cat(sprintf("Plant %s: wdays coef=%.1f (t=%.2f) | resid min p(LB)=%.3f -> %s\n",
              pl, b, b/se, minp, ifelse(minp > 0.05, "WHITE NOISE", "REJECTED")))
}
# RESULT: all coefficients positive and significant (t 2.63-6.82; ~160-236 m3 per
# weighted working day). Residuals are white noise on all six plants -- including
# 710 (p=0.053, at the threshold: much improved over the univariate p=0.030,
# though close to the limit given its short series).


# ==============================================================================
# --- 10. Final production forecast (h=1) for all six plants -------------------
# ==============================================================================
# Final model: ARIMA + working-days regressor, validated order per plant, refit on
# the FULL series. Target = the week after the last observed week (2026-07-27). The
# regressor at the forecast point is the target week's OWN weighted working days
# (known in advance -> leakage-safe). Scenarios follow the monthly chapter: 80%
# prediction interval -> pessimistic (lower) / regular (point) / optimistic (upper).
LEVEL <- 80

final_forecast <- data.frame()
for (pl in names(val_orders)) {
  ord <- val_orders[[pl]]
  s   <- weekly_wd %>% filter(plant == as.integer(pl)) %>% arrange(week)
  nxt <- max(s$week) + 7
  nxt_wd <- week_working_days(nxt)
  fit <- Arima(s$volume, order = ord, xreg = s$wdays, method = "ML")
  fc  <- forecast(fit, h = 1, level = LEVEL, xreg = nxt_wd)
  final_forecast <- rbind(final_forecast, data.frame(
    plant         = as.integer(pl),
    order         = sprintf("(%d,%d,%d)", ord[1], ord[2], ord[3]),
    forecast_week = nxt,
    next_wdays    = nxt_wd,
    pessimistic   = round(as.numeric(fc$lower), 0),
    regular       = round(as.numeric(fc$mean),  0),
    optimistic    = round(as.numeric(fc$upper), 0)
  ))
}

cat("\n===== Final weekly production forecast (h=1, ARIMA + working days, 80% PI) =====\n")
print(final_forecast, row.names = FALSE)
cat(sprintf("\nCity-level totals next week -> regular: %d m3 | pessimistic: %d | optimistic: %d\n",
            sum(final_forecast$regular), sum(final_forecast$pessimistic),
            sum(final_forecast$optimistic)))
# The three per-plant scenarios are the input for the fleet-optimization model.
# NOTE: the target week (2026-07-27) is a full 5.5-day week, so for THIS forecast
# the regressor's effect is small; its value shows in weeks with a weekday holiday.

# --- 10b. Forecast visualization: recent history + next-week scenarios --------
recent_n <- 20
plot_df <- weekly_series %>%
  mutate(plant = as.integer(plant)) %>%
  group_by(plant) %>% arrange(week) %>% slice_tail(n = recent_n) %>% ungroup()
fc_points <- final_forecast %>%
  transmute(plant, week = forecast_week, pessimistic, regular, optimistic)

p_final <- ggplot() +
  geom_line(data = plot_df, aes(week, volume), color = "grey35", linewidth = 0.5) +
  geom_point(data = plot_df, aes(week, volume), color = "grey35", size = 0.7) +
  geom_errorbar(data = fc_points, aes(x = week, ymin = pessimistic, ymax = optimistic),
                width = 3, color = "steelblue") +
  geom_point(data = fc_points, aes(week, regular), color = "red", size = 2) +
  facet_wrap(~ plant, scales = "free_y", ncol = 2) +
  labs(title = "Next-week production forecast (h=1) with 80% interval",
       subtitle = "Grey = last 20 observed weeks | red = point forecast | blue bar = 80% PI",
       x = NULL, y = "Volume (m3)") +
  theme_minimal()
print(p_final)
ggsave(file.path(fig_folder, "final_weekly_forecast.png"),
       p_final, width = 11, height = 7, dpi = 150)

# ==============================================================================
# END OF WEEKLY PLANT-LEVEL FORECAST
# Final model: ARIMA + weighted working-days regressor, one order per plant.
# Deliverable: per-plant pessimistic/regular/optimistic weekly demand scenarios
# (80% PI), the input for the fleet-optimization model (Phase 2 of the TFM).
# ==============================================================================