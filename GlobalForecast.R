# ==============================================================================
# TFM - GLOBAL MONTHLY FORECAST (CITY LEVEL)
# Candidates: ARIMA | ARIMA + intervention | ETS | ARIMA + working days
# Exogenous variables tested: TIIE (Banxico) | Business confidence (INEGI)
#                             Weighted working days (deterministic)
# ==============================================================================

rm(list = ls())
setwd("C:/Users/USER/OneDrive/Desktop/TFM_FinalVersion")

library(dplyr); library(ggplot2); library(forecast)
library(tseries); library(lubridate)

fig_folder <- "Figures/Forecast Global"
if (!dir.exists(fig_folder)) dir.create(fig_folder, recursive = TRUE)

# System locale is Spanish; format(d, "%b") would return "ago." instead of "Aug"
month_label <- function(d) paste(month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))

save_base <- function(fname, expr, w = 1000, hh = 600) {
  eval(expr)
  png(file.path(fig_folder, fname), width = w, height = hh, res = 150)
  eval(expr); dev.off()
}

sig_table <- function(fit) {
  d <- data.frame(Coefficient = names(coef(fit)),
                  Estimate = round(coef(fit), 3),
                  StdError = round(sqrt(diag(fit$var.coef)), 3))
  d$t_stat <- round(d$Estimate / d$StdError, 2)
  d$Significant <- ifelse(abs(d$t_stat) > 1.96, "Yes", "No")
  d
}

calc_metrics <- function(a, f) data.frame(
  MAE  = round(mean(abs(a - f))),
  RMSE = round(sqrt(mean((a - f)^2))),
  MAPE = round(mean(abs((a - f) / a)) * 100, 2))

t_of <- function(fit, v) round(coef(fit)[v] / sqrt(fit$var.coef[v, v]), 2)


# --- 1. Data Loading ----------------------------------------------------------
# The 2026 Q1 file uses M/D/YYYY H:MM; the rest use YYYY-MM-DD HH:MM:SS
csv_files <- list.files("Data", pattern = "\\.csv$", full.names = TRUE)
df <- do.call(rbind, lapply(csv_files, read.csv))

df$order_date <- as.Date(ifelse(
  grepl("^\\d{1,2}/", df$order_date),
  format(as.Date(df$order_date, format = "%m/%d/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(df$order_date), "%Y-%m-%d")))

if (any(is.na(df$order_date))) stop("Unparseable dates found.")

monthly_data <- df %>%
  mutate(month = as.Date(paste0(format(order_date, "%Y-%m"), "-01"))) %>%
  group_by(month) %>%
  summarise(total_production = sum(u_Volumen), .groups = "drop") %>%
  arrange(month)

# ts() assumes consecutive rows: a calendar gap misaligns every later observation
full_months <- seq(min(monthly_data$month), max(monthly_data$month), by = "month")
gaps <- full_months[!full_months %in% monthly_data$month]
if (length(gaps) > 0) stop("Missing month(s): ", paste(format(gaps, "%Y-%m"), collapse = ", "))

last_month  <- max(monthly_data$month)
month_end   <- seq(last_month, by = "month", length.out = 2)[2] - 1
last_ticket <- max(df$order_date)

cat("--- Data ---\n")
cat("Series:", month_label(min(monthly_data$month)), "to", month_label(last_month),
    "|", nrow(monthly_data), "months\n")
cat("Last ticket:", format(last_ticket), "| Month ends:", format(month_end), "\n")
# A partial final month understates demand and drags the forecast down
cat(ifelse(last_ticket < month_end,
           ">>> WARNING: final month INCOMPLETE\n", "Final month complete.\n"))

h <- 2; n_origins <- 6
n_total  <- nrow(monthly_data)
n_train  <- n_total - h
start_ts <- c(year(min(monthly_data$month)), month(min(monthly_data$month)))

ts_data  <- ts(monthly_data$total_production, frequency = 12, start = start_ts)
train_ts <- ts(monthly_data$total_production[1:n_train], frequency = 12, start = start_ts)
test_ts  <- monthly_data$total_production[(n_train + 1):n_total]


# --- 2. Exploratory Data Analysis --------------------------------------------

save_base("boxplot_monthly_production.png", quote(
  boxplot(monthly_data$total_production,
          main = "Distribution of Total Monthly Production",
          ylab = "Total Production (m3)")), 800, 500)
cat("\nOutliers:", boxplot.stats(monthly_data$total_production)$out, "\n")
# Dec 2024: confirmed by the business as a special project, not a recording
# error, so it is retained and modelled via intervention

p_trend <- ggplot(monthly_data, aes(month, total_production)) + geom_line() +
  labs(title = "Monthly Total Production Trend", x = "Month", y = "Total Production (m3)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
print(p_trend)
ggsave(file.path(fig_folder, "monthly_production_trend.png"), p_trend,
       dpi = 300, width = 8, height = 5)

p_seasonal <- ggsubseriesplot(ts(monthly_data$total_production, frequency = 12),
                              main = "Seasonal Subseries Plot of Monthly Total Production")
print(p_seasonal)
ggsave(file.path(fig_folder, "seasonal_subseries.png"), p_seasonal,
       dpi = 300, width = 10, height = 5)

p_heatmap <- df %>%
  mutate(year = format(order_date, "%Y"), mn = as.numeric(format(order_date, "%m"))) %>%
  group_by(year, mn) %>%
  summarise(volume = round(sum(u_Volumen)), .groups = "drop") %>%
  ggplot(aes(factor(mn), year, fill = volume)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::comma(volume)), size = 3) +
  scale_fill_gradient(low = "lightyellow", high = "red", labels = scales::comma) +
  labs(title = "Monthly Production Volume by Year", x = "Month", y = "Year",
       fill = "Volume (m3)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
print(p_heatmap)
ggsave(file.path(fig_folder, "heatmap_monthly_volume.png"), p_heatmap,
       dpi = 300, width = 10, height = 5)


# --- 3. Stationarity and Order Identification ---------------------------------
# Levels p ~ 0.03 formally rejects non-stationarity, but the trend is unambiguous
# and the p-value borderline -> differencing is the prudent choice
diff_production <- diff(monthly_data$total_production)
cat("\nADF levels      p =", round(suppressWarnings(adf.test(monthly_data$total_production))$p.value, 4), "\n")
cat("ADF differenced p =", round(suppressWarnings(adf.test(diff_production))$p.value, 4),
    "(tseries truncates at 0.01 -> report as p < 0.01)\n")

save_base("differenced_series.png", quote(
  plot(diff_production, type = "l", main = "Differenced Monthly Total Production",
       xlab = "Time", ylab = "Differenced Total Production")), 800, 500)

# ACF cuts off after lag 1, PACF decays gradually -> MA signature.
# No spikes at lags 12/24/36 -> seasonal term not indicated at this stage
save_base("acf_differenced.png", quote(
  acf(diff_production, main = "ACF of Differenced Series", lag.max = 36)), 800, 500)
save_base("pacf_differenced.png", quote(
  pacf(diff_production, main = "PACF of Differenced Series", lag.max = 36)), 800, 500)


# --- 4. Baseline Models -------------------------------------------------------
# Split BEFORE selection, otherwise test-set information leaks into the structure.
# Test set = h months, matching the real horizon: a longer one would evaluate
# horizons never used and penalise ARIMA as its forecasts flatten out.

dummy_full <- as.numeric(monthly_data$month == as.Date("2024-12-01"))
if (sum(dummy_full) == 0) stop("Dec 2024 not found in the series.")

xreg_base_full  <- matrix(dummy_full, ncol = 1, dimnames = list(NULL, "special_project"))
xreg_base_train <- xreg_base_full[1:n_train, , drop = FALSE]

fit_arima <- auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fit_base  <- auto.arima(train_ts, xreg = xreg_base_train, seasonal = TRUE,
                        stepwise = FALSE, approximation = FALSE)
fit_ets   <- ets(train_ts)

cat("\n========== 4a. ARIMA (no intervention) ==========\n"); summary(fit_arima)
cat("\n========== 4b. ARIMA + intervention ==========\n"); summary(fit_base)
print(sig_table(fit_base), row.names = FALSE)
cat("\n========== 4c. ETS ==========\n"); summary(fit_ets)

# AIC valid ONLY within the ARIMA family: the ARIMA likelihood is computed on the
# differenced series and the ETS one on levels
cat("\n--- AIC (ARIMA family only) ---\n")
cat("ARIMA no intervention:", round(fit_arima$aic, 2), "\n")
cat("ARIMA + intervention: ", round(fit_base$aic, 2), "\n")
cat("ETS (reference only): ", round(fit_ets$aic, 2), "- NOT comparable\n")

for (m in list(list(fit_arima, "residuals_arima.png"),
               list(fit_base,  "residuals_arima_xreg.png"),
               list(fit_ets,   "residuals_ets.png"))) {
  print(checkresiduals(m[[1]]))
  png(file.path(fig_folder, m[[2]]), width = 1000, height = 600, res = 150)
  checkresiduals(m[[1]]); dev.off()
}


# --- 5. Exogenous Variable I: Banxico TIIE ------------------------------------
# Criterion: a variable significant ONLY without differencing is acting as a
# proxy for the trend, not as a genuine predictor

library(siebanxicor)
setToken("d18ebf0c9d3118739135a89bc5ae98b1f945b659f394a8aa3139b84017184d73")

tiie_monthly <- getSerieDataFrame(
  getSeriesData("SF43783", startDate = "2018-01-01",
                endDate = format(month_end)), "SF43783") %>%
  mutate(month = as.Date(paste0(format(as.Date(date), "%Y-%m"), "-01"))) %>%
  group_by(month) %>% summarise(tiie = mean(value, na.rm = TRUE), .groups = "drop")

# Lag 1: only the previous month's rate is known at forecast time
df_tiie <- monthly_data %>%
  left_join(tiie_monthly, by = "month") %>%
  mutate(tiie_lag1 = lag(tiie, 1), sp = dummy_full) %>%
  filter(!is.na(tiie_lag1))

# Nearly all lags significant with no distinct peak -> shared trend
save_base("ccf_tiie_production.png", quote(
  ccf(df_tiie$tiie, df_tiie$total_production,
      main = "Cross-Correlation: TIIE vs Concrete Production",
      xlab = "Lag", ylab = "CCF")), 800, 500)

n_t <- nrow(df_tiie); ntr_t <- n_t - h
y_t <- ts(df_tiie$total_production[1:ntr_t], frequency = 12,
          start = c(year(min(df_tiie$month)), month(min(df_tiie$month))))
x_t <- cbind(TIIE = df_tiie$tiie_lag1, special_project = df_tiie$sp)[1:ntr_t, , drop = FALSE]

fit_tiie_d0 <- auto.arima(y_t, xreg = x_t, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
fit_tiie_d1 <- auto.arima(y_t, xreg = x_t, d = 1, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

cat("\n===== TIIE: effect of differencing =====\n")
cat("d=0 (levels):      coef =", round(coef(fit_tiie_d0)["TIIE"], 2),
    "| t =", t_of(fit_tiie_d0, "TIIE"), "\n")
cat("d=1 (differenced): coef =", round(coef(fit_tiie_d1)["TIIE"], 2),
    "| t =", t_of(fit_tiie_d1, "TIIE"), "\n")
cat("Positive sign: higher rates would mean more concrete -> DISCARDED\n")


# --- 6. Exogenous Variable II: INEGI Business Confidence ----------------------
# Chosen because a bounded index centred on 50 points was expected to be
# trend-free; that premise proves empirically false

library(httr); library(jsonlite)

url_ice <- paste0("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/",
                  "jsonxml/INDICATOR/701407/es/00/false/BIE-BISE/2.0/",
                  "ea28946f-c592-caa9-ae01-1d648e9ec029?type=json")
obs_ice <- fromJSON(content(GET(url_ice), "text", encoding = "UTF-8"))$Series$OBSERVATIONS[[1]]

# The API returns observations in descending order
conf_monthly <- data.frame(
  month = as.Date(paste0(gsub("/", "-", obs_ice$TIME_PERIOD), "-01")),
  confidence = as.numeric(obs_ice$OBS_VALUE)) %>%
  filter(!is.na(confidence)) %>% arrange(month)

df_conf <- monthly_data %>%
  left_join(conf_monthly, by = "month") %>%
  mutate(conf_lag1 = lag(confidence, 1), sp = dummy_full) %>%
  filter(!is.na(conf_lag1))

p_conf <- monthly_data %>%
  left_join(conf_monthly, by = "month") %>%
  { sf <<- mean(.$total_production) / mean(.$confidence, na.rm = TRUE); . } %>%
  ggplot(aes(month)) +
  geom_line(aes(y = total_production, color = "Production (m3)"), linewidth = 0.7) +
  geom_line(aes(y = confidence * sf, color = "Confidence Index"),
            linewidth = 0.7, linetype = "dashed") +
  scale_y_continuous(name = "Total Production (m3)", labels = scales::comma,
                     sec.axis = sec_axis(~ . / sf, name = "Confidence Index")) +
  scale_color_manual(values = c("Production (m3)" = "black",
                                "Confidence Index" = "steelblue"), name = NULL) +
  labs(title = "Concrete Production vs Construction Business Confidence", x = "Month") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),
                          legend.position = "bottom")
print(p_conf)
ggsave(file.path(fig_folder, "production_vs_confidence.png"), p_conf,
       dpi = 300, width = 10, height = 5)

# Broad plateau of significant correlations rather than an isolated peak
save_base("ccf_confidence_production.png", quote(
  ccf(df_conf$confidence, df_conf$total_production,
      main = "Cross-Correlation: Business Confidence vs Concrete Production",
      xlab = "Lag", ylab = "CCF")), 800, 500)

cat("\n===== BUSINESS CONFIDENCE =====\n")
cat("ADF confidence p =",
    round(suppressWarnings(adf.test(df_conf$confidence))$p.value, 4),
    "-> a bounded index is not necessarily trend-free\n")

n_c <- nrow(df_conf); ntr_c <- n_c - h
y_c <- ts(df_conf$total_production[1:ntr_c], frequency = 12,
          start = c(year(min(df_conf$month)), month(min(df_conf$month))))
x_c <- cbind(confidence = df_conf$conf_lag1, special_project = df_conf$sp)

fit_conf <- auto.arima(y_c, xreg = x_c[1:ntr_c, , drop = FALSE], d = 1,
                       seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
cat("Coefficient:", round(coef(fit_conf)["confidence"], 2),
    "| t =", t_of(fit_conf, "confidence"), "\n")
cat("Non-significant and negative sign -> DISCARDED\n")


# --- 7. Exogenous Variable III: Weighted Working Days -------------------------
# Deterministic and trend-free by construction: known with certainty for future
# months, so the regressor adds no forecast error of its own. Reflects a physical
# capacity constraint rather than an economic hypothesis.

easter_sunday <- function(y) {
  a <- y %% 19; b <- y %/% 100; c <- y %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  hh <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2 * e + 2 * i - hh - k) %% 7
  m <- (a + 11 * hh + 22 * l) %/% 451
  as.Date(sprintf("%d-%02d-%02d", y, (hh + l - 7 * m + 114) %/% 31,
                  ((hh + l - 7 * m + 114) %% 31) + 1))
}

nth_weekday <- function(y, mo, wd, n) {
  first <- as.Date(sprintf("%d-%02d-01", y, mo))
  first + ((wd - as.integer(format(first, "%u"))) %% 7) + 7 * (n - 1)
}

# Art. 74 LFT plus Holy Thursday and Good Friday (not statutory but widely
# observed in construction)
build_holidays <- function(years) {
  hol <- as.Date(character(0))
  for (y in years) {
    e <- easter_sunday(y)
    hol <- c(hol,
             as.Date(sprintf("%d-01-01", y)), nth_weekday(y, 2, 1, 1),
             nth_weekday(y, 3, 1, 3), as.Date(sprintf("%d-05-01", y)),
             as.Date(sprintf("%d-09-16", y)), nth_weekday(y, 11, 1, 3),
             as.Date(sprintf("%d-12-25", y)), e - 3, e - 2)
    if (y %% 6 == 0) hol <- c(hol, as.Date(sprintf("%d-10-01", y)))
  }
  sort(unique(hol))
}

# Mon-Fri = 1, Sat = sat_w, Sun = 0, holiday = 0 (holiday overrides the weekday)
count_weighted_days <- function(m0, holidays, sat_w) {
  days <- seq(m0, seq(m0, by = "month", length.out = 2)[2] - 1, by = "day")
  wd <- as.integer(format(days, "%u"))
  w <- ifelse(wd <= 5, 1.0, ifelse(wd == 6, sat_w, 0))
  w[days %in% holidays] <- 0
  sum(w)
}

all_months <- seq(min(monthly_data$month), last_month + months(h), by = "month")
holidays <- build_holidays(unique(as.integer(format(all_months, "%Y"))))

wv <- df %>%
  mutate(wd = as.integer(format(order_date, "%u"))) %>%
  group_by(wd) %>%
  summarise(vol_per_day = round(sum(u_Volumen) / n_distinct(order_date)), .groups = "drop") %>%
  mutate(day = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")[wd])

cat("\n===== WORKING DAYS =====\n")
cat("Volume per calendar day, by weekday:\n")
print(as.data.frame(wv[, c("day", "vol_per_day")]), row.names = FALSE)
cat("Observed Saturday ratio:",
    round(wv$vol_per_day[wv$wd == 6] / mean(wv$vol_per_day[wv$wd <= 5]), 3), "\n")
# Only the Saturday weight is calibrated: refining every weekday would mean
# estimating seven calendar parameters from ~79 monthly observations

build_xreg <- function(sat_w) {
  wd_tab <- data.frame(month = all_months,
                       working_days = sapply(all_months, count_weighted_days,
                                             holidays = holidays, sat_w = sat_w))
  d <- monthly_data %>% left_join(wd_tab, by = "month")
  list(xreg = cbind(working_days = d$working_days, special_project = dummy_full),
       table = wd_tab)
}

# Candidate weights evaluated on the same rolling forecast used for model
# selection, so the choice rests on out-of-sample performance
eval_weight <- function(sat_w) {
  b <- build_xreg(sat_w)
  fit <- auto.arima(train_ts, xreg = b$xreg[1:n_train, , drop = FALSE], d = 1,
                    seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  err <- data.frame()
  for (i in 1:n_origins) {
    te <- n_total - h - (i - 1)
    tw <- ts(monthly_data$total_production[1:te], frequency = 12, start = start_ts)
    act <- monthly_data$total_production[(te + 1):(te + h)]
    f <- forecast(Arima(tw, order = fit$arma[c(1, 6, 2)],
                        seasonal = list(order = fit$arma[c(3, 7, 4)], period = 12),
                        xreg = b$xreg[1:te, , drop = FALSE]),
                  h = h, xreg = b$xreg[(te + 1):(te + h), , drop = FALSE])
    err <- rbind(err, data.frame(actual = act, pred = as.numeric(f$mean)))
  }
  data.frame(sat_weight = sat_w,
             coef = round(coef(fit)["working_days"], 1),
             t_stat = t_of(fit, "working_days"),
             AIC = round(fit$aic, 2),
             MAPE = round(mean(abs((err$actual - err$pred) / err$actual)) * 100, 2))
}

cat("\nSaturday weight calibration:\n")
calib <- do.call(rbind, lapply(c(0.50, 0.60, 0.65), eval_weight))
print(calib, row.names = FALSE)
sat_best <- calib$sat_weight[which.min(calib$MAPE)]
cat("Selected weight:", sat_best, "\n")
# Coefficient stable across weights -> result does not depend on the calibration

b <- build_xreg(sat_best)
xreg_wd_full <- b$xreg; wd_table <- b$table
xreg_wd_train <- xreg_wd_full[1:n_train, , drop = FALSE]
stopifnot(length(train_ts) == nrow(xreg_wd_train))

df_wd <- monthly_data %>%
  mutate(working_days = xreg_wd_full[, "working_days"],
         sp = dummy_full,
         vol_per_day = total_production / working_days)

cat("Range:", min(wd_table$working_days), "-", max(wd_table$working_days),
    "| Correlation with production:", round(cor(df_wd$working_days, df_wd$total_production), 3), "\n")
cat("Mean volume per working day:", round(mean(df_wd$vol_per_day)), "m3\n")
cat("ADF working days p =", round(suppressWarnings(adf.test(df_wd$working_days))$p.value, 4),
    "-> stationary\n")
# Low correlation in levels is expected: production carries a trend that working
# days do not share. In a d=1 model what matters is the relationship between
# DIFFERENCES. Mirror image of the TIIE case.

p_scatter <- ggplot(df_wd, aes(working_days, total_production)) +
  geom_point(aes(color = factor(sp)), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", linewidth = 0.8) +
  scale_color_manual(values = c("0" = "black", "1" = "red"),
                     labels = c("Normal month", "Special project"), name = NULL) +
  labs(title = "Monthly Production vs Weighted Working Days",
       x = paste0("Weighted working days (Mon-Fri = 1, Sat = ", sat_best, ", holidays = 0)"),
       y = "Total Production (m3)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5),
                          legend.position = "bottom")
print(p_scatter)
ggsave(file.path(fig_folder, "scatter_working_days.png"), p_scatter,
       dpi = 300, width = 8, height = 5)

p_vol_day <- ggplot(df_wd, aes(month, vol_per_day)) + geom_line() +
  labs(title = "Production per Weighted Working Day",
       x = "Month", y = "m3 per working day") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
print(p_vol_day)
ggsave(file.path(fig_folder, "volume_per_working_day.png"), p_vol_day,
       dpi = 300, width = 8, height = 5)

fit_wd <- auto.arima(train_ts, xreg = xreg_wd_train, d = 1,
                     seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

cat("\n========== ARIMA + working days ==========\n"); summary(fit_wd)
print(sig_table(fit_wd), row.names = FALSE)
# auto.arima DROPS the seasonal term here: the "weak seasonality" of the base
# model was a calendar effect

save_base("residuals_working_days.png", quote(print(checkresiduals(fit_wd))))

cat("\n--- AIC (comparable: both ARIMA d=1, same", n_train, "observations) ---\n")
cat("ARIMA + intervention:", round(fit_base$aic, 2), "\n")
cat("ARIMA + working days:", round(fit_wd$aic, 2), "\n")


# --- 8. Rolling Forecast Evaluation (h=2, 6 origins) -------------------------
# A single test set depends heavily on where the cut-off falls. All models keep a
# FIXED structure inside the loop: parameters are re-estimated, structure is not
# re-selected, otherwise the comparison is asymmetric.
# Working days for the target months enter at their REAL value: the calendar is
# not uncertain, so this is leakage-safe. The intervention dummy, by contrast,
# would be 0 for any unknown future event.

a_order <- fit_arima$arma[c(1, 6, 2)]
a_seas  <- list(order = fit_arima$arma[c(3, 7, 4)], period = 12)
b_order <- fit_base$arma[c(1, 6, 2)]
b_seas  <- list(order = fit_base$arma[c(3, 7, 4)], period = 12)
w_order <- fit_wd$arma[c(1, 6, 2)]
w_seas  <- list(order = fit_wd$arma[c(3, 7, 4)], period = 12)

errors <- data.frame()

for (i in 1:n_origins) {
  te <- n_total - h - (i - 1)
  tw  <- ts(monthly_data$total_production[1:te], frequency = 12, start = start_ts)
  act <- monthly_data$total_production[(te + 1):(te + h)]
  
  f_a <- forecast(Arima(tw, order = a_order, seasonal = a_seas), h = h)
  f_b <- forecast(Arima(tw, order = b_order, seasonal = b_seas,
                        xreg = xreg_base_full[1:te, , drop = FALSE]),
                  h = h, xreg = xreg_base_full[(te + 1):(te + h), , drop = FALSE])
  f_e <- forecast(ets(tw, model = fit_ets), h = h)
  f_w <- forecast(Arima(tw, order = w_order, seasonal = w_seas,
                        xreg = xreg_wd_full[1:te, , drop = FALSE]),
                  h = h, xreg = xreg_wd_full[(te + 1):(te + h), , drop = FALSE])
  
  errors <- rbind(errors, data.frame(
    origin = i, last_train = month_label(monthly_data$month[te]),
    actual_h1 = act[1], actual_h2 = act[2],
    arima_h1 = round(f_a$mean[1]), arima_h2 = round(f_a$mean[2]),
    base_h1  = round(f_b$mean[1]), base_h2  = round(f_b$mean[2]),
    ets_h1   = round(f_e$mean[1]), ets_h2   = round(f_e$mean[2]),
    wd_h1    = round(f_w$mean[1]), wd_h2    = round(f_w$mean[2])))
}

cat("\n--- Rolling forecast detail ---\n")
print(errors, row.names = FALSE)

obs_actual <- c(errors$actual_h1, errors$actual_h2)

# Out-of-sample metrics are the only valid basis for comparing across families
metrics_table <- rbind(
  cbind(Model = "ARIMA (no intervention)", calc_metrics(obs_actual, c(errors$arima_h1, errors$arima_h2))),
  cbind(Model = "ARIMA + intervention",    calc_metrics(obs_actual, c(errors$base_h1,  errors$base_h2))),
  cbind(Model = "ETS",                     calc_metrics(obs_actual, c(errors$ets_h1,   errors$ets_h2))),
  cbind(Model = "ARIMA + working days",    calc_metrics(obs_actual, c(errors$wd_h1,    errors$wd_h2))))

cat("\n===== ACCURACY (Rolling forecast h=2, 6 origins) =====\n")
print(metrics_table, row.names = FALSE)

mape_h <- function(c1, c2) round(c(
  mean(abs((errors$actual_h1 - errors[[c1]]) / errors$actual_h1)) * 100,
  mean(abs((errors$actual_h2 - errors[[c2]]) / errors$actual_h2)) * 100), 2)

m_a <- mape_h("arima_h1", "arima_h2"); m_b <- mape_h("base_h1", "base_h2")
m_e <- mape_h("ets_h1", "ets_h2");     m_w <- mape_h("wd_h1", "wd_h2")

cat("\nMAPE by horizon (h=1 | h=2):\n")
cat("ARIMA (no interv.): ", m_a, "\n")
cat("ARIMA + interv.:    ", m_b, "\n")
cat("ETS:                ", m_e, "\n")
cat("ARIMA + work. days: ", m_w, "\n")

p_cmp <- data.frame(
  Model = rep(c("ARIMA", "ARIMA+Interv.", "ETS", "ARIMA+WorkDays"), each = 2),
  Horizon = rep(c("h=1", "h=2"), 4),
  MAPE = c(m_a, m_b, m_e, m_w)) %>%
  mutate(Model = factor(Model, levels = c("ARIMA", "ARIMA+Interv.", "ETS", "ARIMA+WorkDays"))) %>%
  ggplot(aes(Model, MAPE, fill = Horizon)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(MAPE, "%")), position = position_dodge(.9),
            vjust = -0.5, size = 3.2) +
  scale_y_continuous(expand = expansion(mult = c(0, .12))) +
  labs(title = "Model Comparison: MAPE by Horizon (Rolling Forecast, 6 origins)",
       y = "MAPE (%)") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
print(p_cmp)
ggsave(file.path(fig_folder, "model_comparison_mape.png"), p_cmp,
       dpi = 300, width = 10, height = 5)


# --- 9. Final Model and Demand Scenarios --------------------------------------
# Re-estimated on ALL data: evaluation picks the specification, the operational
# forecast must use every observation.

fut_wd <- wd_table$working_days[wd_table$month > last_month][1:h]

fit_final <- Arima(ts_data, order = w_order, seasonal = w_seas, xreg = xreg_wd_full)
xreg_fut  <- cbind(working_days = fut_wd, special_project = rep(0, h))
# working_days: real calendar value | special_project: 0, no event anticipated

cat("\n========== FINAL MODEL ==========\n"); summary(fit_final)
print(sig_table(fit_final), row.names = FALSE)
cat("\nFuture working days:", fut_wd, "\n")

fc_final <- forecast(fit_final, h = h, xreg = xreg_fut, level = 80)
fut_months <- seq(last_month + months(1), by = "month", length.out = h)

scenarios <- data.frame(
  Month       = month_label(fut_months),
  Pessimistic = round(as.numeric(fc_final$lower)),
  Regular     = round(as.numeric(fc_final$mean)),
  Optimistic  = round(as.numeric(fc_final$upper)))

cat("\n===== DEMAND SCENARIOS (80% CI) =====\n")
print(scenarios, row.names = FALSE)
# 80% preferred over 95%: the latter yields a range too wide for fleet sizing

scen_df <- data.frame(month = fut_months,
                      pes = as.numeric(fc_final$lower),
                      reg = as.numeric(fc_final$mean),
                      opt = as.numeric(fc_final$upper))

p_scen <- ggplot(scen_df) +
  geom_ribbon(aes(month, ymin = pes, ymax = opt), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(month, reg, color = "Regular"), linewidth = 1.2) +
  geom_line(aes(month, opt, color = "Optimistic"), linewidth = 0.8, linetype = "dashed") +
  geom_line(aes(month, pes, color = "Pessimistic"), linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(month, reg), size = 3, color = "blue") +
  geom_point(aes(month, opt), size = 3, color = "darkgreen") +
  geom_point(aes(month, pes), size = 3, color = "red") +
  geom_text(aes(month, reg, label = scales::comma(round(reg))), vjust = -1.2, size = 3.5) +
  geom_text(aes(month, opt, label = scales::comma(round(opt))), vjust = -1.2,
            size = 3, color = "darkgreen") +
  geom_text(aes(month, pes, label = scales::comma(round(pes))), vjust = 2,
            size = 3, color = "red") +
  scale_x_date(breaks = scen_df$month, labels = month_label(scen_df$month),
               expand = expansion(mult = 0.18)) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = 0.12)) +
  scale_color_manual(values = c("Regular" = "blue", "Optimistic" = "darkgreen",
                                "Pessimistic" = "red")) +
  labs(title = "2-Month Production Forecast Scenarios (80% CI)",
       x = "Month", y = "Total Production (m3)", color = "Scenario") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 14),
                          legend.position = "bottom")
print(p_scen)
ggsave(file.path(fig_folder, "forecast_scenarios.png"), p_scen,
       dpi = 300, width = 10, height = 5)

# ==============================================================================
# The three scenarios are the input for the fleet optimization model.
# ==============================================================================