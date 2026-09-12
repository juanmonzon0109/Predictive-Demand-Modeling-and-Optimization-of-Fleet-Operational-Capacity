# ==============================================================================
# 04_capacity_estimation.R
# Estimation of per-truck weekly capacity by plant (k_p) from recent data.
# Output: capacity indicators feeding the fleet-allocation model.
# ==============================================================================

setwd("C:/Users/USER/OneDrive/Desktop/TFM_FinalVersion")
library(tidyverse)
library(lubridate)

# ---- Parameters --------------------------------------------------------------

data_dir     <- "Data"
recent_weeks <- 26                                    # recent-capacity window
valid_plants <- c(510, 511, 512, 514, 515, 710)

# Default 2026 base fleet (excludes postuteros)
base_fleet <- tribble(
  ~plant, ~n_base,
  510,     9,
  511,    13,
  512,    11,
  514,    12,
  515,    10,
  710,     6
)

# ---- 1. Load and clean -------------------------------------------------------
# The 2026 Q1 file uses M/D/YYYY H:MM; the rest use YYYY-MM-DD HH:MM:SS.
# Without conditional parsing that quarter is dropped.

csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
df <- do.call(rbind, lapply(csv_files, read.csv))

df$order_date <- as.Date(ifelse(
  grepl("^\\d{1,2}/", df$order_date),
  format(as.Date(df$order_date, format = "%m/%d/%Y %H:%M"), "%Y-%m-%d"),
  format(as.Date(df$order_date), "%Y-%m-%d")))

if (any(is.na(df$order_date))) stop("Unparseable dates found.")

tickets <- df |>
  distinct() |>
  transmute(
    order_date = order_date,
    truck_code = as.character(truck_code),
    plant      = as.numeric(ship_plant_code),
    volume     = as.numeric(u_Volumen),
    cycle      = as.numeric(u_Cicle)
  ) |>
  filter(
    plant %in% valid_plants,
    volume > 0, volume <= 7,
    !is.na(truck_code)
  )

cat("Tickets after cleaning:", nrow(tickets), "\n")
cat("Date range:", format(min(tickets$order_date)),
    "to", format(max(tickets$order_date)), "\n\n")

# ---- 2. Weekly aggregation (Monday-start weeks) ------------------------------

data_max <- max(as_date(tickets$order_date))

weekly <- tickets |>
  mutate(week = floor_date(as_date(order_date), unit = "week", week_start = 1)) |>
  filter(week + 5 <= data_max) |>                     # boundary-week rule
  group_by(plant, week) |>
  summarise(
    volume = sum(volume),
    trips  = n(),
    trucks = n_distinct(truck_code),
    .groups = "drop"
  ) |>
  mutate(
    vol_per_truck   = volume / trucks,
    trips_per_truck = trips  / trucks,
    vol_per_trip    = volume / trips
  )

# ---- 3. Recent window --------------------------------------------------------

week_max <- max(weekly$week)
recent   <- weekly |> filter(week > week_max - weeks(recent_weeks))

cat("Recent window:", format(min(recent$week)), "to", format(max(recent$week)),
    "(", n_distinct(recent$week), "weeks )\n\n")

# ---- 4. Capacity indicators by plant -----------------------------------------

capacity <- recent |>
  group_by(plant) |>
  summarise(
    weeks           = n(),
    trucks_med      = median(trucks),
    trucks_max      = max(trucks),
    vol_per_trip    = round(median(vol_per_trip), 2),
    trips_per_truck = round(median(trips_per_truck), 1),
    k_p50           = round(quantile(vol_per_truck, 0.50), 0),
    k_p75           = round(quantile(vol_per_truck, 0.75), 0),
    k_p90           = round(quantile(vol_per_truck, 0.90), 0),
    k_max           = round(max(vol_per_truck), 0),
    .groups = "drop"
  ) |>
  left_join(base_fleet, by = "plant")

cat("--- Capacity indicators (m3 per truck per week) ---\n")
print(as.data.frame(capacity))

# Consistency check: does the decomposition reproduce the observed level?
cat("\n--- Decomposition check: trips_per_truck x vol_per_trip vs k_p50 ---\n")
capacity |>
  transmute(plant,
            decomposed = round(trips_per_truck * vol_per_trip, 0),
            k_p50,
            gap = round(trips_per_truck * vol_per_trip - k_p50, 0)) |>
  as.data.frame() |>
  print()

# ---- 5. Cycle time by plant (diagnostic) -------------------------------------

cat("\n--- Cycle time (u_Cicle) by plant ---\n")
tickets |>
  mutate(week = floor_date(as_date(order_date), unit = "week", week_start = 1)) |>
  filter(week > week_max - weeks(recent_weeks), !is.na(cycle), cycle > 0) |>
  group_by(plant) |>
  summarise(
    cycle_p25 = round(quantile(cycle, 0.25), 1),
    cycle_med = round(median(cycle), 1),
    cycle_p75 = round(quantile(cycle, 0.75), 1),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

# ---- 6. Fleet presence: regular trucks vs postuteros -------------------------

presence <- tickets |>
  mutate(week = floor_date(as_date(order_date), unit = "week", week_start = 1)) |>
  filter(week > week_max - weeks(recent_weeks)) |>
  group_by(plant, truck_code) |>
  summarise(weeks_active = n_distinct(week), .groups = "drop") |>
  mutate(status = if_else(weeks_active >= 0.8 * recent_weeks,
                          "regular", "occasional"))

cat("\n--- Trucks by presence status (last", recent_weeks, "weeks ) ---\n")
presence |>
  count(plant, status) |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  left_join(base_fleet, by = "plant") |>
  as.data.frame() |>
  print()

cat("\nTotal distinct trucks in window:", n_distinct(presence$truck_code), "\n")
cat("Declared base fleet:", sum(base_fleet$n_base), "\n")

# ---- 7. Weekly active-truck profile (postutero pool sizing) ------------------

cat("\n--- Active trucks per week, city level ---\n")
tickets |>
  mutate(week = floor_date(as_date(order_date), unit = "week", week_start = 1)) |>
  filter(week + 5 <= data_max) |>
  group_by(week) |>
  summarise(trucks = n_distinct(truck_code), .groups = "drop") |>
  summarise(
    min    = min(trucks),
    p25    = quantile(trucks, 0.25),
    median = median(trucks),
    p75    = quantile(trucks, 0.75),
    max    = max(trucks)
  ) |>
  as.data.frame() |>
  print()



# ==============================================================================
# 05_capacity_truck_level.R
# Per-truck weekly capacity by plant, estimated at truck-week level.
# Rationale: plant-week averages divide by every truck that appeared, including
# those present for a single trip, which deflates the capacity estimate.
# Requires objects from 04_capacity_estimation.R: tickets, recent, week_max,
# recent_weeks, base_fleet, valid_plants.
# ==============================================================================

# ---- 1. Truck-week activity within each plant --------------------------------

truck_week <- tickets |>
  mutate(week = floor_date(order_date, unit = "week", week_start = 1)) |>
  filter(week > week_max - weeks(recent_weeks)) |>
  group_by(plant, week, truck_code) |>
  summarise(
    volume = sum(volume),
    trips  = n(),
    days   = n_distinct(order_date),
    .groups = "drop"
  )

cat("Truck-week records:", nrow(truck_week), "\n")

cat("\n--- Days worked per truck-week ---\n")
truck_week |>
  count(days) |>
  mutate(share = round(100 * n / sum(n), 1)) |>
  as.data.frame() |>
  print()

# ---- 2. Full-time truck-weeks ------------------------------------------------
# A truck-week counts as full-time if the truck worked at least 5 days at the
# plant, i.e. it was genuinely assigned there rather than lent for a few trips.

full_time <- truck_week |> filter(days >= 5)

cat("\nFull-time truck-weeks:", nrow(full_time),
    "(", round(100 * nrow(full_time) / nrow(truck_week), 1), "% )\n")

cat("\n--- Weekly output of full-time trucks (m3/truck/week) ---\n")
full_time |>
  group_by(plant) |>
  summarise(
    n_obs = n(),
    p25   = round(quantile(volume, 0.25), 0),
    p50   = round(quantile(volume, 0.50), 0),
    p75   = round(quantile(volume, 0.75), 0),
    p90   = round(quantile(volume, 0.90), 0),
    trips_p50 = round(quantile(trips, 0.50), 0),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

# ---- 3. Capacity revealed under load -----------------------------------------
# Capacity is best observed when the plant is busy: in slack weeks trucks
# produce less because there is nothing to haul, not because they cannot.

peak_weeks <- recent |>
  group_by(plant) |>
  mutate(demand_quartile = ntile(volume, 4)) |>
  ungroup() |>
  filter(demand_quartile == 4) |>
  select(plant, week)

under_load <- full_time |> inner_join(peak_weeks, by = c("plant", "week"))

cat("\n--- Weekly output of full-time trucks in top-quartile demand weeks ---\n")
capacity_load <- under_load |>
  group_by(plant) |>
  summarise(
    n_obs      = n(),
    k_p50      = round(quantile(volume, 0.50), 0),
    k_p75      = round(quantile(volume, 0.75), 0),
    k_p90      = round(quantile(volume, 0.90), 0),
    trips_p50  = round(quantile(trips, 0.50), 0),
    vol_trip   = round(median(volume / trips), 2),
    .groups = "drop"
  )
print(as.data.frame(capacity_load))

# ---- 4. Comparison against the company KPI band ------------------------------
# GCC internal target: 450-510 m3/truck/month  ->  104-118 m3/truck/week.

cat("\n--- Estimated capacity vs KPI band (104-118 m3/truck/week) ---\n")
capacity_load |>
  transmute(
    plant,
    k_week  = k_p50,
    k_month = round(k_p50 * 4.333, 0),
    vs_band = case_when(
      k_p50 < 104 ~ "below",
      k_p50 > 118 ~ "above",
      TRUE        ~ "within"
    )
  ) |>
  as.data.frame() |>
  print()

# ---- 5. Feasibility of the base fleet against demand scenarios ---------------

demand <- tribble(
  ~plant, ~pessimistic, ~regular, ~optimistic,
  510,    834,  1134, 1434,
  511,    866,  1173, 1481,
  512,    885,  1209, 1534,
  514,    771,  1051, 1331,
  515,    903,  1234, 1565,
  710,    441,   607,  774
)

cat("\n--- Utilisation of the 61-truck base fleet (capacity under load) ---\n")
demand |>
  left_join(capacity_load |> select(plant, k_p50), by = "plant") |>
  left_join(base_fleet, by = "plant") |>
  transmute(
    plant,
    n_base,
    capacity = n_base * k_p50,
    u_pess   = round(pessimistic / capacity, 2),
    u_reg    = round(regular     / capacity, 2),
    u_opt    = round(optimistic  / capacity, 2)
  ) |>
  as.data.frame() |>
  print()

# ---- 6. Active trucks per week, recent window only ---------------------------
# Note: per-plant truck counts cannot be summed, since a truck may serve more
# than one plant in the same week. This is the city-level count.

cat("\n--- Active trucks per week, city level, recent window ---\n")
tickets |>
  mutate(week = floor_date(order_date, unit = "week", week_start = 1)) |>
  filter(week > week_max - weeks(recent_weeks)) |>
  group_by(week) |>
  summarise(trucks = n_distinct(truck_code), .groups = "drop") |>
  summarise(
    min    = min(trucks),
    p25    = quantile(trucks, 0.25),
    median = median(trucks),
    p75    = quantile(trucks, 0.75),
    max    = max(trucks)
  ) |>
  as.data.frame() |>
  print()







# ==============================================================================
# 06_fleet_optimization.R
# Fleet allocation by min-max utilisation.
#
# Decision : n_p, trucks assigned to plant p, with sum(n_p) = 61
# Objective: minimise max_p u_p, where u_p = D_p / (n_p * k_p)
#
# No cost parameters are used. Utilisation is measured against a plant-specific
# capacity ceiling k_p, so plants are judged against what they can physically do
# rather than against a common volume target.
#
# Requires capacity_load, demand and base_fleet from 05_capacity_truck_level.R.
# ==============================================================================

fleet_total <- sum(base_fleet$n_base)
plants      <- as.character(base_fleet$plant)

# ---- 1. Optimiser ------------------------------------------------------------
# Greedy: start with one truck per plant, then repeatedly give the next truck to
# the plant that is currently the bottleneck.

allocate_minmax <- function(D, k, n_total) {
  n <- setNames(rep(1, length(D)), names(D))
  for (i in seq_len(n_total - length(D))) {
    u <- D / (n * k)
    n[which.max(u)] <- n[which.max(u)] + 1
  }
  n
}

# Exhaustive single-move local search: confirms no transfer of one truck between
# any pair of plants lowers the maximum utilisation.

is_local_optimum <- function(n, D, k) {
  obj <- max(D / (n * k))
  for (i in names(n)) {
    if (n[i] <= 1) next
    for (j in names(n)) {
      if (i == j) next
      n2 <- n; n2[i] <- n2[i] - 1; n2[j] <- n2[j] + 1
      if (max(D / (n2 * k)) < obj - 1e-9) return(FALSE)
    }
  }
  TRUE
}

# ---- 2. Parameter vectors ----------------------------------------------------

k_p50 <- setNames(capacity_load$k_p50, as.character(capacity_load$plant))[plants]
k_p75 <- setNames(capacity_load$k_p75, as.character(capacity_load$plant))[plants]
k_p90 <- setNames(capacity_load$k_p90, as.character(capacity_load$plant))[plants]

D_pess <- setNames(demand$pessimistic, as.character(demand$plant))[plants]
D_reg  <- setNames(demand$regular,     as.character(demand$plant))[plants]
D_opt  <- setNames(demand$optimistic,  as.character(demand$plant))[plants]

n_current <- setNames(base_fleet$n_base, plants)

# ---- 3. Baseline: current allocation -----------------------------------------

cat("--- Current allocation, regular scenario ---\n")
u_current <- D_reg / (n_current * k_p50)
data.frame(
  plant = plants,
  n     = as.integer(n_current),
  k     = as.integer(k_p50),
  D     = as.integer(D_reg),
  u     = round(u_current, 3)
) |> print(row.names = FALSE)
cat("max u =", round(max(u_current), 3),
    "| min u =", round(min(u_current), 3),
    "| spread =", round(max(u_current) - min(u_current), 3), "\n\n")

# ---- 4. Optimal allocation, regular scenario ---------------------------------

n_opt <- allocate_minmax(D_reg, k_p50, fleet_total)
u_opt <- D_reg / (n_opt * k_p50)

cat("--- Optimal allocation, regular scenario ---\n")
data.frame(
  plant  = plants,
  n_curr = as.integer(n_current),
  n_opt  = as.integer(n_opt),
  change = as.integer(n_opt - n_current),
  u_curr = round(u_current, 3),
  u_opt  = round(u_opt, 3)
) |> print(row.names = FALSE)

cat("max u =", round(max(u_opt), 3),
    "| min u =", round(min(u_opt), 3),
    "| spread =", round(max(u_opt) - min(u_opt), 3), "\n")
cat("Spread reduction:",
    round(100 * (1 - (max(u_opt) - min(u_opt)) / (max(u_current) - min(u_current))), 1),
    "%\n")
cat("Verified local optimum:", is_local_optimum(n_opt, D_reg, k_p50), "\n")
cat("Trucks moved:", sum(pmax(n_opt - n_current, 0)), "\n\n")

# ---- 5. Sensitivity: 3 demand scenarios x 3 capacity percentiles -------------

cat("--- Sensitivity of the optimal allocation ---\n")
grid <- expand.grid(
  scenario = c("pessimistic", "regular", "optimistic"),
  capacity = c("p50", "p75", "p90"),
  stringsAsFactors = FALSE
)

sens <- lapply(seq_len(nrow(grid)), function(i) {
  D <- switch(grid$scenario[i], pessimistic = D_pess, regular = D_reg, optimistic = D_opt)
  k <- switch(grid$capacity[i], p50 = k_p50, p75 = k_p75, p90 = k_p90)
  n <- allocate_minmax(D, k, fleet_total)
  data.frame(
    scenario   = grid$scenario[i],
    capacity   = grid$capacity[i],
    allocation = paste(as.integer(n), collapse = "/"),
    max_u      = round(max(D / (n * k)), 3)
  )
}) |> do.call(what = rbind)

cat("Plant order:", paste(plants, collapse = "/"), "\n")
print(sens, row.names = FALSE)

cat("\nDistinct allocations across the 9 instances:",
    length(unique(sens$allocation)), "\n\n")

# ---- 6. Postutero requirement ------------------------------------------------
# Extra trucks needed per plant to keep utilisation at or below 1, given the
# optimised base allocation. This is the second-stage response to demand peaks.

cat("--- Postuteros required to hold u <= 1 under the optimised allocation ---\n")
post <- data.frame(
  plant = plants,
  n_opt = as.integer(n_opt),
  need_pess = pmax(0, ceiling(D_pess / k_p50) - n_opt),
  need_reg  = pmax(0, ceiling(D_reg  / k_p50) - n_opt),
  need_opt  = pmax(0, ceiling(D_opt  / k_p50) - n_opt)
)
print(post, row.names = FALSE)
cat("Total postuteros needed -- pessimistic:", sum(post$need_pess),
    "| regular:", sum(post$need_reg),
    "| optimistic:", sum(post$need_opt), "\n\n")

cat("--- Same requirement under the CURRENT allocation ---\n")
post_curr <- data.frame(
  plant = plants,
  n_curr = as.integer(n_current),
  need_pess = pmax(0, ceiling(D_pess / k_p50) - n_current),
  need_reg  = pmax(0, ceiling(D_reg  / k_p50) - n_current),
  need_opt  = pmax(0, ceiling(D_opt  / k_p50) - n_current)
)
print(post_curr, row.names = FALSE)
cat("Total postuteros needed -- pessimistic:", sum(post_curr$need_pess),
    "| regular:", sum(post_curr$need_reg),
    "| optimistic:", sum(post_curr$need_opt), "\n")







# ==============================================================================
# 07_allocation_backtest.R
# Persistence check: is the recommended reallocation a property of the target
# week, or a structural feature of the recent operating period?
#
# The min-max allocation is re-solved against the OBSERVED demand of each week
# in the recent window, using the same capacity ceilings.
#
# Requires: recent, capacity_load, base_fleet, plants, fleet_total, k_p50,
#           n_current, allocate_minmax  (from scripts 05 and 06).
# ==============================================================================

# ---- 1. Observed weekly demand by plant --------------------------------------

obs <- recent |>
  select(plant, week, volume) |>
  mutate(plant = as.character(plant)) |>
  tidyr::pivot_wider(names_from = plant, values_from = volume) |>
  arrange(week)

obs <- obs[complete.cases(obs), ]
cat("Weeks available for backtest:", nrow(obs), "\n\n")

# ---- 2. Re-solve the allocation week by week ---------------------------------

bt <- lapply(seq_len(nrow(obs)), function(i) {
  D <- unlist(obs[i, plants])
  names(D) <- plants
  n <- allocate_minmax(D, k_p50, fleet_total)
  u_o <- D / (n * k_p50)
  u_c <- D / (n_current * k_p50)
  data.frame(
    week        = obs$week[i],
    t(setNames(as.integer(n), plants)),
    spread_curr = round(max(u_c) - min(u_c), 3),
    spread_opt  = round(max(u_o) - min(u_o), 3),
    max_u_curr  = round(max(u_c), 3),
    max_u_opt   = round(max(u_o), 3),
    check.names = FALSE
  )
}) |> do.call(what = rbind)

cat("--- Optimal allocation solved on each observed week ---\n")
print(bt, row.names = FALSE)

# ---- 3. Distribution of the allocation across weeks --------------------------

cat("\n--- Trucks assigned per plant across the", nrow(bt), "weeks ---\n")
summ <- lapply(plants, function(p) {
  v <- bt[[p]]
  data.frame(
    plant     = p,
    n_current = as.integer(n_current[p]),
    n_target  = as.integer(n_opt[p]),
    bt_min    = min(v),
    bt_p25    = quantile(v, 0.25, names = FALSE),
    bt_median = median(v),
    bt_p75    = quantile(v, 0.75, names = FALSE),
    bt_max    = max(v)
  )
}) |> do.call(what = rbind)
print(summ, row.names = FALSE)

# ---- 4. How often does the backtest agree with the recommendation? -----------

cat("\n--- Direction of change vs current allocation, by week ---\n")
direction <- lapply(plants, function(p) {
  d <- bt[[p]] - as.integer(n_current[p])
  data.frame(
    plant  = p,
    fewer  = sum(d < 0),
    equal  = sum(d == 0),
    more   = sum(d > 0),
    mean_change = round(mean(d), 2)
  )
}) |> do.call(what = rbind)
print(direction, row.names = FALSE)

# ---- 5. Aggregate improvement -------------------------------------------------

cat("\n--- Utilisation spread, current vs optimised, over the window ---\n")
data.frame(
  measure = c("mean spread", "median spread", "mean max_u", "weeks max_u > 1"),
  current = c(round(mean(bt$spread_curr), 3),
              round(median(bt$spread_curr), 3),
              round(mean(bt$max_u_curr), 3),
              sum(bt$max_u_curr > 1)),
  optimised = c(round(mean(bt$spread_opt), 3),
                round(median(bt$spread_opt), 3),
                round(mean(bt$max_u_opt), 3),
                sum(bt$max_u_opt > 1))
) |> print(row.names = FALSE)

# ---- 6. Fixed recommended allocation evaluated on every observed week --------
# Applies the single recommended allocation n_opt to all weeks, rather than
# re-optimising each week. This is what GCC would actually implement.

cat("\n--- Fixed recommended allocation applied to every observed week ---\n")
fixed <- lapply(seq_len(nrow(obs)), function(i) {
  D <- unlist(obs[i, plants]); names(D) <- plants
  u_f <- D / (n_opt * k_p50)
  u_c <- D / (n_current * k_p50)
  data.frame(
    week       = obs$week[i],
    spread_curr = round(max(u_c) - min(u_c), 3),
    spread_fix  = round(max(u_f) - min(u_f), 3),
    better      = max(u_f) - min(u_f) < max(u_c) - min(u_c)
  )
}) |> do.call(what = rbind)

cat("Weeks where the fixed recommendation beats the current allocation:",
    sum(fixed$better), "of", nrow(fixed), "\n")
cat("Mean spread -- current:", round(mean(fixed$spread_curr), 3),
    "| recommended:", round(mean(fixed$spread_fix), 3), "\n")






# ==============================================================================
# 08_fixed_allocation_stochastic.R
# First-stage allocation over an empirical scenario set.
#
# The prediction-interval scenarios are near-proportional rescalings of one
# another, so they do not discriminate between allocations. The 26 observed
# weeks of the recent window do: they vary in the MIX of demand across plants,
# not only in its level. They are therefore used as the scenario set.
#
# Decision : a single fixed n_p, sum(n_p) = 61, held for every week
# Objective: minimise a functional of u_p = D_p^(s) / (n_p k_p) over scenarios s
#
# Requires: obs, plants, k_p50, n_current, n_opt, fleet_total (scripts 05-07).
# ==============================================================================

set.seed(42)

# ---- 1. Scenario matrix ------------------------------------------------------

Dmat <- as.matrix(obs[, plants])
colnames(Dmat) <- plants
cat("Scenario set:", nrow(Dmat), "weeks x", ncol(Dmat), "plants\n\n")

# ---- 2. Objective functions --------------------------------------------------

max_u_by_week <- function(n, Dmat, k) apply(Dmat, 1, function(D) max(D / (n * k)))

obj_expected  <- function(n, Dmat, k) mean(max_u_by_week(n, Dmat, k))
obj_worstcase <- function(n, Dmat, k) max(max_u_by_week(n, Dmat, k))
obj_postutero <- function(n, Dmat, k) {
  mean(apply(Dmat, 1, function(D) sum(pmax(0, ceiling(D / k) - n))))
}

# ---- 3. Multi-start local search ---------------------------------------------

local_search <- function(n0, Dmat, k, objective) {
  n <- n0; best <- objective(n, Dmat, k)
  repeat {
    improved <- FALSE
    for (i in seq_along(n)) {
      if (n[i] <= 1) next
      for (j in seq_along(n)) {
        if (i == j) next
        n2 <- n; n2[i] <- n2[i] - 1; n2[j] <- n2[j] + 1
        v <- objective(n2, Dmat, k)
        if (v < best - 1e-12) { n <- n2; best <- v; improved <- TRUE }
      }
    }
    if (!improved) break
  }
  list(n = n, obj = best)
}

optimise_fixed <- function(Dmat, k, n_total, objective, starts = 40) {
  p <- length(k)
  inits <- c(
    list(n_current, n_opt, setNames(rep(n_total %/% p, p), plants)),
    replicate(starts, {
      x <- rep(1, p)
      x <- x + as.vector(rmultinom(1, n_total - p, rep(1 / p, p)))
      setNames(x, plants)
    }, simplify = FALSE)
  )
  inits <- lapply(inits, function(x) { x <- as.numeric(x); names(x) <- plants
  x[p] <- x[p] + (n_total - sum(x)); x })
  res <- lapply(inits, local_search, Dmat = Dmat, k = k, objective = objective)
  res[[which.min(sapply(res, `[[`, "obj"))]]
}

# ---- 4. Solve under each objective -------------------------------------------

sol_exp  <- optimise_fixed(Dmat, k_p50, fleet_total, obj_expected)
sol_wc   <- optimise_fixed(Dmat, k_p50, fleet_total, obj_worstcase)
sol_post <- optimise_fixed(Dmat, k_p50, fleet_total, obj_postutero)

cat("--- Fixed allocations under each objective ---\n")
cat("Plant order:", paste(plants, collapse = "/"), "\n\n")
data.frame(
  criterion = c("Current (GCC)",
                "Target-week optimum",
                "Min expected max-u",
                "Min worst-case max-u",
                "Min expected postuteros"),
  allocation = c(paste(as.integer(n_current), collapse = "/"),
                 paste(as.integer(n_opt),     collapse = "/"),
                 paste(as.integer(sol_exp$n), collapse = "/"),
                 paste(as.integer(sol_wc$n),  collapse = "/"),
                 paste(as.integer(sol_post$n),collapse = "/"))
) |> print(row.names = FALSE)

# ---- 5. Evaluate every candidate under every metric --------------------------

candidates <- list(
  "Current (GCC)"           = n_current,
  "Target-week optimum"     = n_opt,
  "Min expected max-u"      = sol_exp$n,
  "Min worst-case max-u"    = sol_wc$n,
  "Min expected postuteros" = sol_post$n
)

cat("\n--- All candidates evaluated on the 26 observed weeks ---\n")
lapply(names(candidates), function(nm) {
  n <- candidates[[nm]]
  u <- max_u_by_week(n, Dmat, k_p50)
  sp <- apply(Dmat, 1, function(D) { v <- D / (n * k_p50); max(v) - min(v) })
  data.frame(
    criterion    = nm,
    mean_max_u   = round(mean(u), 3),
    worst_max_u  = round(max(u), 3),
    weeks_over_1 = sum(u > 1),
    mean_spread  = round(mean(sp), 3),
    mean_postut  = round(obj_postutero(n, Dmat, k_p50), 2)
  )
}) |> do.call(what = rbind) |> print(row.names = FALSE)

# ---- 6. Value of flexibility -------------------------------------------------
# Perfect-information bound: re-optimise the allocation separately on each week.
# The gap between the best fixed allocation and this bound is what no permanent
# reassignment can recover, and is the operational justification for postuteros.

pi_bound <- mean(apply(Dmat, 1, function(D) {
  names(D) <- plants
  n <- allocate_minmax(D, k_p50, fleet_total)
  max(D / (n * k_p50))
}))

cat("\n--- Value of flexibility (mean max-u) ---\n")
data.frame(
  quantity = c("Current allocation",
               "Best fixed allocation",
               "Weekly re-optimisation (perfect information)"),
  mean_max_u = round(c(obj_expected(n_current, Dmat, k_p50),
                       sol_exp$obj,
                       pi_bound), 3)
) |> print(row.names = FALSE)

cat("\nGain from reallocation :",
    round(obj_expected(n_current, Dmat, k_p50) - sol_exp$obj, 3), "\n")
cat("Residual gap to perfect information:",
    round(sol_exp$obj - pi_bound, 3), "\n")

# ---- 7. Sensitivity of the fixed solution to the capacity percentile ---------

cat("\n--- Fixed allocation under alternative capacity ceilings ---\n")
data.frame(
  capacity = c("p50", "p75", "p90"),
  allocation = c(
    paste(as.integer(optimise_fixed(Dmat, k_p50, fleet_total, obj_expected)$n), collapse = "/"),
    paste(as.integer(optimise_fixed(Dmat, k_p75, fleet_total, obj_expected)$n), collapse = "/"),
    paste(as.integer(optimise_fixed(Dmat, k_p90, fleet_total, obj_expected)$n), collapse = "/")
  )
) |> print(row.names = FALSE)








# ==============================================================================
# 09_exhaustive_verification.R
# Global-optimality check for the fixed allocation.
#
# With six plants and 61 trucks the feasible set is small enough to enumerate
# in full, so the solution reported in the thesis is a proven global optimum
# rather than the endpoint of a heuristic search.
#
# Requires: Dmat, plants, k_p50, fleet_total, sol_exp, obj_expected,
#           obj_postutero (script 08).
# ==============================================================================

lo <- 4L; hi <- 20L      # plausible per-plant bounds; widen if a bound binds

# ---- 1. Enumerate all feasible allocations -----------------------------------

grid5 <- as.matrix(expand.grid(rep(list(lo:hi), 5)))
n6    <- fleet_total - rowSums(grid5)
keep  <- n6 >= lo & n6 <= hi
cand  <- cbind(grid5[keep, , drop = FALSE], n6[keep])
colnames(cand) <- plants
storage.mode(cand) <- "integer"

cat("Feasible allocations enumerated:", format(nrow(cand), big.mark = ","), "\n")

# ---- 2. Evaluate expected maximum utilisation, vectorised --------------------
# For each plant, precompute the utilisation of every admissible truck count
# across all weeks, then take the elementwise maximum over plants.

eval_expected <- function(cand, Dmat, k) {
  u <- matrix(-Inf, nrow = nrow(cand), ncol = nrow(Dmat))
  for (p in seq_along(plants)) {
    A <- outer(1 / ((lo:hi) * k[p]), Dmat[, p])      # (hi-lo+1) x weeks
    u <- pmax(u, A[cand[, p] - lo + 1L, , drop = FALSE])
  }
  rowMeans(u)
}

obj_all <- eval_expected(cand, Dmat, k_p50)

best_i   <- which.min(obj_all)
best_n   <- setNames(as.numeric(cand[best_i, ]), plants)

cat("\n--- Global optimum, minimum expected max-u ---\n")
cat("Allocation:", paste(as.integer(best_n), collapse = "/"), "\n")
cat("Objective :", round(obj_all[best_i], 4), "\n")
cat("Local-search solution:", paste(as.integer(sol_exp$n), collapse = "/"),
    "| objective:", round(sol_exp$obj, 4), "\n")
cat("Match:", identical(as.integer(best_n), as.integer(sol_exp$n)), "\n")

# ---- 3. Are the bounds binding? ----------------------------------------------

cat("\nBound check (must be strictly inside [", lo, ",", hi, "]):",
    all(best_n > lo & best_n < hi), "\n")

# ---- 4. How flat is the optimum? ---------------------------------------------
# A very flat objective would mean the recommendation is arbitrary among many
# near-equivalent allocations, which matters for how firmly it is stated.

cat("\n--- Near-optimal allocations (within 1% of the optimum) ---\n")
near <- which(obj_all <= obj_all[best_i] * 1.01)
cat("Count:", length(near), "of", format(nrow(cand), big.mark = ","), "\n\n")

ord <- near[order(obj_all[near])][1:min(10, length(near))]
data.frame(
  allocation = apply(cand[ord, , drop = FALSE], 1,
                     function(r) paste(as.integer(r), collapse = "/")),
  mean_max_u = round(obj_all[ord], 4)
) |> print(row.names = FALSE)

# ---- 5. Range of each plant among the near-optimal set -----------------------

cat("\n--- Truck count per plant across near-optimal allocations ---\n")
data.frame(
  plant       = plants,
  recommended = as.integer(best_n),
  near_min    = apply(cand[near, , drop = FALSE], 2, min),
  near_max    = apply(cand[near, , drop = FALSE], 2, max)
) |> print(row.names = FALSE)






# ==============================================================================
# 10_figures.R
# Figures for the fleet-optimisation chapter.
#
# Requires: truck_week, capacity_load, obs, Dmat, plants, k_p50, n_current,
#           sol_exp, fleet_total, allocate_minmax  (scripts 05-08).
# ==============================================================================

library(ggplot2)

fig_folder <- "Figures/Fleet Optimization"
if (!dir.exists(fig_folder)) dir.create(fig_folder, recursive = TRUE)

# Recommended allocation. Replace with best_n once script 09 confirms it.
n_rec <- sol_exp$n

thesis_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(colour = "grey35", size = 10),
    legend.position = "top",
    legend.title = element_blank()
  )

col_curr <- "grey60"
col_rec  <- "#1F5C99"
col_ref  <- "#B03A2E"

# ---- Figure 1: days worked per truck-week ------------------------------------
# Motivates the capacity estimator: a quarter of truck-weeks are sporadic
# appearances that would otherwise enter the denominator as full units.

f1_data <- truck_week |>
  count(days) |>
  mutate(share  = 100 * n / sum(n),
         group  = if_else(days >= 5, "Full-time (>= 5 days)", "Partial (< 5 days)"))

f1 <- ggplot(f1_data, aes(factor(days), share, fill = group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Full-time (>= 5 days)" = col_rec,
                               "Partial (< 5 days)"    = col_curr)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Days worked per truck-week",
       subtitle = "Recent 26-week window, all plants",
       x = "Days worked in the week", y = "Share of truck-weeks (%)") +
  thesis_theme

ggsave(file.path(fig_folder, "fig1_truck_week_days.png"), f1,
       width = 7, height = 4, dpi = 300)

# ---- Figure 2: capacity decomposition ----------------------------------------
# Each plant sits on a different iso-capacity contour, driven by load size and
# trip intensity. This is why a common volume target is not attainable.

iso <- expand.grid(vol_trip = seq(2.5, 4.8, length.out = 100),
                   k = c(100, 125, 150, 175, 200)) |>
  mutate(trips = k / vol_trip)

f2 <- ggplot() +
  geom_line(data = iso, aes(vol_trip, trips, group = k),
            colour = "grey85", linewidth = 0.4) +
  geom_text(data = iso |> group_by(k) |> slice_max(vol_trip, n = 1),
            aes(vol_trip, trips, label = paste0(k, " m3")),
            hjust = -0.05, size = 2.8, colour = "grey55") +
  geom_point(data = capacity_load, aes(vol_trip, trips_p50),
             size = 3, colour = col_rec) +
  geom_text(data = capacity_load, aes(vol_trip, trips_p50, label = plant),
            vjust = -1.1, size = 3.3) +
  scale_x_continuous(limits = c(2.5, 5.1)) +
  labs(title = "Weekly capacity per truck, decomposed",
       subtitle = "Full-time trucks in top-quartile demand weeks; grey contours are constant capacity",
       x = expression("Load per trip ("*m^3*")"), y = "Trips per truck per week") +
  thesis_theme

ggsave(file.path(fig_folder, "fig2_capacity_decomposition.png"), f2,
       width = 7, height = 4.5, dpi = 300)

# ---- Figure 3: utilisation by plant, current vs recommended ------------------

D_reg_v <- setNames(demand$regular, as.character(demand$plant))[plants]

f3_data <- rbind(
  data.frame(plant = plants, u = as.numeric(D_reg_v / (n_current * k_p50)),
             alloc = "Current", n = as.integer(n_current)),
  data.frame(plant = plants, u = as.numeric(D_reg_v / (n_rec * k_p50)),
             alloc = "Recommended", n = as.integer(n_rec))
)

f3 <- ggplot(f3_data, aes(plant, u, fill = alloc)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = col_ref) +
  geom_text(aes(label = n), position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3, colour = "grey25") +
  annotate("text", x = 0.7, y = 1.03, label = "Capacity ceiling",
           hjust = 0, size = 3, colour = col_ref) +
  scale_fill_manual(values = c("Current" = col_curr, "Recommended" = col_rec)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Utilisation by plant, regular demand scenario",
       subtitle = "Labels give the number of trucks assigned",
       x = "Plant", y = expression(u[p]==D[p]/(n[p]*k[p]))) +
  thesis_theme

ggsave(file.path(fig_folder, "fig3_utilisation_by_plant.png"), f3,
       width = 7, height = 4, dpi = 300)

# ---- Figure 4: weekly bottleneck utilisation and the value of flexibility ----

f4_data <- do.call(rbind, lapply(seq_len(nrow(Dmat)), function(i) {
  D <- Dmat[i, ]; names(D) <- plants
  n_wk <- allocate_minmax(D, k_p50, fleet_total)
  data.frame(
    week = obs$week[i],
    Current      = max(D / (n_current * k_p50)),
    Recommended  = max(D / (n_rec     * k_p50)),
    `Weekly reallocation` = max(D / (n_wk * k_p50)),
    check.names = FALSE
  )
})) |>
  tidyr::pivot_longer(-week, names_to = "alloc", values_to = "u") |>
  mutate(alloc = factor(alloc, levels = c("Current", "Recommended",
                                          "Weekly reallocation")))

f4 <- ggplot(f4_data, aes(week, u, colour = alloc)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = col_ref) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  scale_colour_manual(values = c("Current" = col_curr,
                                 "Recommended" = col_rec,
                                 "Weekly reallocation" = "#2E8B57")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(title = "Bottleneck utilisation across the 26 observed weeks",
       subtitle = "The gap to weekly reallocation cannot be closed by any fixed assignment",
       x = NULL, y = expression(max[p]~u[p])) +
  thesis_theme

ggsave(file.path(fig_folder, "fig4_weekly_bottleneck.png"), f4,
       width = 7.5, height = 4.2, dpi = 300)

cat("Figures written to:", fig_folder, "\n")
print(list.files(fig_folder))







# ---- Figure 2: capacity decomposition ----------------------------------------

lab_offset <- data.frame(
  plant = c(510, 511, 512, 514, 515, 710),
  dx    = c( 0.00,  0.00,  0.00,  0.00,  0.14, -0.14),
  dy    = c( 1.60,  1.60,  1.60,  1.60,  1.60, -2.10)
)

f2_pts <- capacity_load |> left_join(lab_offset, by = "plant")

iso <- expand.grid(vol_trip = seq(2.6, 4.9, length.out = 100),
                   k = c(100, 125, 150, 175, 200)) |>
  mutate(trips = k / vol_trip)

f2 <- ggplot() +
  geom_line(data = iso, aes(vol_trip, trips, group = k),
            colour = "grey85", linewidth = 0.4) +
  geom_text(data = iso |> group_by(k) |> slice_max(vol_trip, n = 1),
            aes(vol_trip, trips, label = paste0(k, " m3")),
            hjust = -0.08, size = 2.8, colour = "grey55") +
  geom_point(data = f2_pts, aes(vol_trip, trips_p50), size = 3, colour = col_rec) +
  geom_text(data = f2_pts,
            aes(vol_trip + dx, trips_p50 + dy,
                label = paste0(plant, " (", k_p50, ")")),
            size = 3.2) +
  coord_cartesian(xlim = c(2.6, 5.15), ylim = c(30, 52)) +
  labs(title = "Weekly capacity per truck, decomposed",
       subtitle = "Full-time trucks in top-quartile demand weeks; labels give plant and capacity",
       x = expression("Load per trip ("*m^3*")"),
       y = "Trips per truck per week") +
  thesis_theme

ggsave(file.path(fig_folder, "fig2_capacity_decomposition.png"), f2,
       width = 7, height = 4.5, dpi = 300)
