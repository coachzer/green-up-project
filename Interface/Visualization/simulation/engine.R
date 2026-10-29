library(simmer)

sim_defaults <- function() {
  list(
    regions = c("Region1", "Region2", "Region3", "Region4"),
    wood_waste_types = c("Type1", "Type2"),
    generation_rate_increase = 1.0,
    collection_rate_increase = 1.0,
    handling_rate_increase = 1.0,
    recycling_rate_increase = 1.0,
    storage_cost_per_unit = 0.5,
    collection_cost_per_unit = 1.0,
    processing_cost_per_unit = 2.0,
    transportation_cost_per_unit = 0.3,
    overflow_penalty_per_unit = 2.0,
    recycling_revenue_per_unit = 2.0,
    avoided_disposal_cost_per_unit = 1.0,
    storage_capacity = 500,
    runtime = 50
  )
}

sim_seasons <- list(
  winter = 0.8,
  spring = 1.0,
  summer = 1.2,
  fall = 1.0
)

sim_max_collection_amount <- 25
sim_collection_interval <- 10
sim_handling_start <- 15
sim_recycling_start <- 20

sim_metrics <- c(
  "Total waste generated (kg)",
  "Total waste collected (kg)",
  "Total waste handled (kg)",
  "Total waste stored (kg)",
  "Total waste recycled (kg)",
  "Overflow penalty",
  "Total storage cost",
  "Total collection cost",
  "Total processing cost",
  "Total transportation cost",
  "Transportation cost after balancing",
  "Total recycling revenue",
  "Total avoided disposal cost"
)


sim_scenario_presets <- function(scenario = "baseline", season = NULL) {
  preset <- list()
  changes <- list()

  has_season <- length(season) == 1 && !is.na(season) && nzchar(trimws(season))

  if (scenario == "increased_waste_generation") {
    preset$generation_rate_increase <- 2
    changes <- c(changes, "Generation rate increase set to 2")
  } else if (scenario == "improved_collection_efficiency") {
    preset$collection_rate_increase <- 1.5
    changes <- c(changes, "Collection rate increase set to 1.5")
  } else if (scenario == "improved_handling_efficiency") {
    preset$handling_rate_increase <- 1.5
    changes <- c(changes, "Handling rate increase set to 1.5")
  } else if (scenario == "enhanced_recycling_programs") {
    preset$recycling_rate_increase <- 2
    preset$recycling_revenue_per_unit <- 5
    preset$avoided_disposal_cost_per_unit <- 2
    changes <- c(
      changes,
      "Recycling rate increase set to 2",
      "Recycling revenue per unit set to 5",
      "Avoided disposal cost per unit set to 2"
    )
  } else if (scenario == "storage_capacity_expansion") {
    preset$storage_capacity <- 1000
    changes <- c(changes, "Storage capacity set to 1000")
  } else if (scenario == "penalties_for_overflow") {
    preset$overflow_penalty_per_unit <- 5
    changes <- c(changes, "Overflow penalty per unit set to 5")
  } else if (scenario == "different_transportation_costs") {
    preset$transportation_cost_per_unit <- 0.5
    changes <- c(changes, "Transportation cost per unit set to 0.5")
  } else if (scenario == "seasonal_variations") {
    if (has_season && season == "Year-long") {
      preset$seasonal_variations <- TRUE
      changes <- c(changes, paste("Seasonal multiplier drawn per event from",
                                  paste(unique(unlist(sim_seasons)), collapse = ", ")))
    } else if (has_season) {
      preset$seasonal_variations <- FALSE
      preset$seasonal_multiplier <- sim_seasons[[tolower(season)]]
      changes <- c(changes, paste("Seasonal multiplier set to",
                                  preset$seasonal_multiplier, "for", season))
    } else {
      preset$seasonal_variations <- FALSE
      preset$seasonal_multiplier <- 1
      changes <- c(changes, "Seasonal variations disabled")
    }
  }

  list(values = preset, changes = changes)
}

sim_params <- function(scenario = "baseline", season = NULL, overrides = list()) {
  presets <- sim_scenario_presets(scenario, season)
  params <- utils::modifyList(utils::modifyList(sim_defaults(), presets$values), overrides)
  params$scenario <- scenario
  list(params = params, changes = presets$changes)
}


run_wood_waste_sim <- function(params, seed = NULL, distance_matrix = NULL) {
  if (!is.null(seed)) set.seed(seed)

  log <- sim_logger(params$scenario)
  env <- simmer("Wood Waste Flow Simulation")

  for (region in params$regions) {
    for (waste_type in params$wood_waste_types) {
      for (g in c("total_waste_generated_", "total_waste_collected_",
                  "total_waste_handled_", "total_waste_stored_",
                  "total_waste_recycled_", "overflow_penalty_",
                  "waste_to_collect_", "waste_to_handle_", "waste_to_recycle_",
                  "total_storage_cost_",
                  "total_collection_cost_", "total_processing_cost_",
                  "total_transportation_cost_", "total_recycling_revenue_",
                  "total_avoided_disposal_cost_")) {
        env <- add_global(env, paste0(g, region, "_", waste_type), 0)
      }
    }
  }

  n_regions <- length(params$regions)
  env <- env |>
    add_resource("collection_truck", capacity = n_regions) |>
    add_resource("waste_processor", capacity = n_regions) |>
    add_resource("recycling_facility", capacity = n_regions)

  rate_or_1 <- function(x) if (is.null(x) || is.na(x)) 1 else x

  for (region in params$regions) {
    for (waste_type in params$wood_waste_types) {
      trajs <- sim_trajectories(env, params, region, waste_type, log$log_event)
      env <- env |>
        add_generator(paste0("waste_gen_", region, "_", waste_type), trajs$generate_waste,
                      function() rexp(1, rate = 0.5 * rate_or_1(params$generation_rate_increase))) |>
        add_generator(paste0("waste_collector_", region, "_", waste_type), trajs$collect_waste,
                      function() rexp(1, rate = 1.5 * rate_or_1(params$collection_rate_increase))) |>
        add_generator(paste0("waste_handler_", region, "_", waste_type), trajs$handle_waste,
                      function() rexp(1, rate = 1 * rate_or_1(params$handling_rate_increase))) |>
        add_generator(paste0("waste_recycler_", region, "_", waste_type), trajs$recycle_waste,
                      function() rexp(1, rate = 1 * rate_or_1(params$recycling_rate_increase)))
    }
  }

  simmer::run(env, until = params$runtime)

  if (is.null(distance_matrix)) distance_matrix <- sim_distance_matrix(params$regions)
  transportation_costs <- sim_balancing_cost(env, params, distance_matrix)

  list(
    events = log$events(),
    costs = log$costs(),
    totals = sim_totals(env, params, transportation_costs),
    distance_matrix = distance_matrix,
    params = params
  )
}


sim_logger <- function(scenario) {
  n <- 0L
  time <- numeric(); region <- character(); waste_type <- character(); event <- character()
  amount <- numeric(); collection <- numeric(); processing <- numeric()
  transportation <- numeric(); revenue <- numeric()
  g <- function(env, name, r, w) get_global(env, paste0(name, r, "_", w))

  list(
    log_event = function(env, r, w, e) {
      n <<- n + 1L
      time[n] <<- simmer::now(env)
      region[n] <<- r
      waste_type[n] <<- w
      event[n] <<- e
      amount[n] <<- get_attribute(env, "waste_amount")
      collection[n] <<- g(env, "total_collection_cost_", r, w)
      processing[n] <<- g(env, "total_processing_cost_", r, w)
      transportation[n] <<- g(env, "total_transportation_cost_", r, w)
      revenue[n] <<- g(env, "total_recycling_revenue_", r, w)
      n
    },
    events = function() data.frame(
      Time = time, Region = region, WasteType = waste_type, Event = event,
      Amount = amount, Scenario = rep(scenario, n), stringsAsFactors = FALSE),
    costs = function() data.frame(
      Time = time, Region = region, WasteType = waste_type,
      CollectionCost = collection, ProcessingCost = processing,
      TransportationCost = transportation, RecyclingRevenue = revenue,
      Scenario = rep(scenario, n), stringsAsFactors = FALSE)
  )
}

sim_waste_amount <- function(env, params, min_val = 10, max_val = 30) {
  base_amount <- runif(1, min_val, max_val)
  multiplier <- if (!is.null(params$seasonal_variations) && params$seasonal_variations) {
    sim_seasons[[sample(names(sim_seasons), 1)]]
  } else if (!is.null(params$seasonal_multiplier)) {
    params$seasonal_multiplier
  } else {
    1
  }
  time_multiplier <- if (simmer::now(env) > 50) 1.5 else 1
  base_amount * multiplier * time_multiplier
}

sim_draw_80_to_100_pct <- function(x) {
  if (!is.na(x) && x > 0) {
    amount <- runif(1, x * 0.8, x)
    if (is.na(amount)) 0 else amount
  } else {
    0
  }
}

sim_trajectories <- function(env, params, region, waste_type, log_event) {
  key <- function(name) paste0(name, region, "_", waste_type)
  amount <- function() get_attribute(env, "waste_amount")
  add_to <- function(name, value) get_global(env, key(name)) + value

  generate_waste <- trajectory(paste("Generate Waste -", region, "-", waste_type)) |>
    set_attribute("waste_amount", function() sim_waste_amount(env, params)) |>
    set_global(key("total_waste_generated_"), function() add_to("total_waste_generated_", amount())) |>
    set_global(key("waste_to_collect_"), function() add_to("waste_to_collect_", amount())) |>
    set_attribute("log_row", function() log_event(env, region, waste_type, "Generation")) |>
    timeout(function() rexp(1, rate = 0.5))

  collect_waste <- trajectory(paste("Collect Waste -", region, "-", waste_type)) |>
    timeout(sim_collection_interval) |>
    seize("collection_truck") |>
    set_attribute("waste_amount", function()
      sim_draw_80_to_100_pct(min(sim_max_collection_amount, get_global(env, key("waste_to_collect_"))))) |>
    set_global(key("waste_to_collect_"), function() add_to("waste_to_collect_", -amount())) |>
    set_global(key("total_waste_collected_"), function() add_to("total_waste_collected_", amount())) |>
    set_global(key("overflow_penalty_"), function()
      add_to("overflow_penalty_", sim_handle_overflow(env, params, key("total_waste_stored_"), amount())$overflow_penalty)) |>
    set_global(key("total_waste_stored_"), function()
      sim_handle_overflow(env, params, key("total_waste_stored_"), amount())$new_storage) |>
    set_global(key("total_collection_cost_"), function()
      add_to("total_collection_cost_", amount() * params$collection_cost_per_unit)) |>
    set_global(key("total_storage_cost_"), function()
      add_to("total_storage_cost_", amount() * params$storage_cost_per_unit)) |>
    set_global(key("total_transportation_cost_"), function()
      add_to("total_transportation_cost_", amount() * params$transportation_cost_per_unit)) |>
    set_global(key("waste_to_handle_"), function() add_to("waste_to_handle_", amount())) |>
    set_attribute("log_row", function() log_event(env, region, waste_type, "Collection")) |>
    timeout(function() rexp(1, rate = 1.5)) |>
    release("collection_truck")

  handle_waste <- trajectory(paste("Handle Waste -", region, "-", waste_type)) |>
    timeout(sim_handling_start) |>
    seize("collection_truck") |>
    seize("waste_processor") |>
    set_attribute("waste_amount", function() sim_draw_80_to_100_pct(get_global(env, key("waste_to_handle_")))) |>
    set_global(key("waste_to_handle_"), function() add_to("waste_to_handle_", -amount())) |>
    set_global(key("total_waste_stored_"), function() max(0, add_to("total_waste_stored_", -amount()))) |>
    set_global(key("total_waste_handled_"), function() add_to("total_waste_handled_", amount())) |>
    set_global(key("total_processing_cost_"), function()
      add_to("total_processing_cost_", amount() * params$processing_cost_per_unit)) |>
    set_global(key("waste_to_recycle_"), function() add_to("waste_to_recycle_", amount())) |>
    set_attribute("log_row", function() log_event(env, region, waste_type, "Handling")) |>
    timeout(function() rexp(1, rate = 1)) |>
    release("waste_processor") |>
    release("collection_truck")

  recycle_waste <- trajectory(paste("Recycle Waste -", region, "-", waste_type)) |>
    timeout(sim_recycling_start) |>
    seize("recycling_facility") |>
    set_attribute("waste_amount", function() sim_draw_80_to_100_pct(get_global(env, key("waste_to_recycle_")))) |>
    set_global(key("waste_to_recycle_"), function() add_to("waste_to_recycle_", -amount())) |>
    set_global(key("total_waste_recycled_"), function() add_to("total_waste_recycled_", amount())) |>
    set_global(key("total_recycling_revenue_"), function()
      add_to("total_recycling_revenue_", amount() * params$recycling_revenue_per_unit)) |>
    set_global(key("total_avoided_disposal_cost_"), function()
      add_to("total_avoided_disposal_cost_", amount() * params$avoided_disposal_cost_per_unit)) |>
    set_attribute("log_row", function() log_event(env, region, waste_type, "Recycling")) |>
    timeout(function() rexp(1, rate = 0.5)) |>
    release("recycling_facility")

  list(generate_waste = generate_waste, collect_waste = collect_waste,
       handle_waste = handle_waste, recycle_waste = recycle_waste)
}

sim_handle_overflow <- function(env, params, stored_key, waste_amount) {
  capacity <- params$storage_capacity
  new_storage <- get_global(env, stored_key) + waste_amount
  if (new_storage >= capacity) {
    overflow_penalty <- (new_storage - capacity) * params$overflow_penalty_per_unit
    new_storage <- capacity
  } else {
    overflow_penalty <- 0
  }
  list(new_storage = new_storage, overflow_penalty = overflow_penalty)
}

sim_distance_matrix <- function(regions) {
  n <- length(regions)
  m <- matrix(runif(n * n, min = 1, max = 100), nrow = n, ncol = n)
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  diag(m) <- 0
  dimnames(m) <- list(regions, regions)
  m
}

sim_balancing_cost <- function(env, params, distance_matrix) {
  regions <- params$regions
  transportation_costs <- matrix(0, nrow = length(regions), ncol = length(params$wood_waste_types),
                                 dimnames = list(regions, params$wood_waste_types))

  for (waste_type in params$wood_waste_types) {
    level <- vapply(regions, function(region)
      get_global(env, paste0("total_waste_stored_", region, "_", waste_type)), numeric(1))
    avg_storage <- mean(level)
    for (region in regions) {
      for (source_region in regions) {
        need <- avg_storage - level[[region]]
        if (need <= 0) break
        moved <- min(need, level[[source_region]] - avg_storage)
        if (source_region == region || moved <= 0) next
        cost <- moved * params$transportation_cost_per_unit * distance_matrix[source_region, region]
        level[[region]] <- level[[region]] + moved
        level[[source_region]] <- level[[source_region]] - moved
        transportation_costs[source_region, waste_type] <- transportation_costs[source_region, waste_type] + cost
      }
    }
  }
  transportation_costs
}

sim_totals <- function(env, params, transportation_costs) {
  globals <- c("total_waste_generated_", "total_waste_collected_", "total_waste_handled_",
               "total_waste_stored_", "total_waste_recycled_", "overflow_penalty_",
               "total_storage_cost_", "total_collection_cost_", "total_processing_cost_",
               "total_transportation_cost_", NA, "total_recycling_revenue_",
               "total_avoided_disposal_cost_")
  rows <- list()
  for (region in params$regions) {
    for (waste_type in params$wood_waste_types) {
      value <- vapply(globals, function(g) {
        if (is.na(g)) transportation_costs[region, waste_type]
        else get_global(env, paste0(g, region, "_", waste_type))
      }, numeric(1), USE.NAMES = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(Region = region, WasteType = waste_type,
                                              Metric = sim_metrics, Value = value,
                                              stringsAsFactors = FALSE)
    }
  }
  dplyr::bind_rows(rows)
}
