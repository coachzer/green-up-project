source(test_path("..", "..", "engine.R"))

small_params <- function(scenario = "baseline", season = NULL, ...) {
  sim_params(scenario, season, overrides = utils::modifyList(
    list(regions = c("North", "South"), wood_waste_types = c("Sawdust"), runtime = 30),
    list(...)))$params
}

# The engine prints progress; keep test output readable
run_quiet <- function(params, seed = 1) {
  utils::capture.output(res <- run_wood_waste_sim(params, seed = seed))
  res
}
