
test_that("same seed gives the same result, different seeds differ", {
  p <- small_params()
  a <- run_quiet(p, seed = 7)
  b <- run_quiet(p, seed = 7)
  c <- run_quiet(p, seed = 8)
  expect_identical(a, b)
  expect_false(identical(a$events, c$events))
})

test_that("runtime bounds event times", {
  res <- run_quiet(small_params(runtime = 5))
  expect_gt(nrow(res$events), 0)
  expect_true(all(res$events$Time <= 5))
})

test_that("regions and waste types come from params", {
  res <- run_quiet(small_params(regions = c("A", "B", "C"), wood_waste_types = c("x", "y")))
  expect_setequal(unique(res$events$Region), c("A", "B", "C"))
  expect_setequal(unique(res$events$WasteType), c("x", "y"))
  expect_equal(dimnames(res$distance_matrix), list(c("A", "B", "C"), c("A", "B", "C")))
})

test_that("scenario presets override user values and label the output", {
  p <- small_params("storage_capacity_expansion", storage_capacity = 200)
  expect_equal(p$storage_capacity, 1000)
  expect_equal(small_params("penalties_for_overflow")$overflow_penalty_per_unit, 5)
  expect_equal(small_params("seasonal_variations", "Summer")$seasonal_multiplier, 1.2)
  res <- run_quiet(p)
  expect_true(all(res$events$Scenario == "storage_capacity_expansion"))
})

test_that("stored waste never exceeds storage capacity", {
  res <- run_quiet(small_params(storage_capacity = 40, runtime = 60))
  stored <- res$totals$Value[res$totals$Metric == "Total waste stored (kg)"]
  expect_true(all(stored <= 40))
  expect_true(any(stored == 40))
})

test_that("generation rate changes how much is generated", {
  gen <- function(rate) {
    t <- run_quiet(small_params(generation_rate_increase = rate, runtime = 60), seed = 3)$totals
    sum(t$Value[t$Metric == "Total waste generated (kg)"])
  }
  expect_gt(gen(2), gen(1))
})

test_that("output shape matches what the dashboard renders", {
  res <- run_quiet(small_params())
  expect_named(res, c("events", "costs", "totals", "distance_matrix", "params"))
  expect_named(res$events, c("Time", "Region", "WasteType", "Event", "Amount", "Scenario"))
  expect_named(res$costs, c("Time", "Region", "WasteType", "CollectionCost", "ProcessingCost",
                            "TransportationCost", "RecyclingRevenue", "Scenario"))
  expect_named(res$totals, c("Region", "WasteType", "Metric", "Value"))
  expect_type(res$events$Time, "double")
  expect_type(res$events$Amount, "double")
  expect_type(res$totals$Value, "double")
  expect_true(all(res$events$Event %in% c("Generation", "Collection", "Handling", "Recycling")))
  expect_equal(nrow(res$totals), length(sim_metrics) * 2 * 1)
  expect_identical(unique(res$totals$Metric), sim_metrics)
  expect_true(all(is.finite(res$totals$Value)))
})

test_that("overflow uses the scenario's capacity and penalty, not the raw inputs", {
  totals <- function(p) run_quiet(p, seed = 5)$totals
  metric <- function(t, m) t$Value[t$Metric == m]

  t <- totals(small_params("storage_capacity_expansion", storage_capacity = 40, runtime = 60))
  expect_true(all(metric(t, "Total waste stored (kg)") < 1000))
  expect_true(all(metric(t, "Overflow penalty") == 0))

  base <- totals(small_params(storage_capacity = 40, overflow_penalty_per_unit = 2, runtime = 60))
  pen <- totals(small_params("penalties_for_overflow", storage_capacity = 40,
                             overflow_penalty_per_unit = 2, runtime = 60))
  expect_gt(sum(metric(base, "Overflow penalty")), 0)
  expect_equal(metric(pen, "Overflow penalty"), metric(base, "Overflow penalty") * 2.5)
})

test_that("a run prints nothing to the console", {
  expect_silent(run_wood_waste_sim(small_params(), seed = 1))
})
