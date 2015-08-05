context("Test similarity metric class")

test_that("sim.metric works as expected", {
  sim_metric <- sim.metric(data.frame(name = "test"))
  expect_equal(format(sim_metric), "sim_test")

})
