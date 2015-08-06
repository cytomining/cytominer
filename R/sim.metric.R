#' Constructor for sim.metric S3 class
#'
#' @param sim_metric_df data.frame describing the similarity metric
#'
#' @return sim.metric object
#'

sim.metric <- function(sim_metric_df) {
  testthat::expect_true("name" %in% names(sim_metric_df))
  class(sim_metric_df) <- "sim.metric"
  sim_metric_df
}

#' Format a sim.metric object
#'
#' @param s sim.metric object
#' @param ... other arguments
format.sim.metric <- function(s, ...) stringr::str_c("sim", s$name, sep = "_")
