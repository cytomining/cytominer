#' Normalize rows
#'
#' @param population population
#' @param variables variables
#' @param grouping_variables grouping_variables
#' @param sample sample
#' @param operation operation
#' @param ... Arguments to be passed to methods
#'
#' @return object after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize <- function(population, variables, grouping_variables, sample, operation = "standardize", ...) {

  if (operation == "linearize") {
    stop("Not implemented")
  } else if (operation == "robustize") {
    centering_function <- dplyr::funs(median)
    scaling_function <- dplyr::funs(mad)
  } else if (operation == "standardize") {
    centering_function <- dplyr::funs(mean)
    scaling_function <- dplyr::funs(sd)
  } else {
    stop("unknown operation")
  }

  groups_sql <-
    sample %>%
    dplyr::select_(.dots = grouping_variables) %>%
    dplyr::distinct()

  groups <- groups_sql %>%
    dplyr::collect()

  normalize_helper_1 <- function(group) {
    normalize_helper(group = group,
                     population = population,
                     variables = variables,
                     sample = sample,
                     centering_function = centering_function,
                     scaling_function = scaling_function
    )

  }
  Reduce(dplyr::union,
         Map(normalize_helper_1, split(groups, seq(nrow(groups))))
    )

}

#' Normalize helper
#'
#' @param group group
#' @param population population
#' @param variables variables
#' @param sample sample
#' @param centering_function centering_function
#' @param scaling_function scaling_function
#'
#' @return object after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize_helper <- function(group, population, variables, sample, centering_function, scaling_function) {

  sample_group <-
    sample %>%
    dplyr::inner_join(group, by = names(group), copy = TRUE)

  center <-
    sample_group %>%
    dplyr::summarise_each_(centering_function, vars = variables) %>%
    dplyr::collect()

  scale <-
    sample_group %>%
    dplyr::summarise_each_(scaling_function, vars = variables) %>%
    dplyr::collect()

  population %>%
    dplyr::inner_join(group, by = names(group), copy = TRUE) %>%
    scale_dplyr(center = center, scale = scale, vars = variables)
}
