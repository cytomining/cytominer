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
normalize <- function(population, variables, grouping_variables, sample,
                      operation = "standardize", ...) {

  worker <- function(group) {

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

  groups <-
    sample %>%
    dplyr::select_(.dots = grouping_variables) %>%
    dplyr::distinct() %>%
    dplyr::collect()

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

  Reduce(dplyr::union,
         Map(worker, split(groups, seq(nrow(groups))))
  )

}

