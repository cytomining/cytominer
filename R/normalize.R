#' Normalize data
#'
#' @param population ...
#' @param variables ...
#' @param strata ...
#' @param sample ...
#' @param operation ...
#' @param ... arguments passed to normalization operation
#'
#' @return normalized data
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
normalize <- function(population, variables, strata, sample, operation = "standardize", ...) {
  if (operation == "robustize") {
    location <- dplyr::funs(median)

    dispersion <- dplyr::funs(mad)
  } else if (operation == "standardize") {
    location <- dplyr::funs(mean)

    dispersion <- dplyr::funs(sd)
  } else {
    error <- paste("undefined operation `", operation, "'", sep="")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  groups <-
    sample %>%
    dplyr::select_(.dots = strata) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  Reduce(
    dplyr::union,
    Map(
      f = function(group) {
        stratum <-
          sample %>%
          dplyr::inner_join(y = group, by = names(group), copy = TRUE)

        location <-
          stratum %>%
          dplyr::summarise_each_(funs = location, vars = variables) %>%
          dplyr::collect()

        dispersion <-
          stratum %>%
          dplyr::summarise_each_(funs = dispersion, vars = variables) %>%
          dplyr::collect()

        population %>%
          dplyr::inner_join(y = group, by = names(group), copy = TRUE) %>%
          scale_dplyr(center = location, scale = dispersion, vars = variables)
        },
      split(x = groups, f = seq(nrow(groups)))
    )
  )
}
