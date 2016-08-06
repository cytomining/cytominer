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
#' @export
normalize <- function(population, variables, strata, sample, operation = "standardize", ...) {
  scale <- function(data, location, dispersion, variables) {
    for (variable in variables) {
      object <- list(lazyeval::interp(~ (x - m) / s, x = as.name(variable), m = location[[variable]], s = dispersion[[variable]]))

      name <- paste0(variable, "_")

      data %<>%
        dplyr::mutate_(.dots = setNames(object = object, nm = name))
    }

    data %>%
      dplyr::select_(~-dplyr::one_of(variables))  %>%
      dplyr::rename_(.dots = setNames(paste0(variables, "_"), variables))
  }

  if (operation == "robustize") {
    location <- dplyr::funs(median)

    dispersion <- dplyr::funs(mad)
  } else if (operation == "standardize") {
    location <- dplyr::funs(mean)

    dispersion <- dplyr::funs(sd)
  } else {
    error <- paste0("undefined operation `", operation, "'")

    futile.logger::flog.error(msg = error)

    stop(error)
  }

  futile.logger::flog.debug("Creating temp table for sample")
  sample %<>% dplyr::compute()
  futile.logger::flog.debug("Created temp table for sample")

  groups <-
    sample %>%
    dplyr::select_(.dots = strata) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  Reduce(
    dplyr::union,
    Map(
      f = function(group) {
        futile.logger::flog.debug(group)
        futile.logger::flog.debug("\tstratum")
        stratum <-
          sample %>%
          dplyr::inner_join(y = group, by = names(group), copy = TRUE) %>%
          dplyr::compute()

        futile.logger::flog.debug("\tlocation")
        location <-
          stratum %>%
          dplyr::summarise_each_(funs = location, vars = variables) %>%
          dplyr::collect()

        futile.logger::flog.debug("\tdispersion")
        dispersion <-
          stratum %>%
          dplyr::summarise_each_(funs = dispersion, vars = variables) %>%
          dplyr::collect()

        futile.logger::flog.debug("\tscale")
        scaled <-
          population %>%
          dplyr::inner_join(y = group, by = names(group), copy = TRUE) %>%
          scale(location, dispersion, variables)
        futile.logger::flog.debug("\tscaled")
        scaled
      },
      split(x = groups, f = seq(nrow(groups)))
    )
  )
}
