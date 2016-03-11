#' Select features using correlation threshold
#'
#' @param population ...
#' @param variables ...
#' @param sample ...
#' @param cutoff ...
#' @param method ...
#'
#' @return Excluded variables
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
correlation_threshold <- function(population, variables, sample, cutoff = 0.90,
                                  method = "pearson") {

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    findCorrelation(cutoff = cutoff)

  variables[excluded_indexes]
}
