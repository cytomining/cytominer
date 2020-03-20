#' Whiten data.
#'
#' \code{whiten} transforms specified observation variables by estimating a whitening transformation on a sample and applying it to the population.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample that is used by the method to estimate whitening parameters. \code{sample} has same structure as \code{population}. Typically, \code{sample} corresponds to controls in the experiment.
#' @param regularization_param optional parameter used in whitening to offset eigenvalues to avoid division by zero.
#'
#' @return transformed data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_Well = c("A01", "A02", "B01", "B02"),
#'   Intensity_DNA = c(8, 20, 12, 32),
#'   Texture_DNA = c(5, 2, 43, 13)
#' )
#' variables <- c("Intensity_DNA", "Texture_DNA")
#' whiten(population, variables, population, 0.01)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @importFrom stats cov
#' @export
whiten <- function(population, variables, sample, regularization_param = 1) {
  sample %<>%
    dplyr::collect()

  sample_data <- sample %>%
    dplyr::select(variables) %>%
    as.matrix()

  population %<>%
    dplyr::collect()

  population_data <- population %>%
    dplyr::select(variables) %>%
    as.matrix()

  # mean of sample
  sample_mean <- colMeans(sample_data)

  # covariance of sample
  sample_cov <- cov(sample_data)

  # eigen decomposition \Sigma = E * \Lambda * E'
  eig_decomp <- eigen(sample_cov)

  # compute whitening transformation, which is {\Lambda + \epsilon}^.5 x E'
  W <- diag((eig_decomp$values + regularization_param)^-0.5) %*%
    t(eig_decomp$vectors)

  # apply whitening transformation, which is (X - \mu) * W'
  transformed_population_data <- sweep(population_data, 2, sample_mean) %*% t(W)

  colnames(transformed_population_data) <- paste0("PC", 1:NCOL(W))

  transformed_population_data %<>% as.data.frame()

  transformed_population <-
    dplyr::bind_cols(
      list(
        population %>% dplyr::select(-dplyr::one_of(variables)),
        transformed_population_data
      )
    )

  transformed_population
}
