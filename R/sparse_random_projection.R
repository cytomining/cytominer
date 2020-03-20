#' Reduce the dimensionality of a population using sparse random projection.
#'
#' \code{sparse_random_projection} reduces the dimensionality of a population by projecting
#' the original data with a sparse random matrix. Generally more efficient and faster to
#' compute than a Gaussian random projection matrix, while providing similar embedding quality.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param n_components size of the projected feature space.
#'
#' @return Dimensionality reduced \code{population}.
#'
#' @examples
#' population <- tibble::tibble(
#'   Metadata_Well = c("A01", "A02", "B01", "B02"),
#'   AreaShape_Area_DNA = c(10, 12, 7, 7),
#'   AreaShape_Length_DNA = c(2, 3, 1, 5),
#'   Intensity_DNA = c(8, 20, 12, 32),
#'   Texture_DNA = c(5, 2, 43, 13)
#' )
#' variables <- c("AreaShape_Area_DNA", "AreaShape_Length_DNA", "Intensity_DNA", "Texture_DNA")
#' sparse_random_projection(population, variables, 2)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
sparse_random_projection <- function(population, variables, n_components) {
  population %<>%
    dplyr::collect()

  # Extract variables (columns) from population (data frame)
  # and convert to a matrix
  population_data <- population %>%
    dplyr::select(variables) %>%
    as.matrix()

  # Generate the sparse component matrix
  n_features <- ncol(population_data)
  component_matrix <- generate_component_matrix(
    n_features = n_features,
    n_components = n_components,
    density = 1.0 / sqrt(n_features)
  )

  # Compute the projection
  projected_population_data <- population_data %*% component_matrix %>%
    as.matrix()

  # Coerce the result into a data frame
  colnames(projected_population_data) <- paste0(
    "R",
    1:NCOL(projected_population_data)
  )

  projected_population_data %<>% as.data.frame()

  projected_population <-
    dplyr::bind_cols(
      population %>% dplyr::select(-dplyr::one_of(variables)),
      projected_population_data
    )

  projected_population
}

#' A sparse matrix for sparse random projection.
#'
#' \code{generate_component_matrix} generates the sparse random component matrix
#' for performing sparse random projection. If \code{density} is the density of
#' the sparse matrix and \code{n_components} is the size of the projected space,
#' the elements of the random matrix are drawn from
#'
#'     \code{-sqrt(1 / (density * n_components))} with probability \code{density / 2}
#'     \code{0}                                   with probability \code{1 - density}
#'     \code{sqrt(1 / (density * n_components))}  with probability \code{density / 2}
#'
#' @param n_features the dimensionality of the original space.
#' @param n_components the dimensionality of the projected space.
#' @param density the density of the sparse random matrix.
#'
#' @return A sparse random matrix of size \code{(n_features, n_components)}.
#'
#' @examples
#' generate_component_matrix(500, 100, 0.3)
#' @importFrom Matrix sparseMatrix
#' @importFrom stats rbinom
#' @export
generate_component_matrix <- function(n_features, n_components, density) {
  # Generate nonzero elements - follows the binomial distribution:
  #
  #   1 (nonzero) with probability density
  #   0           with probability 1 - density
  #
  # (Requires n_features * n_components space)
  nonzero_elements <- rbinom(n_features * n_components, 1, density)
  indices <- which(nonzero_elements != 0)

  # Generate sign of the nonzero elements - the probability of
  # positive or negative sign is equal (0.5).
  signs <- 2 * rbinom(length(indices), 1, 0.5) - 1

  # Compute the value of the nonzero elements
  k <- sqrt(1.0 / (density * n_components))

  # Generate the component matrix
  component_matrix <- sparseMatrix(
    i = (indices - 1) %% n_features + 1,
    j = (indices - 1) %/% n_features + 1,
    x = k * signs,
    dims = c(n_features, n_components)
  )

  component_matrix
}
