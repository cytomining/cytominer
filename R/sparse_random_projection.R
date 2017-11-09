sparse_random_projection <- function(population, variables, n_components) {
  population %<>%
    dplyr::collect()

  # Extract variables (columns) from population (data frame) and convert to a matrix
  population_data <- population %>%
    dplyr::select(dplyr::one_of(variables)) %>%
    as.matrix()

  # Generate the sparse component matrix
  n_features <- dim(population_data)[-1]
  component_matrix <- generate_component_matrix(
    n_features = n_features,
    n_components = n_components,
    density = 1.0 / sqrt(n_features)
  )

  # Compute the projection
  projected_population_data <- population_data %*% component_matrix %>%
    as.matrix()

  # Coerce the result into a data frame
  colnames(projected_population_data) <- paste0("R", 1:NCOL(projected_population_data))

  projected_population_data %<>% as.data.frame()

  projected_population <-
    dplyr::bind_cols(
      list(
        population %>% dplyr::select(-dplyr::one_of(variables)),
        projected_population_data
      )
    )

  projected_population
}

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
