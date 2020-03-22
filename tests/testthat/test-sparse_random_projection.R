context("sparse_random_projection")

test_that("`generate_component_matrix` contains 3 values", {
  n_features <- 500
  n_components <- 100
  density <- 0.3

  component_matrix <- generate_component_matrix(
    n_features = n_features,
    n_components = n_components,
    density = density
  ) %>%
    as.matrix()

  # Assert there are exactly 3 unique values
  expect_length(unique(c(component_matrix)), 3)

  expected_k <- sqrt(1.0 / (density * n_components))

  # Assert that the values are [-k, 0, k]
  expect_true(-expected_k %in% component_matrix)
  expect_true(0 %in% component_matrix)
  expect_true(expected_k %in% component_matrix)
})

test_that("`generate_component_matrix` samples values from distribution", {
  n_features <- 500
  n_components <- 100
  density <- 0.3

  component_matrix <- generate_component_matrix(
    n_features = n_features,
    n_components = n_components,
    density = density
  ) %>%
    as.matrix()

  # Check the values follow the correct distribution:
  #
  #  -k    with probability density / 2
  #   0    with probability 1 - density
  #   k    with probability density / 2
  n_negative <- length(which(component_matrix < 0))
  n_zero <- length(which(component_matrix == 0))
  n_positive <- length(which(component_matrix > 0))

  p_negative <- n_negative / (n_features * n_components)
  p_zero <- n_zero / (n_features * n_components)
  p_positive <- n_positive / (n_features * n_components)

  expect_equal(p_negative, density / 2, tolerance = 0.005)
  expect_equal(p_zero, 1 - density, tolerance = 0.005)
  expect_equal(p_positive, density / 2, tolerance = 0.005)
})

test_that("`sparse_random_projection` creates a sparse random projection", {
  data <- data.frame(
    id = 1:10,
    a = rnorm(10),
    b = rnorm(10),
    c = rnorm(10),
    x = rnorm(10),
    y = rnorm(10),
    z = rnorm(10)
  )

  db <- DBI::dbConnect(RSQLite::SQLite(),
    ":memory:",
    loadable.extensions = TRUE
  )

  data <- dplyr::copy_to(db, data)

  projected_population <- sparse_random_projection(
    population = data,
    variables = c("a", "b", "c", "y", "z"),
    n_components = 3
  )

  projected_population_data <- projected_population %>%
    dplyr::select(c("R1", "R2", "R3")) %>%
    as.matrix()

  expect_equal(dim(projected_population_data), c(10, 3))

  DBI::dbDisconnect(db)
})
