context("husk")

test_that("`husk` husks tall data", {
  # TODO: split into smaller tests

  n_dim <- 5
  n_points <- 1000

  set.seed(50)
  data <- matrix(rnorm(n_points * n_dim), n_points, n_dim)
  data <-
    data + abs(matrix(rnorm(n_points * n_dim), n_points, n_dim)) * 50

  stopifnot(qr(data)$rank == n_dim)

  data <- as.data.frame(data)

  variables <- names(data)

  # ------------------------
  population <- data
  sample <- data
  remove_outliers <- FALSE
  epsilon <- 1e-10
  remove_signal <- FALSE

  futile.logger::flog.threshold(futile.logger::DEBUG)
  husked <- husk(
    population = population,
    variables = variables,
    sample = sample,
    remove_outliers = remove_outliers,
    epsilon = epsilon,
    remove_signal = remove_signal
  )
  futile.logger::flog.threshold(futile.logger::WARN)

  husked_cov <-
    husked %>%
    cov() %>%
    as.matrix() %>%
    unname()

  identity_matrix <- diag(rep(1, n_dim))

  expect_equal(
    diag(husked_cov),
    diag(identity_matrix),
    tolerance = 10^-6
  )
  # ------------------------
  population <- data
  sample <- data
  remove_outliers <- TRUE
  epsilon <- 1e-10
  remove_signal <- TRUE
  flatten_noise <- TRUE

  futile.logger::flog.threshold(futile.logger::DEBUG)
  husked <- husk(
    population = population,
    variables = variables,
    sample = sample,
    remove_outliers = remove_outliers,
    epsilon = epsilon,
    remove_signal = remove_signal,
    flatten_noise = flatten_noise
  )
  futile.logger::flog.threshold(futile.logger::WARN)

  husked_cov <-
    husked %>%
    cov() %>%
    as.matrix() %>%
    unname()

  identity_matrix <- diag(rep(1, n_dim))

  # It should be almost identity
  # TODO:
  #   - This test can sometimes fail because of outlier removal and noise
  #     flattening. Come up with a better test.
  expect_equal(
    diag(husked_cov),
    diag(identity_matrix),
    tolerance = .1
  )
  # ------------------------
})


test_that("`husk` husks wide data", {
  n_dim <- 100
  n_points <- 10

  set.seed(42)
  data <- matrix(rnorm(n_points * n_dim), n_points, n_dim)
  data <-
    data + abs(matrix(rnorm(n_points * n_dim), n_points, n_dim)) * 50

  stopifnot(qr(data)$rank == n_points)

  data <- as.data.frame(data)

  variables <- names(data)

  population <- data
  sample <- data
  epsilon <- 1e-10
  remove_signal <- FALSE
  remove_outliers <- FALSE

  husked <- husk(
    population = population,
    variables = variables,
    sample = sample,
    remove_outliers = remove_outliers,
    epsilon = epsilon,
    remove_signal = remove_signal
  )

  husked_cov <-
    husked %>%
    cov() %>%
    as.matrix() %>%
    unname()

  expect_equal(
    diag(husked_cov)[1:(n_points - 1)],
    rep(1, n_points - 1),
    tolerance = 10^-6
  )

  expect_equal(
    diag(husked_cov)[n_points:n_dim],
    rep(0, n_dim - n_points + 1),
    tolerance = 10^-6
  )
})

