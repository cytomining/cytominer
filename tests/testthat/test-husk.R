context("husk")

test_that("`husk` husks data", {

  # TODO:
  #   - Split into multiple tests
  #

  # Test tall matrix
  n_dim <- 5
  n_points <- 10000

  data <- matrix(rnorm(n_points * n_dim), n_points, n_dim)
  data <-
    data + abs(matrix(rnorm(n_points * n_dim), n_points, n_dim)) * 50

  stopifnot(qr(data)$rank == n_dim)

  data <- as.data.frame(data)

  variables <- names(data)

  population <- data
  sample <- data
  regularization_param <- 1e-10
  husk <- FALSE
  husk_threshold <- 2
  remove_outliers <- FALSE

  husked <- husk(
    population = population,
    variables = variables,
    sample = sample,
    regularization_param = regularization_param,
    husk = husk,
    husk_threshold = husk_threshold,
    remove_outliers = remove_outliers
  )

  husked_cov <-
    husked %>%
    cov() %>%
    as.matrix() %>%
    unname()

  identity_matrix <- diag(rep(1, n_dim))

  expect_equal(
    diag(husked_cov),
    diag(identity_matrix),
    tolerance = 10 ^ -6
  )

})

# test_that("`spherize` uses svd consistently", {
#
#   data <- matrix(rnorm(10 * 100), 10, 100)
#   data <- data + abs(matrix(rnorm(10 * 100), 10, 100)) * 10
#   data <- as.data.frame(data)
#
#   variables <- names(data)
#
#   regularization_param  <- 0
#
#   spherize_df <-
#     spherize(
#       population = data,
#       variables = variables,
#       sample = data,
#       regularization_param = 0
#     )
#
#   spherize_test <- function(data, regularization_param)  {
#
#     sample_mean <- colMeans(sample_data)
#
#     sample_cov <- cov(data)
#
#     n <- nrow()
#
#     # eigen decomposition \Sigma = E * \Lambda * E'
#     eig_decomp <- eigen(data)
#
#     E_t <- t(eig_decomp$vectors)
#
#     Lambda <- eig_decomp$values
#
#     # compute sphering transformation, which is {\Lambda + \epsilon}^.5 x E'
#     W <- diag((Lambda + regularization_param)^-0.5) %*% E_t
#
#     # apply sphering transformation, which is (X - \mu) * W'
#     transformed_data <- sweep(data, 2, sample_mean) %*% t(W)
#
#   }
#
#   # ---------------------------------------------------------------------------
#   # ---------------------------------------------------------------------------
#
#   df <- data.frame(svd = Lambda[1:length(Lambda)-1], eig = xLambda[1:length(Lambda)-1])
#
#   lm(df, formula = svd ~ eig)
#
#   xx <- cor(xtransformed_population_data[,1:(n-1)], transformed_population_data[,1:(n-1)])
#
#   plot(diag(xx))
#
#
#   expect_equal(
#     spherize(
#       population = data,
#       variables = c("x", "y", "z"),
#       sample = data,
#       regularization_param = 0
#     )
#   )
#
#
# })
