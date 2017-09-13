context("aggregate")

test_that("`aggregate` aggregates data", {

  data <-
    rbind(
      data.frame(g = "a", x = rnorm(5), y = rnorm(5)),
      data.frame(g = "b", x = rnorm(5), y = rnorm(5))
    )

  db <- dplyr::src_sqlite(":memory:", create = T)
  
  data <- dplyr::copy_to(db, data)

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs = dplyr::funs(median), .vars = c("x", "y"))
  )

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "median") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs =dplyr::funs(median), .vars = c("x", "y"))
  )

  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "mean+sd") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::summarise_at(.funs =c(dplyr::funs(mean), dplyr::funs(sd)), .vars = c("x", "y"))
  )
  
  lower_tri_mat <- function(mat) {
    mat[lower.tri(mat, diag = T)]  
  }
  
  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "cov") %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::do(data.frame(matrix(lower_tri_mat(stats::cov(.[, c("x", "y")])), 
                                  dimnames = list(NULL, c("x__x", "y__x", "y__y")),
                                  nrow = 1))) %>%
      dplyr::ungroup()
  )
  
  random_proj <- matrix(runif(2 * 3), nrow = 3, ncol = 2)
  expect_equal(
    aggregate(population = data,
              variables = c("x", "y"),
              strata = c("g"),
              operation = "cov",
              random_projection = random_proj) %>%
      dplyr::collect(),
    data %>%
      dplyr::group_by(g) %>%
      dplyr::do(data.frame(matrix(lower_tri_mat(stats::cov(.[, c("x", "y")])), 
                                  dimnames = list(NULL, c("x__x", "y__x", "y__y")),
                                  nrow = 1) %*% random_proj)) %>%
      dplyr::ungroup()
  )
})
