utils::globalVariables(c("strata_col_dummy"))
#' Mark outlier rows.
#'
#' \code{mark_outlier_rows} drops outlier rows.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample that is used by outlier removal methods
#'   to estimate parameters. \code{sample} has same structure as
#'   \code{population}. Typically, \code{sample} corresponds to controls in the
#'   experiment.
#' @param operation optional character string specifying method for outlier
#'   removal. There is currently only one option (\code{"svd_iqr"}).
#' @param outlier_col optional character string specifying the name for the
#'   column that will indicate outliers (in the output).
#'   Default \code{"is_outlier"}.
#' @param ... arguments passed to outlier removal operation.
#'
#' @return \code{population} with an extra column \code{is_outlier}.
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' suppressMessages(suppressWarnings(library(magrittr)))
#' population <- tibble::tibble(
#'   Metadata_group = sample(c("a", "b"), 100, replace = TRUE),
#'   Metadata_type = sample(c("control", "trt"), 100, replace = TRUE),
#'   AreaShape_Area = c(rnorm(98), 20, 30),
#'   AreaShape_Eccentricity = rnorm(100)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_Eccentricity")
#' sample <- population %>% dplyr::filter(Metadata_type == "control")
#' population_marked <-
#'   cytominer::mark_outlier_rows(
#'     population,
#'     variables,
#'     sample,
#'     operation = "svd+iqr"
#'   )
#' population_marked %>%
#'   dplyr::group_by(is_outlier) %>%
#'   dplyr::sample_n(3)
#' @export
mark_outlier_rows <-
  function(population,
           variables,
           sample,
           operation = "svd+iqr",
           outlier_col = "is_outlier",
           ...) {
    stopifnot(operation == "svd+iqr")

    if (operation == "svd+iqr") {
      get_whiskers <- function(x) {
        grDevices::boxplot.stats(x, do.conf = FALSE, do.out = FALSE)$stats[c(1, 5)]
      }

      # ------------------------------
      # Learn model to detect outliers
      # ------------------------------
      sample_vars <-
        sample %>% dplyr::select(all_of(variables))

      X <- as.matrix(stats::na.omit(sample_vars))
      X <- scale(X, center = TRUE, scale = TRUE)
      xsvd <- svd(X, nu = 2, nv = 2)
      V <- xsvd$v[, 1:2]
      U <- xsvd$u[, 1:2]
      Si <- diag(1 / xsvd$d[1:2])

      Uw <- cbind(
        get_whiskers(U[, 1]),
        get_whiskers(U[, 2])
      )

      X_center <- attr(X, "scaled:center")
      X_scale <- attr(X, "scaled:scale")

      # ------------------------------
      # Apply model
      # ------------------------------

      population_vars <-
        population %>% dplyr::select(all_of(variables))

      Y <- as.matrix(population_vars)

      Y <- scale(Y, center = X_center, scale = X_scale)

      Uc <- Y %*% V %*% Si

      Uout <-
        Uc[, 1] < Uw[1, 1] |
          Uc[, 1] > Uw[2, 1] |
          Uc[, 2] < Uw[1, 2] |
          Uc[, 2] > Uw[2, 2]

      population[[outlier_col]] <- Uout

      population
    } else {
      error <-
        paste0("undefined operation '", operation, "'")

      stop(error)
    }
  }
