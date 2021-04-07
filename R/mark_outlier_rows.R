utils::globalVariables(c("strata_col_dummy"))
#' Mark outlier rows.
#'
#' \code{mark_outlier_rows} drops outlier rows.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata optional character vector specifying grouping variables for
#'   grouping prior to outlier removal. If \code{NULL}, no stratification is
#'   performed.
#' @param operation optional character string specifying method for outlier
#'   removal. There is currently only one option (\code{"svd_iqr"}).
#' @param sample tbl containing sample that is used by outlier removal methods
#'   to estimate parameters. \code{sample} has same structure as
#'   \code{population}. Typically, \code{sample} corresponds to controls in the
#'   experiment.
#' @param outlier_col optional character string specifying the name for the
#'   column that will indicate outliers (in the output).
#'   Default \code{"is_outlier"}.
#' @param ... arguments passed to outlier removal operation.
#'
#' @return \code{population} with an extra column \code{is_outlier}.
#'
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
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
#' strata <- c("Metadata_group")
#' sample <- population %>% dplyr::filter(Metadata_type == "control")
#' population_marked <-
#'   cytominer::mark_outlier_rows(
#'     population,
#'     variables,
#'     sample,
#'     strata,
#'     operation = "svd+iqr"
#'   )
#' population_marked %>%
#'   group_by(is_outlier) %>%
#'   sample_n(3)
#' @export
mark_outlier_rows <- function(population,
                              variables,
                              sample,
                              strata = NULL,
                              operation = "svd+iqr",
                              outlier_col = "is_outlier",
                              ...) {
  stopifnot(operation == "svd+iqr")

  if (is.null(strata)) {
    population$strata_col_dummy <- 1
    sample$strata_col_dummy <- 1
    strata <- c("strata_col_dummy")
  }

  get_outlier_detector <- function(df) {
    dfv <- df %>% dplyr::select(all_of(variables))

    if (operation == "svd+iqr") {
      get_whiskers <- function(x) {
        grDevices::boxplot.stats(x, do.conf = FALSE, do.out = FALSE)$stats[c(1, 5)]
      }

      X0 <- as.matrix(stats::na.omit(dfv))
      X <- scale(X0, center = TRUE, scale = TRUE)
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
      df_names <- colnames(dfv)

      outlier_detector <- function(df) {
        dfv <- df %>% dplyr::select(all_of(variables))

        stopifnot(df_names == colnames(dfv))

        M <- as.matrix(dfv)

        Ms <- scale(M, center = X_center, scale = X_scale)

        Uc <- Ms %*% V %*% Si

        Uout <-
          Uc[, 1] < Uw[1, 1] |
            Uc[, 1] > Uw[2, 1] |
            Uc[, 2] < Uw[1, 2] |
            Uc[, 2] > Uw[2, 2]

        df[[outlier_col]] <- Uout

        df
      }

      outlier_detector
    } else {
      error <-
        paste0("undefined operation '", operation, "'")

      stop(error)
    }
  }

  groups <-
    sample %>%
    dplyr::select(all_of(strata)) %>%
    dplyr::distinct()

  cleaned <-
    Reduce(
      dplyr::union_all,
      Map(
        f = function(group) {
          futile.logger::flog.debug(group)
          futile.logger::flog.debug("\tstratum")
          stratum <-
            sample %>%
            dplyr::inner_join(
              y = group,
              by = names(group),
              copy = TRUE
            )

          futile.logger::flog.debug("\toutlier_stats")
          outlier_detector <-
            stratum %>%
            dplyr::select(all_of(variables)) %>%
            get_outlier_detector()

          futile.logger::flog.debug("\tremove_outliers")
          cleaned <-
            population %>%
            dplyr::inner_join(
              y = group,
              by = names(group),
              copy = TRUE
            ) %>%
            outlier_detector()
          futile.logger::flog.debug("\tcleaned")

          cleaned
        },
        split(x = groups, f = seq(nrow(groups)))
      )
    )

  if ("strata_col_dummy" %in% cleaned) {
    cleaned <- cleaned %>% dplyr::select(-strata_col_dummy)
  }

  cleaned
}
