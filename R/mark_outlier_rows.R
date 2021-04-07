#' Mark outlier rows.
#'
#' \code{mark_outlier_rows} drops outlier rows.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param strata character vector specifying grouping variables for grouping
#'   prior to outlier removal.
#' @param operation optional character string specifying method for outlier
#'   removal. There is currently only one option (\code{"svd_iqr"}).
#' @param sample tbl containing sample that is used by outlier removal methods
#'   to estimate parameters. \code{sample} has same structure as
#'   \code{population}. Typically, \code{sample} corresponds to controls in the
#'   experiment.
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
#'   Metadata_group = c(
#'     "control", "control", "control", "control",
#'     "experiment", "experiment", "experiment", "experiment"
#'   ),
#'   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
#'   AreaShape_Area = c(100, 12, 15, 16, 8, 8, 7, 7),
#'   AreaShape_Eccentricity = c(1, 5, 2, 1, 3, 8, 70, 4)
#' )
#' variables <- c("AreaShape_Area", "AreaShape_Eccentricity")
#' strata <- c("Metadata_batch")
#' sample <- population %>% dplyr::filter(Metadata_group == "control")
#' cytominer::mark_outlier_rows(population, variables, strata,
#'                              sample, operation = "svd_iqr")
#' @export
mark_outlier_rows <- function(population,
                              variables,
                              strata,
                              sample,
                              operation = "svd+iqr",
                              ...) {
  stopifnot(operation == "svd+iqr")

  get_outlier_detector <- function(df) {
    dfv <- df %>% dplyr::select(all_of(variables))

    if (operation == "svd+iqr") {
      get_whiskers <- function(x) {
        grDevices::boxplot.stats(x, do.conf = FALSE, do.out = FALSE)$stats[c(1, 5)]
      }

      X0 <- as.matrix(na.omit(dfv))
      X <- scale(X0, center = TRUE, scale = TRUE)
      xsvd <- svd(X, nu = 2, nv = 2)
      V <- xsvd$v[, 1:2]
      U <- xsvd$u[, 1:2]
      Si <- diag(1 / xsvd$d[1:2])

      Uw <- cbind(get_whiskers(U[, 1]),
                  get_whiskers(U[, 2]))
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

        df$is_outlier <- Uout

        df
      }

      outlier_detector

    } else {
      error <-
        paste0("undefined operation space '", operation_space, "'")

      stop(error)
    }

  }

  mark_outliers <- function(df, outlier_detector) {
    if (operation_trimmer == "iqr") {
      X <- scale(X0, center = TRUE, scale = FALSE)
      xsvd <- svd(X, nu = 2, nv = 0)
      u1 <- xsvd$u[, 1]
      u2 <- xsvd$u[, 2]
      u1out <- graphics::boxplot(u1, plot = FALSE)$out
      u2out <- graphics::boxplot(u2, plot = FALSE)$out
      uout <- c(which(u1 %in% u1out),
                which(u2 %in% u2out))

    } else {
      error <-
        paste0("undefined operation trimmer '", operation_trimmer, "'")

      stop(error)
    }
  }

  groups <-
    sample %>%
    dplyr::select(all_of(strata)) %>%
    dplyr::distinct()

  Reduce(dplyr::union_all,
         Map(
           f = function(group) {
             futile.logger::flog.debug(group)
             futile.logger::flog.debug("\tstratum")
             stratum <-
               sample %>%
               dplyr::inner_join(y = group,
                                 by = names(group),
                                 copy = TRUE)

             futile.logger::flog.debug("\toutlier_stats")
             outlier_detector <-
               stratum %>%
               dplyr::select(all_of(variables)) %>%
               get_outlier_detector()

             futile.logger::flog.debug("\tremove_outliers")
             cleaned <-
               population %>%
               dplyr::inner_join(y = group,
                                 by = names(group),
                                 copy = TRUE) %>%
               outlier_detector()
             futile.logger::flog.debug("\tcleaned")

             cleaned
           },
           split(x = groups, f = seq(nrow(groups)))
         ))

}
