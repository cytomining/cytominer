#' Husk data.
#'
#' \code{husk} detects unwanted variation in the sample and removes it from the
#' population.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample that is used by the method to estimate
#'   husking parameters. \code{sample} has same structure as \code{population}.
#'   Typically, \code{sample} corresponds to controls in the experiment.
#' @param remove_outliers optional boolean specifying whether to remove
#'   outliers. Default is \code{TRUE}.
#' @param epsilon optional parameter used in husking to offset
#'   eigenvalues to avoid division by zero. Default is \code{1}.
#' @param remove_signal optional boolean specifying whether to husk the signal
#'   instead of only scaling it down. Default is \code{TRUE}.
#' @param flatten_noise optional boolean specifying whether to flatten the noise
#'   instead of scaling it up. Default is \code{TRUE}. The parameter is ignored
#'   if \code{remove_signal} is \code{FALSE}.
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
#' husk(population, variables, population, epsilon = 1, remove_signal = TRUE)
#' husk(population, variables, population, epsilon = 1e-5, remove_signal = TRUE)
#' husk(population, variables, population, epsilon = 1, remove_signal = FALSE)
#' husk(population, variables, population, epsilon = 1e-5, remove_signal = FALSE)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @importFrom stats cov
#' @export
husk <-
  function(population,
           variables,
           sample,
           remove_outliers = TRUE,
           epsilon = 1e-6,
           remove_signal = TRUE,
           flatten_noise = TRUE) {

    # -------------------------
    # Find and drop outliers
    # -------------------------

    if (remove_outliers) {
      sample <- mark_outlier_rows(
        population = sample,
        variables = variables,
        sample = sample,
        operation = "svd+iqr"
      ) %>%
        dplyr::filter(!is_outlier) %>%
        dplyr::select(-is_outlier)

    }

    # -------------------------
    # Get the sample matrix
    # -------------------------

    X <-
      sample %>%
      dplyr::select(all_of(variables)) %>%
      as.matrix()

    # -------------------------
    # Center and scale the  data
    # -------------------------
    #
    # Note that PCA on zero-centered, unit variance data is equivalent to
    # PCA on the correlation matrix

    X <- scale(X, center = TRUE, scale = TRUE)
    d <- ncol(X)
    n <- nrow(X)

    # -------------------------
    # Stop if rank < min(n-1, d)
    # -------------------------
    #
    # For matrices with n <=d, the maximum rank is n-1 because of mean centering
    # For matrices with n > d, the maximum rank is d
    #
    # TODO:
    #   - Figure out how to handle this edge case

    # compute the rank of the matrix
    r <- qr(X)$rank

    stopifnot((r == n - 1) | (r == d))

    # -------------------------
    # Compute SVD to get full V
    # -------------------------
    #
    # Get the full V, not just row space, because we need its null space as well
    # (for n < d). Note that we don't need U, just S and V.

    xsvd <- svd(X, nu = 0, nv = d)
    V <- xsvd$v

    # -------------------------
    # Get s.d. of PCs from SVD
    # -------------------------
    #
    # Scale the singular value by dividing by sqrt(n-1) to get the s.d.
    # of the corresponding PC
    # Recap:
    # all.equal(
    #   prcomp(X)$sdev,
    #   svd(scale(X, center = TRUE, scale = FALSE))$d / sqrt(n-1)
    # )

    S <- xsvd$d / sqrt(n - 1)

    # -------------------------
    # Extrapolate s.d. for the null space
    # -------------------------

    # Set the s.d. of the vectors of the null space to the smallest s.d.
    # (when n <= d; there is no null space otherwise unless we had a low rank
    # matrix to start with).

    if (n <= d) {
      stopifnot(r == n - 1) # See above ("Stop if rank < min(n-1, d)")
      Sr <- c(S[1:r], rep(S[r], d - r))
    } else {
      Sr <- S
    }

    # -------------------------
    # Regularize
    # -------------------------
    #
    # I'm not entirely sure we need to do this, but no harm in doing
    # so if `epsilon` is very small

    Sr <- Sr + epsilon

    # -------------------------
    # Find number of PCs that comprise the signal
    # -------------------------
    # This is a whole subfield in itself, and there are many ways of doing it
    #
    # For now, we just trim the s.d.'s that are "outliers",
    #
    # TODO:
    #   - Choose a more principled way of doing this, most likely parallel
    #     analysis.
    #   - Ponder whether a faster, conservative lower bound can be obtained:
    #     https://en.wikipedia.org/wiki/Wigner_semicircle_distribution
    #     https://en.wikipedia.org/wiki/Wigner_surmise
    #     http://www.stats.ox.ac.uk/~cucuring/Lecture_4_PCA_RMTX_Finance.pdf
    #       (slide 5, 35 are particularly interesting)
    #   - "Considering Horn’s Parallel Analysis from a Random Matrix Theory
    #     Point of View". doi:10.1007/s11336-016-9515-z
    #   - Also consider this word of caution from
    #     https://cran.r-project.org/web/packages/jackstraw/vignettes/jackstraw.pdf
    #
    #     Determining the number of “statistically significant” PCs is an active
    #     area of research,and defining a number of significant PCs depends on the
    #     data structure and the context. Refer to Anderson (1963), Tracy and
    #     Widom (1996), Johnstone (2001), Leek (2010). We do not advocate the
    #     blind use of parallel analysis to obtain r_hat [the number of PCs to
    #     keep]
    #

    if (remove_signal) {
      q <- find_significant_pcs(S)

      if (flatten_noise) {
        Sr[(q + 1):d] <- Sr[q + 1]
      }

      Sr <- Sr[(q + 1):d]

      V <- V[, (q + 1):d]
    }


    # -------------------------
    # Create the transformation
    # -------------------------

    projt <- V %*% diag(1 / Sr)

    husk_helper <- function(M) {
      scale(M,
            center = attr(X, "scaled:center"),
            scale = attr(X, "scaled:scale")) %*% projt
    }

    # -------------------------
    # Transform the population matrix
    # -------------------------

    population_metadata <-
      population %>%
      dplyr::select(-all_of(variables))

    population_data <-
      population %>%
      dplyr::select(all_of(variables)) %>%
      as.matrix()

    population_data_transformed <-
      husk_helper(population_data) %>%
      as.data.frame()

    # -------------------------
    # Assemble the data frame
    # -------------------------

    husked <-
      dplyr::bind_cols(population_metadata,
                       population_data_transformed)

    husked
  }



#' Find significant PC's given the eigenvalues.
#'
#' \code{find_significant_pcs} finds significant PC's given the eigenvalues.
#'
#' @param S numeric vector with eigenvalues of covariance matrix, sorted in
#'   descending order.
#' @param method optional string specifying method to estimate number of
#'   significant PCs
#' @param n optional integer specifying number of rows in the data matrix.
#'   Default is \code{NULL}
#' @param d optional integer specifying number of columns in the data matrix.
#'   Default is \code{NULL}
#'
#' @return number of significant PCs
#'
#' @export
find_significant_pcs <-
  function(S,
           method = "outlier",
           n = NULL,
           d = NULL) {
    stopifnot(!is.unsorted(-S))

    stopifnot(method %in% c("outlier"))

    f_outlier_threshold <- function(x) {
      stats::quantile(x, .75) + 1.5 * stats::IQR(x)
    }

    if (method == "outlier") {
      q <- which(S ^ 2 < f_outlier_threshold(S ^ 2))[1] - 1

      stopifnot(!is.na(q))

      futile.logger::flog.debug(glue::glue("Outlier-based approach reports {q} PCs with signal."))
    }

    q
  }
