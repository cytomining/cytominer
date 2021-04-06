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
#' @param regularization_param optional parameter used in husking to offset
#'   eigenvalues to avoid division by zero. Default is \code{1}.
#' @param remove_signal optional boolean specifying whether to husk the signal
#'   instead of only scaling it down. Default is \code{TRUE}.
#' @param remove_outliers optional boolean specifying whether to remove
#'   outliers. Default is \code{TRUE}.
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
#' husk(population, variables, population, 1, remove_signal = TRUE)
#' husk(population, variables, population, 1e-5, remove_signal = TRUE)
#' husk(population, variables, population, 1, remove_signal = FALSE)
#' husk(population, variables, population, 1e-5, remove_signal = FALSE)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @importFrom stats cov
#' @export
husk <-
  function(population,
           variables,
           sample,
           regularization_param = 1e-6,
           remove_signal = TRUE,
           remove_outliers = TRUE) {
    # -------------------------
    # Get the sample matrix
    # -------------------------

    X0 <-
      sample %>%
      dplyr::select(all_of(variables)) %>%
      as.matrix()

    # -------------------------
    # Find and drop outliers
    # -------------------------
    #
    # Find outliers in the top 2 PCs, using > 1.5IQR rule
    # TODO:
    #   - Ponder shortcomings and improvements

    if (remove_outliers) {
      X <- scale(X0, center = TRUE, scale = FALSE)
      xsvd <- svd(X, nu = 2, nv = 0)
      u1 <- xsvd$u[, 1]
      u2 <- xsvd$u[, 2]
      u1out <- boxplot(u1, plot = FALSE)$out
      u2out <- boxplot(u2, plot = FALSE)$out
      uout <- c(
        which(u1 %in% u1out),
        which(u2 %in% u2out)
      )
      n_outliers <- length(uout)

      X <- X0
      if (n_outliers > 0) {
        X <- X0[-uout, ]
      }
    } else {
      X <- X0
    }

    # -------------------------
    # Center and scale the cleaned data
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
    # For wide matrices, the maximum rank is n-1 because of mean centering
    # For tall matrices, the maximum rank is d
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
    # Get projection matrix
    # -------------------------

    # Set the s.d. of the vectors of the null space to the smallest s.d.
    # (when n <= d; there is no null space otherwise), and then add a
    # regularizer

    if (n <= d) {
      Sr <- c(S[1:r], rep(S[r], d - r))
    } else {
      Sr <- S
    }

    # Regularize
    Sr <- Sr + regularization_param

    # (See comments at the end of the function regularization alternatives)

    proj <- diag(1 / Sr) %*% t(V)

    # -------------------------
    # Find number of PCs to retain to keep the signal
    # (but we actually later want to throw them out, not keep them)
    # -------------------------
    # This is a whole subfield in itself, and there are many ways of doing it.
    # But we can take a lazy approach for now and just trim the s.d. that are
    # "outliers"

    outlier_s <- boxplot(S^2, plot = FALSE)$out

    if(length(outlier_s) == 0) {
      q <- 0
    } else {
      q <- which(S^2 < min(outlier_s))[1]
    }

    futile.logger::flog.info(glue::glue("There are {q} PCs with signal."))

    # -------------------------
    # Optionally trim the projection matrix
    # -------------------------
    #
    # Rationale: PCs with signal should be removed

    if (remove_signal) {
      proj <- proj[(q+1):d, ] # husk the signal, keep the white noise
    }

    # -------------------------
    # Create the transformation
    # -------------------------

    husk_helper <- function(M) {
      scale(M,
        center = attr(X, "scaled:center"),
        scale = attr(X, "scaled:scale")
      ) %*% t(proj)
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
      dplyr::bind_cols(
        population_metadata,
        population_data_transformed
      )

    husked
  }

# I considered this alternative to regularization but abandoned it:
# - Set the s.d. to `sqrt(husk_threshold)` for all the basis vectors of
#   the null (when n <= d; there is no null space otherwise)
# - Set the s.d. to `sqrt(husk_threshold)` for all PCs with s.d. < 1
# - Do not add a regularizer

#  if (!is.na(q)) {
#    if (n <= d) {
#      Sr <- c(S[1:(q - 1)], rep(1, d - q + 1))
#    } else {
#      Sr <- S
#      Sr[q:d] <- sqrt(husk_threshold)
#    }
#  }
