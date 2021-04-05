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
#' @param husk optional boolean specifying whether to fully husk the signal.
#'   Default is \code{TRUE}.
#' @param husk_threshold optional parameter for setting the s.d. threshold above
#'   which the dimension is husked. Default is \code{1}.
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
#' husk(population, variables, population, 1, husk = TRUE)
#' husk(population, variables, population, 1e-5, husk = TRUE)
#' husk(population, variables, population, 1, husk = FALSE)
#' husk(population, variables, population, 1e-5, husk = FALSE)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @importFrom stats cov
#' @export
husk <-
  function(population,
           variables,
           sample,
           regularization_param = 1,
           husk = TRUE,
           husk_threshold = 1,
           remove_outliers = TRUE) {
    # -------------------------
    # Step : Get the sample matrix
    # -------------------------

    X0 <-
      sample %>%
      dplyr::select(all_of(variables)) %>%
      as.matrix()

    # -------------------------
    # Step : Find and drop outliers
    # -------------------------
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
    # Step : Center and scale the cleaned data
    # -------------------------

    X <- scale(X, center = TRUE, scale = TRUE)
    d <- ncol(X)
    n <- nrow(X)

    # -------------------------
    # Step : Stop if rank < min(n-1, d)
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
    # Step : Compute SVD to get full V
    # -------------------------
    # Get the full V, not just row space, because we need its null space as well
    # (for n < d).
    # Note that we don't need U, just S and V.
    # Scale the singular value by dividing by sqrt(n-1) to directly get the s.d.
    # of the corresponding PC
    # TODO:
    #   - Consider faster implementations but note that we do need the full V.

    xsvd <- svd(X, nu = 0, nv = d)
    V <- xsvd$v
    S <-
      xsvd$d / sqrt(n - 1) # divide by sqrt(n-1) so as to get the s.d.

    # Note: going forward, we refer to S as the s.d.'s of the PC's (because we
    # have scaled it to be so)

    # -------------------------
    # Step : Get projection matrix
    # TODO:
    #   - Ponder the rationale overall
    # -------------------------

    # Find the first scaled singular value (s.d.) that is less than 1.
    # We will later (optionally) do one or both of the following
    #   - set the s.d. to 1 for all PCs with s.d. < 1
    #   - drop all PCs that have s.d.'s > 1
    # TODO:
    #   - Ponder the rationale of choosing 1. This might be flawed!
    #
    #     The rationale is that when PCA is performed on X with unit s.d., (or,
    #     equivalently, when PCA is performed on the correlation matrix instead
    #     of the covariance matrix), we assume that PCs with > 1 s.d. definitely
    #     contain signal (and should thus be (optionally) dropped, even if they
    #     are scaled down to unit s.d.). We should husk the signal, and keep
    #     the (whitened) noise.

    # Note q is NA if *no* s.d. is less than 1
    q <- which(S < 1)[1]
    futile.logger::flog.debug(glue::glue("{qx} PCs have s.d. > 1",
      qx = ifelse(is.na(q), 0, q)
    ))

    if (regularization_param > 0) {
      # - Set the s.d. of the vectors of the null space to the smallest s.d.
      #   (when n <= d; there is no null space otherwise)
      # - Add a regularizer
      # TODO :
      #   - Ponder the rationale for padding

      if (n <= d) {
        Sr <- c(S[1:r], rep(S[r], d - r))
      } else {
        Sr <- S
      }

      Sr <- Sr + regularization_param
    } else {
      # - Set the s.d. to 1 for all the basis vectors of the null
      #   (when n <= d; there is no null space otherwise)
      # - Set the s.d. to 1 for all PCs with s.d. < 1
      # - Do not add a regularizer

      if (!is.na(q)) {
        if (n <= d) {
          Sr <- c(S[1:(q - 1)], rep(1, d - q + 1))
        } else {
          Sr <- S
          Sr[q:d] <- 1
        }
      }
      # ---------------
    }

    proj <- diag(1 / Sr) %*% t(V)

    # -------------------------
    # Step : Optionally trim the projection matrix
    # -------------------------
    # Rationale: PCs with s.d. > 1 definitely have signal; we should remove them
    # TODO:
    #   - Verify this rationale is sensible (see discussion above)

    if (husk) {
      proj <- proj[q:d, ] # husk the signal, keep the white noise
    }

    # -------------------------
    # Step : Create the transformation
    # -------------------------

    husk_helper <- function(M) {
      scale(M,
        center = attr(X, "scaled:center"),
        scale = attr(X, "scaled:scale")
      ) %*% t(proj)
    }

    # -------------------------
    # Step : Transform the population matrix
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

    husked <-
      dplyr::bind_cols(
        population_metadata,
        population_data_transformed
      )

    husked
  }
