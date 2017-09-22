#' Generalized log transform data.
#'
#' \code{generalized_log} transforms specified observation variables using \eqn{x = log( (x + sqrt(x ^ 2 + offset ^ 2 )) / 2 )}.
#'
#' @param population tbl with grouping (metadata) and observation variables.
#' @param variables character vector specifying observation variables.
#' @param sample tbl containing sample that is used by the method to estimate whitening parameters. \code{sample} has same structure as \code{population}. Typically, \code{sample} corresponds to controls in the experiment.
#' @param regularization_param optional parameter used in whitening to offset eigenvalues to avoid division by zero.
#'
#' @return transformed data of the same class as \code{population}.
#'
#' @examples
#' population <- tibble::data_frame(
#'    Metadata_Well = c("A01", "A02", "B01", "B02"),
#'    Intensity_DNA = c(8, 20, 12, 32),
#'    Texture_DNA = c(5, 2, 43, 13)
#'  )
#' variables <- c("Intensity_DNA", "Texture_DNA")
#' whiten(population, variables, population, 0.01)
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @importFrom rlang :=
#' @importFrom dplyr one_of
#' @importFrom stats cov
#' @export
whiten <- function(population, variables, sample, regularization_param = 1) {
  sample %<>% 
    dplyr::select(one_of(variables)) %>%
    dplyr::collect()
  
  population %<>% 
    dplyr::collect()
  
  avg <- sample %>%
    dplyr::summarise_at(.vars = variables, .funs = mean) %>%
    as.matrix()
  
  covariance <- cov(sample[, variables])
  
  eig_decomp <- eigen(covariance)
  
  W <- diag((eig_decomp$values + regularization_param)^-0.5) %*% 
    t(eig_decomp$vectors)
  
  population_data <- population %>% 
    dplyr::select(one_of(variables)) %>%
    as.matrix()
  
  transformed_population <- population
  
  new_col_names <- paste("PC", 1:NCOL(W), sep = "")
  names(new_col_names) <- variables
  
  transformed_population[, variables] <- t(W %*% 
                                             (apply(population_data, 
                                                    1, 
                                                    function(x) (x - avg)
                                                    )
                                              )
                                           )
  
  for (variable in variables) {
    new_name <- rlang::sym(new_col_names[variable])
    old_name <- rlang::sym(variable)
    
    
    transformed_population %<>%
      dplyr::rename(!!new_name := !!old_name)
  }
  
  transformed_population
}