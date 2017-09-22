#' variables <- c("Intensity_DNA", "Texture_DNA")
#' whiten(population, variables, sample)
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
    select(one_of(variables)) %>%
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