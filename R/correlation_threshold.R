#' Select features using correlation threshold
#'
#' @param population Data frame with observation and grouping variables (metadata).  
#' @param variables Vector of column names defining the used features. 
#' @param sample Subpopulation used to calculate the distribution of the given variables. 
#' @param cutoff Threshold is a variable between [0,1] that defines the minimum correlation of a selected feature. Default value 0.90.
#' @param method Defines the method used to calculate the correlation. Method is a character string indicating which correlation coefficient (or covariance) is used. One of "pearson" (default), "kendall", or "spearman": can be abbreviated. 
#'
#' @return List with excluded features. 
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
correlation_threshold <- function(population, variables, sample, cutoff = 0.90,
                                  method = "pearson") {

  excluded_indexes <-
    sample %>%
    dplyr::select_(.dots = variables) %>%
    cor(method = method) %>%
    findCorrelation(cutoff = cutoff)

  variables[excluded_indexes]
}
