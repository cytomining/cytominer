#' Drop rows that are NA in all variables
#'
#' @param population ...
#' @param variables ...
#'
#' @return data without rows with NA in all variables
#'
#' 
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
drop_na_rows <- function(population, variables) {
  
  
  if (is.data.frame(population)) {
    population %>%
      tibble::rownames_to_column(., var = "rowname_temp") %>%
      tidyr::gather_("key", "value",variables)  %>%
      dplyr::filter(!is.na(value)) %>%
      tidyr::spread(key, value) %>%
      dplyr::select(-rowname_temp)
  } else { 
    
    # Coalesce() must have at least 2 arguments.
    if(length(variables) == 1)
      variables <- c(variables, variables)
    
    population %>%
      dplyr::filter_(.dots =
          sprintf("!is.null(coalesce(%s))",
            paste(variables, collapse = ","))
      )
  }
  
}
