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
      rownames_to_column() %>%
      gather_("key", "value",variables)  %>%
      filter(!is.na(value)) %>%
      spread(key, value) %>%
      select(-rowname)
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
