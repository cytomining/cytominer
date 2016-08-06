#' Drop rows that are NA in all variables
#'
#' @param population ...
#' @param variables ...
#'
#' @return data without rows with NA in all variables
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
drop_na_rows <- function(population, variables) {

 # Coalesce() must have at least 2 arguments.
 if(length(variables) == 1)
    variables <- c(variables, variables)

 population %>%
   dplyr::filter_(.dots =
                    sprintf("!is.null(coalesce(%s))",
                            paste(variables, collapse = ","))
                  )

 # population %>%
 #   dplyr::mutate_at(variables, dplyr::funs(is.na)) %>%
 #   dplyr::mutate(all_na = rowSums(.[variables]) == length(variables)) %>%
 #   dplyr::filter(all_na == FALSE) %>%
 #   dplyr::select(-all_na)

}
