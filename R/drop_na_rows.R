#' Drop rows that are NA in all variables
#'
#' @param population Data frame with observation and grouping variables (metadata).
#' @param variables Vector of column names defining the used features.
#'
#' @return data without rows with NA in all variables
#'
#' @examples
#'  population <- tibble::data_frame(
#'    Metadata_group = c("control", "control","control","control","experiment","experiment","experiment","experiment"),
#'    Metadata_batch = c("a","a","b","b","a","a","b","b"),
#'    AreaShape_Area = c(10,12,15,16,8,8,7,7),
#'    AreaShape_length = c(2,3,NA,NA,4,5,1,5)
#'  )
#' variables <- c('AreaShape_Area','AreaShape_length')
#' na_per_row = drop_na_rows(population, variables)
#' 
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
# irisx %>% 
# rownames_to_column() %>% 
#   gather_("k", "v", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))  %>% 
#   filter(!is.na(v)) %>% 
#   spread(k, v) %>% 
#   select(-rowname)

# population %>%
#   dplyr::mutate_at(variables, dplyr::funs(is.na)) %>%
#   dplyr::mutate(all_na = rowSums(.[variables]) == length(variables)) %>%
#   dplyr::filter(all_na == FALSE) %>%
#   dplyr::select(-all_na)

}
