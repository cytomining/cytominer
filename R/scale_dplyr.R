#' Scale and center a data.frame or a dplyr::tbl_sql
#'
#' @param x data.frame or dplyr::tbl_sql
#' @param center center
#' @param scale scale
#' @param vars vars
#'
#' @return data.frame or dplyr::tbl_sql after normalization
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
scale_dplyr <- function(x, center, scale, vars) {
  for (var in vars) {
    x %<>%
      dplyr::mutate_(.dots =
                       setNames(list(
                         lazyeval::interp(~ (x - m) / s,
                                          x = as.name(var),
                                          m = center[[var]],
                                          s = scale[[var]])
                       ),
                       paste0(var, '_'))
      )

  }

  x %>%
    dplyr::select_(.dots = paste0(vars, '_'))  %>%
    dplyr::rename_(.dots = setNames(paste0(vars, '_'), vars))
}
