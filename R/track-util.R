#' track helpers
#' 
#' These functions allow you to calculate different migration parameters. 
#' 
#' @name track_helpers
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
NULL

# make sure to not disturb the environment ------------------------------------
old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)
old <- setwd(tempdir())
on.exit(setwd(old), add = TRUE)

#' @export
#' @rdname select_helpers
