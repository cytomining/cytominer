#' Rotate the platemap for a 384-well plate
#'
#' @return profile.data with platemap rotate by 180 degrees
#'
rotate_platemap <- function(...) UseMethod("rotate_platemap")

#' @describeIn rotate_platemap
#'
#' @param P profile.data generated from a 384-well plate experiment
#' and containing 'Well' in the metdata
#'
rotate_platemap.profile.data <- function(P, ...) {
  testthat::expect_false("WellRow" %in% names(meta(P)))
  testthat::expect_false("WellCol" %in% names(meta(P)))
  testthat::expect_true("Well" %in% names(meta(P)))

  rotmap <- outer(letters[1:16], seq(24) %>% sprintf("%02d", .), FUN="paste", sep="")
  names(rotmap) <- as.character(rotmap[rev(seq(16)), rev(seq(24))])

  testthat::expect_true(all(meta(P)$Well %in% rotmap))
  testthat::expect_true(all(meta(P)$Well %in% names(rotmap)))

  P$metadata <- dplyr::mutate(P$metadata,
                              Well = rotmap[as.character(Well)])
  P

}
