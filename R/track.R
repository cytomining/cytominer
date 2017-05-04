#' Compute track statistics
#'
#' @param population ...
#'
#' @return track statistics
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#'
track <- function(population, group_track, ...) {
  # process `population`, which is the data you get from CellProfiler
  tracks <- add_dist(population, group_track)
  
  track_feature_list <- list(
    get_track_angle(tracks),
    get_track_chemotactic_index(tracks),
    get_track_directionality(tracks),
    get_track_distance(tracks),
    get_track_dp(tracks),
    get_track_fmi(tracks),
    get_track_lifetime(tracks),
    get_track_msd(tracks,t = 5),
    get_track_sectors(tracks),
    get_track_speed(tracks)) 
  
  tracks <-  Reduce(function(...) merge(..., all = TRUE, by = group_track), track_feature_list)

  return(tracks)
}
