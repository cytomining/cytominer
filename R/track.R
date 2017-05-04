#' Compute track statistics
#'
#' @param population ...
#'
#' @return track statistics
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
track <- function(population, group_track, ...) {
  # process `population`, which is the data you get from CellProfiler
  tracks <- add_dist(population, group_track)
  
  features <- list(
    angle(tracks),
    chemotacticIndex(tracks),
    directionality(tracks),
    distance(tracks),
    dp(tracks),
    forwardMigrationIndex(tracks),
    lifeTime(tracks),
    meanSquaredDisplacement(tracks,t = 5),
    sectorAnalysis(tracks),
    speed(tracks)) 
  
  return(Reduce(function(...) merge(..., all = TRUE, by = group_track), features))
}

#' @export
displace <- function(tracks, group_track) {
  dplyr::right_join(
    tracks %>%
      dplyr::select_(.dots = c(group_track, 'Location_Center_X', 'Location_Center_Y','Metadata_timePoint')) %>%
      dplyr::mutate(Metadata_timePoint2 =  (Metadata_timePoint - 1) ) %>% 
      dplyr::filter(Metadata_timePoint2 != -1) %>%
      dplyr::select(-Metadata_timePoint), 
    tracks, by = (.dots = c(group_track, "Metadata_timePoint2" = "Metadata_timePoint" ))) %>%
    dplyr::mutate(Track_dX = Location_Center_X.x - Location_Center_X.y) %>% 
    dplyr::mutate(Track_dY = Location_Center_Y.x - Location_Center_Y.y) %>% 
    dplyr::select(-Location_Center_X.x, -Location_Center_Y.x) %>%
    dplyr::rename(Location_Center_X = Location_Center_X.y) %>%
    dplyr::rename(Location_Center_Y = Location_Center_Y.y) %>% 
    dplyr::rename(Metadata_timePoint = Metadata_timePoint2) %>%
    dplyr::mutate(TrackObjects_Distance_Traveled = sqrt(Track_dX^2 + Track_dY^2)) 
}

#' @export
speed <- function(tracks) {
  tracks %>%
    dplyr::summarize(Track_Length = n(),
      Track_Speed = sum(TrackObjects_Distance_Traveled, na.rm = TRUE) / (n() - 1), 
      Track_Speed_max = max(TrackObjects_Distance_Traveled, na.rm = TRUE),
      Track_Speed_X = sum(Track_dX, na.rm = TRUE) / (n() - 1),
      Track_Speed_Y = sum(Track_dY, na.rm = TRUE) / (n() - 1)) %>%
    dplyr::select(-Track_Length)
}

#' @export
forwardMigrationIndex <- function(tracks) {
  s <- tracks %>% 
    dplyr::summarize(Track_Integrated_Distance_Traveled = sum(TrackObjects_Distance_Traveled, na.rm = TRUE),
      Track_Displacement_X = tail(Location_Center_X, n = 1) - Location_Center_X[1],
      Track_Displacement_Y = tail(Location_Center_Y, n = 1) - Location_Center_Y[1]
    ) %>% 
    dplyr::mutate(Track_xFMI = Track_Displacement_X / Track_Integrated_Distance_Traveled, 
      Track_yFMI = Track_Displacement_Y / Track_Integrated_Distance_Traveled) %>%
    dplyr::select(-Track_Integrated_Distance_Traveled, -Track_Displacement_X, -Track_Displacement_Y )
}

#' @export
lifeTime  <- function(tracks) {
  tracks %>%
    dplyr::summarize( 
      Track_Length = n(), 
      Track_Life_Time = length(unique(Metadata_timePoint)),
      Track_One_Cell = length(unique(Metadata_timePoint)) == length(Metadata_timePoint) )  
}

#' @export
angle <- function(tracks) {
  tracks %>% 
    dplyr::summarize(Track_Angle = atan2(tail(Location_Center_Y, n = 1) - Location_Center_Y[1], 
      tail(Location_Center_X, n = 1) - Location_Center_X[1]))
}

#' @export
distance <- function(tracks) {
  tracks %>% 
    dplyr::summarize(Track_Integrated_Distance_Traveled = sum(TrackObjects_Distance_Traveled, na.rm = TRUE),
      Track_Distance_Traveled = sqrt( (tail(Location_Center_Y, n = 1) - Location_Center_Y[1] )^2 + 
          (tail(Location_Center_X, n = 1) - Location_Center_X[1] )^2 )) 
}

#' @export
directionality <- function(tracks) {
  tracks %>% 
    distance() %>%
    dplyr::mutate( Track_Directionality = Track_Distance_Traveled / Track_Integrated_Distance_Traveled) %>%
    dplyr::select(-Track_Distance_Traveled, -Track_Integrated_Distance_Traveled)
}

#' @export
meanSquaredDisplacement <- function(tracks,t) {
  tracks %>% 
    dplyr::summarize(Track_MSD = (Location_Center_X[t] - Location_Center_X[1])^2 +
        (Location_Center_Y[t] - Location_Center_Y[1])^2)
}

#' @export
directionalPersistence <- function(tracks) {
  tracks %>%
    directionality %>%
    dplyr::mutate(Track_DP = ceiling(3 * Track_Directionality)) %>%
    dplyr::select(-Track_Directionality)
}

#' @export
chemotacticIndex <- function(tracks) {
  tracks %>% 
    angle() %>%
    dplyr::mutate(Track_CI = -cos(Track_Angle) ) %>%
    dplyr::select(-Track_Angle)
}


# returned sector indices
#
#   \  3  /
# 1   \ /   2
#     / \
#   /  4  \
#
#' @export
sectorAnalysis <- function(tracks) {
  tracks %>%
    angle() %>%
    dplyr::mutate(Track_Positive_Sector     = as.numeric( abs(Track_Angle) > (3 * pi / 4)),
      Track_Negative_Sector     = as.numeric( abs(Track_Angle) < pi / 4),
      Track_Neutral_Sector_Up   = as.numeric( (Track_Angle >= pi / 4) & (Track_Angle < 3 * pi / 4 )),
      Track_Neutral_Sector_Down = as.numeric( (Track_Angle <= -pi / 4) & (Track_Angle >= -3 * pi / 4 ))
    ) %>%
    dplyr::mutate(Track_Sector = Track_Positive_Sector + 2 * Track_Negative_Sector + 3 * Track_Neutral_Sector_Up + 4 * Track_Neutral_Sector_Down) %>%
    dplyr::select(-Track_Angle)
}
