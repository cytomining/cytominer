#' Compute track statistics
#'
#' @param population, single cell data
#' @param grouping_variable, column name storing the track label
#' @return track
#' @importFrom magrittr %>%
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  data <- dplyr::group_by_(data,'TrackObjects_Label')
#'  tracks <- track(data,'TrackObjects_Label')
#' @export
track <- function(population, grouping_variable) {
  # process `population`, which is the data you get from CellProfiler
  tracks <- displace(population, grouping_variable)
  
  features <- list(
    angle(tracks),
    chemotacticIndex(tracks),
    directionality(tracks),
    distance(tracks),
    directionalPersistence(tracks),
    forwardMigrationIndex(tracks),
    lifeTime(tracks),
    meanSquaredDisplacement(tracks,tau = 2),
    sectorAnalysis(tracks),
    speed(tracks)) 
  
  return(Reduce(function(...) merge(..., all = TRUE, by = grouping_variable), features))
}

#' Add spatial displacement per frame for each track object
#'
#' @param population, data frame storing single cell data 
#' @param group_variable, column name storing the track label
#' @return displacement   
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#' @importFrom magrittr %>%
#' @export
displace <- function(population, group_variable) {
  dplyr::right_join(
    population %>%
      dplyr::select_(.dots = c(group_variable, 'Location_Center_X', 'Location_Center_Y','Metadata_timePoint')) %>%
      dplyr::mutate(Metadata_timePoint2 =  (Metadata_timePoint - 1) ) %>% 
      dplyr::filter(Metadata_timePoint2 != -1) %>%
      dplyr::select(-Metadata_timePoint), 
    population, by = (.dots = c(group_variable, "Metadata_timePoint2" = "Metadata_timePoint" ))) %>%
      dplyr::mutate(Track_dX = Location_Center_X.x - Location_Center_X.y) %>% 
      dplyr::mutate(Track_dY = Location_Center_Y.x - Location_Center_Y.y) %>% 
      dplyr::select(-Location_Center_X.x, -Location_Center_Y.x) %>%
      dplyr::rename(Location_Center_X = Location_Center_X.y) %>%
      dplyr::rename(Location_Center_Y = Location_Center_Y.y) %>% 
      dplyr::rename(Metadata_timePoint = Metadata_timePoint2) %>%
      dplyr::mutate(TrackObjects_Distance_Traveled = sqrt(Track_dX^2 + Track_dY^2)) 
}

#' Add spatial displacement per frame for each track object
#'
#' @param tracks data frame with single cell data 
#' @return displacement   
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  speed <- cytominer::speed(tracks)
#' @importFrom magrittr %>%
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

#' Computer the forward migration index of a track object
#'
#' @param tracks data frame with track objects 
#' @return forward migration index   
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  forwardMigrationIndex <- cytominer::forwardMigrationIndex(tracks)
#'    
#' @importFrom magrittr %>%
#' @importFrom utils tail
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

#' Calculate lifeTime of a track object.
#'
#' @param tracks data frame with track objects 
#' @return Calculate life time of each track object
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  lifeTime <-  cytominer::lifeTime(tracks)
#'    
#' @importFrom magrittr %>%
#' @export
lifeTime  <- function(tracks) {
  tracks %>%
    dplyr::summarize( 
      Track_Length = n(), 
      Track_Life_Time = length(unique(Metadata_timePoint)),
      Track_One_Cell = length(unique(Metadata_timePoint)) == length(Metadata_timePoint) )  
}

#' Calculate angle of a track object.
#'
#' @param tracks data frame with track objects 
#' @return The angle of each track 
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  angle <-  cytominer::angle(tracks)
#'    
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
angle <- function(tracks) {
  tracks %>% 
    dplyr::summarize(Track_Angle = atan2(tail(Location_Center_Y, n = 1) - Location_Center_Y[1], 
      tail(Location_Center_X, n = 1) - Location_Center_X[1]))
}

#' Calculate distance traveled and the integrated distance traveled of a track object.
#'
#' @param tracks data frame with track objects
#' @return distance traveled 
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  distance <-  cytominer::distance(tracks)
#'    
#' @importFrom magrittr %>%
#' @importFrom utils tail
#' @export
distance <- function(tracks) {
  tracks %>% 
    dplyr::summarize(Track_Integrated_Distance_Traveled = sum(TrackObjects_Distance_Traveled, na.rm = TRUE),
      Track_Distance_Traveled = sqrt( (tail(Location_Center_Y, n = 1) - Location_Center_Y[1] )^2 + 
          (tail(Location_Center_X, n = 1) - Location_Center_X[1] )^2 )) 
}

#' Calculate the directionality of a track object.
#'
#' @param tracks data frame with track objects
#'
#' @return directionality
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  directionality <-  cytominer::directionality(tracks)
#'    
#' @importFrom magrittr %>%
#' @export
directionality <- function(tracks) {
  tracks %>% 
    distance() %>%
    dplyr::mutate( Track_Directionality = Track_Distance_Traveled / Track_Integrated_Distance_Traveled) %>%
    dplyr::select(-Track_Distance_Traveled, -Track_Integrated_Distance_Traveled)
}

#' Calculate the mean squared displacement of a track object.
#'
#' @param tracks data frame with track objects
#' @param tau delta t
#' @return meanSquaredDisplacement
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tau <- 2
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  meanSquaredDisplacement <-  cytominer::meanSquaredDisplacement(tracks,tau)
#'    
#' @importFrom magrittr %>%
#' @export
meanSquaredDisplacement <- function(tracks,tau) {
  tracks %>% 
    dplyr::summarize(Track_MSD = (Location_Center_X[tau] - Location_Center_X[1])^2 +
        (Location_Center_Y[tau] - Location_Center_Y[1])^2)
}

#' Calculate the mean directionalPersistence of a track object.
#'
#' @param tracks data frame with track objects
#' @return directional persistence
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  directionalPersistence <-  cytominer::directionalPersistence(tracks)
#'    
#' @importFrom magrittr %>%
#' @export
directionalPersistence <- function(tracks) {
  directionalPersistence <- tracks %>%
    directionality %>%
    dplyr::mutate(Track_DP = ceiling(3 * Track_Directionality)) %>%
    dplyr::select(-Track_Directionality)
}

#' Calculate the mean chemotactic index of a track object.
#'
#' @param tracks data frame with track objects
#' @return chemotacticIndex
#' @examples 
#' data <- tibble::data_frame(
#'   Metadata_timePoint = c(1:5),
#'   Location_Center_X = c(1, 2, 3, 4, 5),
#'   Location_Center_Y = c(1, 1, 1, 1, 1),
#'   TrackObjects_Label = c(rep(1, 5))
#' )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  chemotacticIndex <-  cytominer::chemotacticIndex(tracks)
#' @importFrom magrittr %>%
#' @export
chemotacticIndex <- function(tracks) {
  chemotacticIndex <- tracks %>% 
    angle() %>%
    dplyr::mutate(Track_CI = -cos(Track_Angle) ) %>%
    dplyr::select(-Track_Angle)
}

#
#   \  3  /
# 1   \ /   2
#     / \
#   /  4  \
#' perform sector analysis and label each track according to its direction of movement.
#
#' @param tracks data frame with track objects
#' @return sector 
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  tracks <- cytominer::displace(data,'TrackObjects_Label')
#'  sectorAnalysis <-  cytominer::sectorAnalysis(tracks)

#'    
#' @importFrom magrittr %>%
#' @export
sectorAnalysis <- function(tracks) {
  sectorAnalysis <- tracks %>%
    angle() %>%
    dplyr::mutate(Track_Positive_Sector     = as.numeric( abs(Track_Angle) > (3 * pi / 4)),
      Track_Negative_Sector     = as.numeric( abs(Track_Angle) < pi / 4),
      Track_Neutral_Sector_Up   = as.numeric( (Track_Angle >= pi / 4) & (Track_Angle < 3 * pi / 4 )),
      Track_Neutral_Sector_Down = as.numeric( (Track_Angle <= -pi / 4) & (Track_Angle >= -3 * pi / 4 ))
    ) %>%
    dplyr::mutate(Track_Sector = Track_Positive_Sector + 2 * Track_Negative_Sector + 3 * Track_Neutral_Sector_Up + 4 * Track_Neutral_Sector_Down) %>%
    dplyr::select(-Track_Angle)
}


#' calculate valid observation time as sum of the length of all 
#' valid tracks divided by the sum of the length of all tracks 
#' 
#' @param tracks data frame with track objects
#' @param minPathLength minimum length of a valid track
#' @return validObservationTime
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  data <- dplyr::group_by_(data,'TrackObjects_Label')
#'  tracks <- track(data,'TrackObjects_Label')
#'  minPathLength <- 5
#'  vot <-   validObservationTime(tracks, minPathLength)
#' @importFrom magrittr %>% 
#' @export
validObservationTime <- function(tracks, minPathLength) {
  validObservationTime <- merge(tracks %>%  
      dplyr::filter(Track_Length > minPathLength) %>%
      dplyr::summarise(sum_track_valid = sum(Track_Length)) , 
    tracks %>% 
      dplyr::summarise(sum_track = sum(Track_Length))) %>%
    dplyr::mutate(VOT = (sum_track_valid / sum_track)) %>%
    dplyr::select(-sum_track_valid)
}


#' Identify valid tracks. Valid tracks are defined as tracks with a life time longer then a predifined value.
#' 
#' @param tracks data frame with track objects
#' @param minPathLength minimum length of a valid track
#' @return validObservationTime
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  data <- dplyr::group_by_(data,'TrackObjects_Label')
#'  tracks <- track(data,'TrackObjects_Label')
#'  minPathLength <- 5
#'  validateTracks <-   validateTracks(tracks, minPathLength)
#' @importFrom magrittr %>% 
#' @export
validateTracks <- function(tracks, minPathLength){
  tracks %>%
    dplyr::mutate(Track_Valid = as.numeric(Track_Length > minPathLength)) %>%
    dplyr::summarize(
      Exp_Tracks = n(),
      Exp_Valid_Tracks = sum(Track_Valid), 
      Exp_Valid_Track_Fraction = sum(Track_Valid) / n()
      )
}


#' Assess track quality.
#' @param tracks data frame with track objects
#' @param minPathLength minimum length of a valid track
#' @param trackLabel column name of track index column  
#' @return validObservationTime
#' @examples 
#'  data <- tibble::data_frame(
#'    Metadata_timePoint = c(1:5),
#'    Location_Center_X = c(1, 2, 3, 4, 5),
#'    Location_Center_Y = c(1, 1, 1, 1, 1),
#'    TrackObjects_Label = c(rep(1, 5))
#'  )
#'  trackLabel <- 'TrackObjects_Label'
#'  data <- dplyr::group_by_(data,trackLabel)
#'  tracks <- track(data,trackLabel)
#'  minPathLength <- 5
#'  trackQuality <- assess(tracks,minPathLength,trackLabel)
#' @importFrom magrittr %>% 
#' @export
assess <- function(tracks, minPathLength, trackLabel) {
  trackInfo <- list(validObservationTime(tracks, minPathLength),
    validateTracks(tracks,minPathLength))
  return(Reduce(function(...) merge(..., all = TRUE, by_ = trackLabel), trackInfo))
}