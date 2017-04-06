#' Compute trajectory statistics
#'
#' @param population ...
#'
#' @return trajectory statistics
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
track <- function(population, ...) {
  # process `population`, which is the data you get from CellProfiler

  calculate.speed <- function(population) {
    #  if (NROW(x) > 121) {
    #    print(unique(x[1,c("TrackObjects_Label", "Metadata_condition", "Metadata_MATRIX", "Metadata_dateCreated")]))
    #  }
    track <- population %>% 
      dplyr::select(AreaShape_Center_X, AreaShape_Center_Y, Metadata_timePoint) %>%
      dplyr::arrange(Metadata_timePoint)
    
    res <- track %>%
      dplyr::mutate(Metadata_timePoint2 =  Metadata_timePoint - 1) %>% 
      dplyr::select(-Metadata_timePoint) %>%
      dplyr::left_join(., track, by = c("Metadata_timePoint2" = "Metadata_timePoint")) %>%
      na.omit(.)  %>% 
      dplyr::mutate(speed = sqrt((AreaShape_Center_X.y - AreaShape_Center_X.x)^2 +
                            (AreaShape_Center_Y.y - AreaShape_Center_Y.x)^2)) %>%
      dplyr::select(-AreaShape_Center_X.y, -AreaShape_Center_X.x, -AreaShape_Center_Y.y, -AreaShape_Center_Y.x) 
    return(res)
  }
  
  population.with.speed <- population %>% 
    dplyr::group_by(TrackObjects_Label) %>%
    dplyr::do(calculate.speed(.)) 
  
  mean.speed <- population.with.speed %>% 
    dplyr::group_by(TrackObjects_Label) %>%
    dplyr::summarise( mean.speed = mean(speed )) 
  
  return(mean.speed)
}