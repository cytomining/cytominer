context("track")
test_that("`track` collapse single cell data to track objects", {
  
  # sample data set with one simple tracks
  # todo: add more tracks to cover different scenarios inlcuding
  # *invalid tracks
  # *not te
  data <- tibble::data_frame(
    Metadata_timePoint = c(1:5),
    Location_Center_X = c(1, 2, 3, 4, 5),
    Location_Center_Y = c(1, 1, 1, 1, 1),
    TrackObjects_Label = c(rep(1, 5))
  )

  distances <- tibble::data_frame(
    TrackObjects_Distance_Traveled = c(1,1,1,1)
  )
  
  track_angle <- tibble::data_frame(
    Track_Angle = c(0)
  )
  
  track_ci  <- tibble::data_frame(
    Track_CI = c(-1)
  )
  
  track_directionality  <- tibble::data_frame(
    Track_Directionality = c(1)
  )
  
  track_distance  <- tibble::data_frame(
    Track_Distance_Traveled = c(4),
    Track_Integrated_Distance_Traveled = c(4)
    
  )
  
  track_dp  <- tibble::data_frame(
    Track_DP = c(3)
  )
  
  track_fmi <- tibble::data_frame(
    Track_xFMI = c(1),
    Track_yFMI = c(0)
  )
  
  track_life_time  <- tibble::data_frame(
    Track_Length = c(as.integer(5)),
    Track_Life_Time = c(as.integer(5)),
    Track_One_Cell = TRUE
  )
  
  track_msd  <- tibble::data_frame(
    Track_MSD = c(1)
  )
  
  track_sectors  <- tibble::data_frame(
    Track_Positive_Sector = c(0), 
    Track_Negative_Sector = c(1), 
    Track_Neutral_Sector_Up = c(0), 
    Track_Neutral_Sector_Down = c(0), 
    Track_Sector = c(2)
  )
  
  track_speed  <- tibble::data_frame(
    Track_Speed = c(1),
    Track_Speed_max = c(1), 
    Track_Speed_X = c(1), 
    Track_Speed_Y = c(0) 
  )

  track_data <- cytominer:::displace(data,'TrackObjects_Label')

  expect_equal(
    track_data %>%
      dplyr::select(TrackObjects_Distance_Traveled) %>%
      na.omit(), 
    distances
    )
  
  expect_equal( 
    track_data %>% cytominer::angle(.),
    track_angle
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::chemotacticIndex(.),
    track_ci
  )
  
  expect_equal(   
    track_data %>% 
      cytominer::directionality(.),
    track_directionality
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::distance(.),
    track_distance
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::directionalPersistence(.),
    track_dp
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::forwardMigrationIndex(.),
    track_fmi
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::lifeTime(.),
    track_life_time
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::meanSquaredDisplacement(.,2),
    track_msd
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::sectorAnalysis(),
    track_sectors
  )
  
  expect_equal( 
    track_data %>% 
      cytominer::speed(),
    track_speed
  )
  
})
