context("trajectory")

test_that("`trajectory` summarizes trajectories", {

  # sample data set with two simple tracks
  
  data <- tibble::data_frame(Metadata_timePoint = c(1:5),
                             AreaShape_Center_X = c(1,5,1,5,9), 
                             AreaShape_Center_Y = c(1,4,1,4,7), 
                             TrackObjects_Label = c(rep(1,2),rep(2,3))
  ) 

  res <- track(data)
  expect_true(all(
    res$mean.speed == 5
  ))
})


