context("trajectory")

test_that("`trajectory` summarizes trajectories", {
  
  # edit this to create a small example dataset (say 2 tracks, 5 timepoints each)
  # an easy way to do this is to using `dput`
  data <- tibble::data_frame()
  
  expect_equal(
    trajectory(data),
    tibble::data_frame(speed = 1)
  )
})


  