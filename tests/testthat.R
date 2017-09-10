Sys.setenv("R_TESTS" = "")

library(testthat)
library(cytominer)

test_check("cytominer")
