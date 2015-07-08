#' Constructor for profile.data S3 class
#'
#' @param cf YAML configuration file
#' @param use_csv If TRUE, force loading of CSV file even if binary exists
#'
#' @return profile.data object

profile.data <- function(cf, use_csv = F) {

  # Load the configuration file
  expect_true(file.exists(cf))
  cfg <- yaml::yaml.load_file(cf)
  cfg$cwd <- dirname(cf)

  # construct names of associated files
  cf_name <- sapply(strsplit(basename(cf),"\\."),
                    function(x) paste(x[1:(length(x) - 1)], collapse = "."))
  cfg$profile_file <- paste(cf_name, "csv", sep = ".")
  cfg$profile_file_binary <- paste(cf_name, "rda", sep = ".")
  cfg$digest_file <- paste(cf_name, "digest", sep = ".")

  frda <- file.path(cfg$cwd, cfg$profile_file_binary)
  fcsv <- file.path(cfg$cwd, cfg$profile_file)
  fdig <- file.path(cfg$cwd, cfg$digest_file)

  # Read binary file, or if it doesn't exist, then csv.
  # Save binary file if it doesn't exist
  # Do checksum comparison to make sure its the right binary

  if (file.exists(frda) & !use_csv) {
    data <- readRDS(frda)
    digest_val <- readLines(fdig)
    testthat::expect_equal(digest_val, digest::digest(data))
  } else {
    data <- data.frame(read.csv(fcsv, header = TRUE))
    saveRDS(data, file = frda)
    writeLines(digest::digest(data), fdig)
  }

  # get the index of featdata metadata columns
  testthat::expect_true(!is.null(cfg$feat_start))
  testthat::expect_is(cfg$feat_start, "integer")
  testthat::expect_more_than(cfg$feat_start, 1)
  metadata_cids <- seq(cfg$feat_start - 1)
  featdata_cids <- seq(cfg$feat_start, ncol(data))
  metadata <- data[,metadata_cids]
  featdata <- data[,featdata_cids]

  # Use row.names as an index so that featdata and metadata can be linked
  expect_true(all(row.names(data)==seq(nrow(data))))

  # Create profile.data object
  obj <- list(cfg = cfg,
              metadata = metadata,
              featdata = featdata)

  class(obj) <- "profile.data"

  obj
}
