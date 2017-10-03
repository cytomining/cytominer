context("extract_subpopulations")

test_that(
"`extract_subpopulations` extracts and assigns each point to a subpopulation", {

  set.seed(24)

  # generate two data frames corresponding to treatment and control
  k <- 6
  data_trt <- matrix(runif(1000), 100, 10)
  data_ctrl <- matrix(runif(2000), 200, 10)

  # set 10% of the data to be NA's
  data_trt[sample(1:length(data_trt),
                  size = round(length(data_trt) * 0.1))] <- NA
  data_ctrl[sample(1:length(data_ctrl),
                   size = round(length(data_ctrl) * 0.1))] <- NA

  data_trt <- data.frame(data_trt, Metadata_id = 1:nrow(data_trt))
  data_ctrl <- data.frame(data_ctrl, Metadata_id = 1:nrow(data_ctrl))

  variables <- grep(colnames(data_trt), pattern = "Metadata_", inv = TRUE,
                value = TRUE)

  subpops <- extract_subpopulations(population = data_trt,
                                    reference = data_ctrl,
                                    variables = variables,
                                    k = k)

  # pairwise distance between the rows of x and rows of y
  cross_dist <- function(x, y) {
    dist_matrix <-
      rbind(x, y) %>%
      dist() %>%
      as.matrix()

    dist_matrix[1:nrow(x), (nrow(x) + 1):(nrow(x) + nrow(y))]

  }

  # find nearest cluster center and distance to it
  cluster_assign <- function(data, centers, variables, k) {
    dist_to_clusters <- cross_dist(data[, variables], centers)

    apply(dist_to_clusters, 1,
          function(x) data_frame(dist_to_cluster = min(x),
                                 cluster_id = which.min(x))) %>%
      bind_rows()

  }

  trt_clusters <-
    cluster_assign(data =
                     data_trt[stats::complete.cases(data_trt[, variables]), ],
                   centers = subpops$subpop_centers,
                   variables = variables,
                   k = k)

  ctrl_clusters <-
    cluster_assign(data =
                     data_ctrl[stats::complete.cases(data_ctrl[, variables]), ],
                   centers = subpops$subpop_centers,
                   variables = variables,
                   k = k)

  # test whether the cluster assignment and distance to the clusters
  # are consistent with the returned cluster centers
  expect_equal(
    subpops$treatment_clusters[, c("dist_to_cluster", "cluster_id")],
    trt_clusters
  )

  expect_equal(
    subpops$ctrl_clusters[, c("dist_to_cluster", "cluster_id")],
    ctrl_clusters
  )

  # test whether the summation of cluster proportions is equal to one
  expect_equal(
    subpops$subpop_profiles %>%
      dplyr::select(-cluster_id) %>%
      summarise_all(sum),
    dplyr::frame_data(~control, ~treatment,
                      1, 1)
  )
})
