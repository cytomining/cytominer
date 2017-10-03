context("extract_subpopulations")

test_that(
"`extract_subpopulations` extracts and assigns each point to a subpopulation", {

  set.seed(24)

  k <- 6
  data_trt <- matrix(runif(1000), 100, 10)
  data_ctrl <- matrix(runif(2000), 200, 10)

  data_trt[sample(1:length(data_trt),
                  size = round(length(data_trt) * 0.1))] <- NA
  data_ctrl[sample(1:length(data_ctrl),
                   size = round(length(data_ctrl) * 0.1))] <- NA

  data_trt <- data.frame(data_trt, Metadata_id = 1:NROW(data_trt))
  data_ctrl <- data.frame(data_ctrl, Metadata_id = 1:NROW(data_ctrl))

  feats <- colnames(data_trt)
  feats <- feats[which(!stringr::str_detect(feats, "Metadata_"))]

  subpops <- extract_subpopulations(population_treatment = data_trt,
                         population_control = data_ctrl,
                         feats = feats,
                         k = k)

  cluster_assign <- function(data, centers, feats, k) {
    dist_to_clusters <-
      as.matrix(dist(rbind(data[, feats],
                           centers)))[1:NROW(data),
                                      (NROW(data) + 1):(NROW(data) + k)]
    clusters <- t(apply(dist_to_clusters, 1,
                            function(x) c(min(x),
                                          which.min(x))))
    colnames(clusters) <- c("dist_to_cluster", "cluster_id")
    clusters <- dplyr::as_tibble(clusters)
    clusters$cluster_id <- as.integer(clusters$cluster_id)
    return(clusters)
  }

  trt_clusters <-
    cluster_assign(data = data_trt[stats::complete.cases(data_trt[, feats]), ],
                   centers = subpops$subpop_centers,
                   feats = feats,
                   k = k)

  ctrl_clusters <-
    cluster_assign(data =
                     data_ctrl[stats::complete.cases(data_ctrl[, feats]), ],
                   centers = subpops$subpop_centers,
                   feats = feats,
                   k = k)

  # test whether the cluster assignment and distance to the clusters
  # are consistent with the returned cluster centers

  expect_equal(
    subpops$treatment_clusters[, c("dist_to_cluster",
                                              "cluster_id")],
    trt_clusters
  )

  expect_equal(
    subpops$ctrl_clusters[, c("dist_to_cluster",
                                   "cluster_id")],
    ctrl_clusters
  )

  # test whether the summation of cluster proportions is equal to one
  expect_equal(
    subpops$subpop_profiles %>%
      dplyr::select(-cluster_id) %>%
      tidyr::gather(key = "pert_type", value = "freq") %>%
      dplyr::group_by(pert_type) %>%
      dplyr::summarise(tot.freq = sum(freq)) %>%
      dplyr::select(tot.freq),
    dplyr::tibble(tot.freq = c(1, 1))
  )
})
