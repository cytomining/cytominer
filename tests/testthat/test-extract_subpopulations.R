context("extract_subpopulations")

test_that(
  "`extract_subpopulations` extracts and assigns each point to a subpopulation",
  {
    set.seed(24)

    # generate two data frames corresponding to treatment and control
    k <- 6
    population <- matrix(runif(1000), 100, 10)
    reference <- matrix(runif(2000), 200, 10)

    # set 10% of the data to be NA's
    population[sample(
      1:length(population),
      size = round(length(population) * 0.1)
    )] <- NA
    reference[sample(
      1:length(reference),
      size = round(length(reference) * 0.1)
    )] <- NA

    population <- data.frame(population, Metadata_id = 1:nrow(population))
    reference <- data.frame(reference, Metadata_id = 1:nrow(reference))

    variables <- grep(
      colnames(population),
      pattern = "Metadata_", inv = TRUE,
      value = TRUE
    )

    subpops <- extract_subpopulations(
      population = population,
      reference = reference,
      variables = variables,
      k = k
    )

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
      dist_to_clusters <-
        cross_dist(data[, variables], centers)

      apply(
        dist_to_clusters, 1,
        function(x) {
          tibble::tibble(
            dist_to_cluster = min(x),
            cluster_id = which.min(x)
          )
        }
      ) %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(cluster_id = unname(cluster_id))
    }

    population_clusters <-
      cluster_assign(
        data =
          population[
            stats::complete.cases(population[, variables]),
          ],
        centers = subpops$subpop_centers,
        variables = variables,
        k = k
      )

    reference_clusters <-
      cluster_assign(
        data =
          reference[
            stats::complete.cases(reference[, variables]),
          ],
        centers = subpops$subpop_centers,
        variables = variables,
        k = k
      )

    # test whether the cluster assignment and distance to the clusters
    # are consistent with the returned cluster centers
    expect_equal(
      subpops$population_clusters[, c("dist_to_cluster", "cluster_id")] %>%
        as.data.frame(),
      population_clusters %>%
        as.data.frame()
    )

    expect_equal(
      subpops$reference_clusters[, c("dist_to_cluster", "cluster_id")] %>%
        as.data.frame(),
      reference_clusters %>%
        as.data.frame()
    )

    # test whether the summation of cluster proportions is equal to one
    expect_equal(
      subpops$subpop_profiles %>%
        dplyr::select(-cluster_id) %>%
        dplyr::summarise_all(sum),
      dplyr::tribble(
        ~population, ~reference,
        1, 1
      )
    )
  }
)
