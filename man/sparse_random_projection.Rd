% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse_random_projection.R
\name{sparse_random_projection}
\alias{sparse_random_projection}
\title{Reduce the dimensionality of a population using sparse random projection.}
\usage{
sparse_random_projection(population, variables, n_components)
}
\arguments{
\item{population}{tbl with grouping (metadata) and observation variables.}

\item{variables}{character vector specifying observation variables.}

\item{n_components}{size of the projected feature space.}
}
\value{
Dimensionality reduced \code{population}.
}
\description{
\code{sparse_random_projection} reduces the dimensionality of a population
by projecting the original data with a sparse random matrix. Generally more
efficient and faster to compute than a Gaussian random projection matrix,
while providing similar embedding quality.
}
\examples{
population <- tibble::tibble(
  Metadata_Well = c("A01", "A02", "B01", "B02"),
  AreaShape_Area_DNA = c(10, 12, 7, 7),
  AreaShape_Length_DNA = c(2, 3, 1, 5),
  Intensity_DNA = c(8, 20, 12, 32),
  Texture_DNA = c(5, 2, 43, 13)
)
variables <- c("AreaShape_Area_DNA", "AreaShape_Length_DNA", "Intensity_DNA", "Texture_DNA")
sparse_random_projection(population, variables, 2)
}
