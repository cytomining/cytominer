# Contribution process

You can contribute to `cytominer` through GitHub issues and pull requests.

## Issues

If you have a proposal for new functionality for `cytominer` and would like to discuss it with the team, please [file an issue](https://github.com/cytomining/cytominer/issues/new) describing the feature you would like to see implemented. We're happy to discuss it with you. 

Additionally, we encourage you to [file an issue](https://github.com/cytomining/cytominer/issues/new) to report broken or unexpected functionality (a.k.a., "bugs"). Please include information such as the versions of `cytominer` and R you use, and provide steps to reproduce the behavior. Sometimes this includes generating small data sets. If necessary, you can share them with us via services like Dropbox and Google Drive.

## Pull requests

If you have a feature you have implemented for `cytominer` that you would like to add to the project, please [create a pull request](https://github.com/cytomining/cytominer/pulls) and we'll work with you to incorporate your addition. Please read the section below on [extending cytominer](#extending-cytominer) and ensure your addition meets the specification and follows [this style guide](http://style.tidyverse.org/).

# Extending cytominer

`cytominer` defines several high-level functions for manipulating profiling data: `aggregate`, `normalize`, `transform`, and `variable_select`. Each function accepts a consistent set of parameters: `population`, `variables`, and `operation`. The `operation` parameter allows the user to specify an exact operation to apply to their data. For example, `"standardize"` is an `operation` that can be applied by the `normalize` function. You can read more about these parameters and the functions in the sections below.

When extending `cytominer`, it is important to consider which of the four functions the new operation belongs to. You should define the new operation in a new file (`R/new_operation.R`), and extend its corresponding high-level function to support the operation. Generally, extending a high-level function to support a new operation is straightforward; look for an `if` block which cases on the `operation` string and add a case for the new operation. Be sure to document the new operation and also list it as an option in the corresponding high-level function's `operation` documentation.

## Parameters
|variable |description |
|---|---|
|`population` |tbl with grouping (metadata) and observation variables.|
|`variables` |character vector specifying observation variables.|
|`operation` |character string specifying a specific method to use.|
|`sample`* |tbl containing sample that is used by some `operation` methods.|
|`strata`* |character vector specifying grouping variables.|
|`...` |optional arguments passed to the `operation` |

\* these parameters may not be required by the function and are omitted. If these are required for new functionality, please extend the high-level function signature to include the parameter with a sane default (e.g., `NULL`).

## Aggregate
Aggregate data based on given grouping.

```R
aggregate <- function(population, variables, strata, 
                      operation = "mean",
                      univariate = TRUE, ...) { ... }
```

`univariate` is a boolean specifying whether the aggregation function is univariate or multivariate.

### Example

```R
population <- tibble::data_frame(
   Metadata_group = c("control", "control", "control", "control",
                      "experiment", "experiment", "experiment",
                      "experiment"),
   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
 )
variables <- c("AreaShape_Area")
strata <- c("Metadata_group", "Metadata_batch")
aggregate(population, variables, strata, operation = "mean")
```

## Normalize
Normalize observation variables based on the specified normalization method.

```R
normalize <- function(population, variables, strata, sample,
                      operation = "standardize", ...) { ... }
```

### Example

```R
suppressMessages(suppressWarnings(library(magrittr)))
population <- tibble::data_frame(
   Metadata_group = c("control", "control", "control", "control",
                      "experiment", "experiment", "experiment", "experiment"),
   Metadata_batch = c("a", "a", "b", "b", "a", "a", "b", "b"),
   AreaShape_Area = c(10, 12, 15, 16, 8, 8, 7, 7)
 )
variables <- c('AreaShape_Area')
strata <- c('Metadata_batch')
sample <- population %>% dplyr::filter(Metadata_group == 'control')
cytominer::normalize(population, variables, strata, sample, operation = "standardize")
```

## Transform
Transform observation variables based on the specified transformation method.

```R
transform <- function(population, variables,
                      operation = "generalized_log", ...) { ... }
```

### Example

```R
population <- tibble::data_frame(
   Metadata_Well = c("A01", "A02", "B01", "B02"),
   Intensity_DNA = c(8, 20, 12, 32)
 )
variables <- c("Intensity_DNA")
transform(population, variables, operation = "generalized_log")
```

## Variable select
Select observation variables based on the specified variable selection method.

```R
variable_select <- function(population, variables,
                            sample = NULL,
                            operation = "variance_threshold", ...) { ... }
```

### Example

```R
suppressMessages(suppressWarnings(library(magrittr)))
population <- tibble::data_frame(
   x = rnorm(100),
   y = rnorm(100)/1000
 )  
population %<>% dplyr::mutate(z = x + rnorm(100) / 10)
sample <- population %>% dplyr::slice(1:30) 
variables <- c("x", "y", "z")
operation <- "correlation_threshold"
cor(sample)

# `x` and `z` are highly correlated; one of them will be removed
head(population)
futile.logger::flog.threshold(futile.logger::ERROR)
variable_select(population, variables, sample, operation) %>% head()
```
