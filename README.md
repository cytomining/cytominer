# cytominer
[![Build Status](https://travis-ci.org/cytomining/cytominer.png?branch=master)](https://travis-ci.org/cytomining/cytominer)
[![Coverage Status](https://img.shields.io/codecov/c/github/cytomining/cytominer/master.svg)](https://codecov.io/github/cytomining/cytominer?branch=master)

Library for mining patterns in perturbation data

## Installation

```R
# install.packages("devtools")
devtools::install_github("cytomining/cytominer", dependencies = TRUE, build_vignettes = TRUE)
```

Occasionally, the `Suggests` dependencies [may not get installed](https://github.com/hadley/devtools/issues/1370), depending on your system, so you'd need to install those explicitly.
