
# cp2015

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paulofelipe/cp2015/workflows/R-CMD-check/badge.svg)](https://github.com/paulofelipe/cp2015/actions)
<!-- badges: end -->

The goal of `cp2015` package is to implement the Caliend and Parro (2015) quantitative trade model in R.

## Installation

To install the development version, run the following command:

``` r
remotes::install_github("paulofelipe/cp2015")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cp2015)
## Data for example
## cp2015_nafta is the data available in the package 
data <- cp2015_nafta

## Simulation with balanced trade
cp2015_nafta$deficit <- cp2015_nafta$deficit %>%
  dplyr::mutate(
    D_bln = 0,
    D_cfl = 0
  )

## Get the results
results <- run_cp2015(data = cp2015_nafta)

# Welfare for NAFTA countries
nafta <- c("Canada", "Mexico", "USA")
results$welfare %>%
  dplyr::filter(
    region %in% nafta
  )
```
