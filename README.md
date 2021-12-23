
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

This is a basic example which replicates the simulation presented in the subsection 5.1 of the article ["Estimates of the Trade and Welfare Effects of NAFTA"](https://academic.oup.com/restud/article/82/1/1/1547758).

``` r
library(cp2015)
## Data for example
## cp2015_nafta is the data available in the package 
data <- cp2015_nafta

## Simulation imposing zero aggregate deficits.
cp2015_nafta$deficit <- cp2015_nafta$deficit %>%
  dplyr::mutate(
    D_bln = 0, # bln = baseline
    D_cfl = 0 # cfl = counterfactual
  )

## Get the results
results <- run_cp2015(data = cp2015_nafta)

# Welfare for NAFTA countries
nafta <- c("Canada", "Mexico", "USA")
results$welfare %>%
  dplyr::filter(region %in% nafta)
```

```
# tot = terms of trade
# vot = volume of trade

# A tibble: 3 × 5
  region     tot    vot welfare realwage
  <fct>    <dbl>  <dbl>   <dbl>    <dbl>
1 Canada -0.108  0.0443 -0.0638    0.323
2 Mexico -0.412  1.72    1.31      1.72 
3 USA     0.0435 0.0412  0.0847    0.112
```
## Vignette

See the vignette for more details:
```r
vignette("cp2015-nafta")
```