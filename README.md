
# cp2015

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/paulofelipe/cp2015/workflows/R-CMD-check/badge.svg)](https://github.com/paulofelipe/cp2015/actions)
<!-- badges: end -->

The goal of `cp2015` package is to implement the Caliendo and Parro (2015) quantitative trade model in R.

## Installation

To install the development version, run the following command:

``` r
remotes::install_github("paulofelipe/cp2015")
```

## Example

This is a basic example which replicates the simulation presented in the subsection 5.1 of the article ["Estimates of the Trade and Welfare Effects of NAFTA"](https://academic.oup.com/restud/article/82/1/1/1547758).

```r
library(cp2015)
## Data for example
## cp2015_nafta is the data available in the package 
data("cp2015_nafta")

## Simulation imposing zero aggregate deficits.
results <- run_cp2015(data = cp2015_nafta, zero_aggregate_deficit = TRUE)
```

### Welfare results

We have extended the welfare decomposition formula to account for the possibility of changes in iceberg trade costs (variations in technical efficiency).

<!-- $d \ln W_n = \frac{1}{I_n}\sum_{j = 1}^J\sum_{i = 1}^N \underbrace{\left(E_{ni}^j d \ln c_n^j - M_{ni}^j d \ln c_i^j \right)}_{\text{Terms of trade}} + \frac{1}{I_n} \sum_{j = 1}^J\sum_{i = 1}^N \underbrace{\tau_{ni}^ j M_{ni}^j \left(d \ln M_{ni}^j - d \ln c_i^j \right)}_{\text{Volume of trade}} - \frac{1}{I_n} \sum_{j = 1}^J\sum_{i = 1}^N \underbrace{M_{ni}^j (1 + \tau_{ni}^j) d \ln d_{ni}^j}_{\text{Technical efficiency}}$ --> <img style="transform: translateY(0.1em); background: white;" src="svg/9jtcFFupyB.svg">

```r
# Welfare for NAFTA countries
nafta <- c("Canada", "Mexico", "USA")
results$welfare %>%
  dplyr::filter(region %in% nafta)
```

This example considers only variations in tariffs. In this way, there are no variations in technical efficiency. However, the package allows simulations that take into account changes in the iceberg trade costs.

```
# Values are in %
# tot = terms of trade
# vot = volume of trade
# tech = technical efficiency

# A tibble: 3 × 6
  region     tot    vot  tech welfare realwage
  <fct>    <dbl>  <dbl> <dbl>   <dbl>    <dbl>
1 Canada -0.108  0.0443     0 -0.0638    0.323
2 Mexico -0.412  1.72       0  1.31      1.72 
3 USA     0.0435 0.0412     0  0.0848    0.112
```


## Vignette

See the vignette for more details:
```r
vignette("cp2015-nafta")
```