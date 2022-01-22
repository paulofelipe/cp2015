test_that("solution without deficits", {
  expected_results <- structure(list(
    region = structure(c(5L, 20L, 30L), .Label = c(
      "Argentina",
      "Australia", "Austria", "Brazil", "Canada", "Chile", "China",
      "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",
      "India", "Indonesia", "Ireland", "Italy", "Japan", "Korea", "Mexico",
      "Netherlands", "New Zealand", "Norway", "Portugal", "South Africa",
      "Spain", "Sweden", "Turkey", "UK", "USA", "Row"
    ), class = "factor"),
    tot = c(-0.11, -0.41, 0.04), vot = c(0.04, 1.72, 0.04), tech = c(
      0,
      0, 0
    ), welfare = c(-0.06, 1.31, 0.08), realwage = c(
      0.32,
      1.72, 0.11
    )
  ), row.names = c(NA, -3L), class = "data.frame")

  data(cp2015_nafta)

  results <- run_cp2015(
    data = cp2015_nafta,
    zero_aggregate_deficit = TRUE,
    tol = 1e-7,
    nthreads = 1,
    verbose = TRUE
  )

  nafta <- c("Canada", "Mexico", "USA")
  results <- results$welfare %>%
    dplyr::filter(region %in% nafta) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 2)
    )

  expect_equal(results, expected_results)
})

test_that("solution with deficits", {
  expected_results <- structure(list(
    region = structure(c(5L, 20L, 30L), .Label = c(
      "Argentina",
      "Australia", "Austria", "Brazil", "Canada", "Chile", "China",
      "Denmark", "Finland", "France", "Germany", "Greece", "Hungary",
      "India", "Indonesia", "Ireland", "Italy", "Japan", "Korea", "Mexico",
      "Netherlands", "New Zealand", "Norway", "Portugal", "South Africa",
      "Spain", "Sweden", "Turkey", "UK", "USA", "Row"
    ), class = "factor"),
    tot = c(-0.08, -0.41, 0.05), vot = c(0.04, 1.59, 0.04), tech = c(
      0,
      0, 0
    ), welfare = c(-0.04, 1.17, 0.08), realwage = c(
      0.33,
      1.64, 0.12
    )
  ), row.names = c(NA, -3L), class = "data.frame")
  data(cp2015_nafta)

  results <- run_cp2015(
    data = cp2015_nafta,
    zero_aggregate_deficit = FALSE,
    tol = 1e-7,
    nthreads = 1,
    verbose = TRUE
  )

  nafta <- c("Canada", "Mexico", "USA")
  results <- results$welfare %>%
    dplyr::filter(region %in% nafta) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), round, 2)
    )

  expect_equal(results, expected_results)
})