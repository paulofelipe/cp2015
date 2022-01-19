#' Prepares the components for the Caliendo and Parro model (2015)
#'
#' Given a database, it builds the elements needed to run the simulations.
#'
#' The returned elements in the list are:
#' \itemize{
#'   \item parameters. Elements that do not change during simulation.
#' It does not include exogenous variables as the user may change these variables
#' in the scenario specifications.
#'   \item variables. All model variables. Endogenous and Exogenous.
#'   \item sets. Vectors that list the regions, sectors and production factors
#' present in the data.
#'   \item parameters_descs. Coefficient descriptions.
#'   \item variables_descs. Variable descriptions.
#' }
#'
#' @param data a named list with the basedata to create the model components.
#' Run \code{help(cp2015_nafta)} to see the data format that is required.
#'
#' @return a named list with 5 elements.
#'
#' @importFrom rlang .data
#' @keywords internal
prepare_cp2015 <- function(data) {
  sets <- data$sets

  # Trade elasticities
  theta_j <- data$theta %>%
    df_to_array(indexes = list(sector = sets$sectors))

  # Initial trade shares
  pi_nij0 <- data$trade %>%
    dplyr::group_by(sector, importer) %>%
    dplyr::mutate(
      value = value * (1 + tariff) / sum(value * (1 + tariff))
    ) %>%
    dplyr::select(importer, exporter, sector, value) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  # Initial tariff
  tau_nij0 <- data$trade %>%
    dplyr::select(importer, exporter, sector, tariff) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  # Initial iceberg trade cost
  # d_nij0 <- data$trade %>%
  #   dplyr::select(importer, exporter, sector, d) %>%
  #   df_to_array(
  #     indexes = list(
  #       importer = sets$regions,
  #       exporter = sets$regions,
  #       sector = sets$sectors
  #     )
  #   )

  # Output
  total_intermediate_consumption <- data$intermediate_consumption %>%
    dplyr::group_by(region, sector) %>%
    dplyr::summarise(value_ic = sum(value), .groups = "drop")

  total_value_added <- data$value_added %>%
    dplyr::group_by(region, sector) %>%
    dplyr::summarise(value_va = sum(value), .groups = "drop")

  output <- dplyr::full_join(
    x = total_intermediate_consumption,
    y = total_value_added,
    by = c("region", "sector")
  ) %>%
    dplyr::mutate(
      value_output = value_ic + value_va
    ) %>%
    dplyr::select(region, sector, value_output)

  # Share of intermediate consumption in production
  gamma_nkj <- data$intermediate_consumption %>%
    dplyr::left_join(
      y = output,
      by = c("sector", "region")
    ) %>%
    dplyr::mutate(value = value / value_output) %>%
    dplyr::select(region, input, sector, value) %>%
    df_to_array(
      indexes = list(
        region = sets$regions,
        input = sets$sectors,
        sector = sets$sectors
      )
    )

  # Share of value added in production
  gamma_nj <- data$value_added %>%
    dplyr::left_join(
      y = output,
      by = c("sector", "region")
    ) %>%
    dplyr::mutate(value = value / value_output) %>%
    dplyr::select(region, sector, value) %>%
    df_to_array(
      indexes = list(
        region = sets$regions,
        sector = sets$sectors
      )
    )

  # Share of each sector in final demand
  alpha_nj <- data$final_consumption %>%
    dplyr::group_by(region) %>%
    dplyr::mutate(value = value / sum(value)) %>%
    dplyr::select(region, sector, value) %>%
    df_to_array(
      indexes = list(
        region = sets$regions,
        sector = sets$sectors
      )
    )

  # Change in cost of the input bundle
  c_nj_hat <- create_array(
    value = 1,
    indexes = list(
      region = sets$regions,
      sector = sets$sectors
    )
  )

  # Change in price index of composite intermediate good
  P_nj_hat <- create_array(
    value = 1,
    indexes = list(
      region = sets$regions,
      sector = sets$sectors
    )
  )

  # d' is the new iceberg trade costs
  d_nij_hat <- data$trade %>%
    dplyr::select(importer, exporter, sector, d_bln) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  d_nij_hat_cfl <- data$trade %>%
    dplyr::select(importer, exporter, sector, d_cfl) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  # tau' is the new tariff. It is initialized with the initial tariffs
  # tau_nij1 <- tau_nij0

  tau_nij1 <- data$trade %>%
    dplyr::select(importer, exporter, sector, tariff_bln) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  tau_nij1_cfl <- data$trade %>%
    dplyr::select(importer, exporter, sector, tariff_cfl) %>%
    df_to_array(
      indexes = list(
        importer = sets$regions,
        exporter = sets$regions,
        sector = sets$sectors
      )
    )

  # pi' is the new shares
  pi_nij1 <- pi_nij0

  # Change in factor remuneration (wages)
  # An additional dimensional is added.
  w_n_hat <- create_array(
    value = 1,
    indexes = list(
      region = sets$regions
    )
  )

  # Initital factor income
  wL_n <- data$value_added %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(value = sum(value), .groups = "drop") %>%
    df_to_array(
      indexes = list(
        region = sets$regions
      )
    )


  # X' is the expenditure level
  X_nj1 <- data$trade %>%
    dplyr::group_by(region = importer, sector) %>%
    dplyr::summarise(value = sum(value * (1 + tariff)), .groups = "drop") %>%
    df_to_array(
      indexes = list(
        region = sets$regions,
        sector = sets$sectors
      )
    )

  # Imports
  # M_n <- data$trade %>%
  #   dplyr::group_by(region = importer) %>%
  #   dplyr::summarise(value = sum(value), .groups = "drop") %>%
  #   df_to_array(indexes = list(region = sets$regions))

  # Exports
  # E_n <- data$trade %>%
  #   dplyr::group_by(region = exporter) %>%
  #   dplyr::summarise(value = sum(value), .groups = "drop") %>%
  #   df_to_array(indexes = list(region = sets$regions))

  # Déficit
  D_n <- data$deficit %>%
    # avoids note importFrom("stats", "D")
    dplyr::select("region", "D") %>%
    df_to_array(indexes = list(region = sets$regions))

  D_n_bln <- data$deficit %>%
    dplyr::select(region, D_bln) %>%
    df_to_array(indexes = list(region = sets$regions))

  D_n_cfl <- data$deficit %>%
    dplyr::select(region, D_cfl) %>%
    df_to_array(indexes = list(region = sets$regions))

  # D_n <- M_n - E_n

  # I' is the income level
  # factors_income <- data$value_added %>%
  #   dplyr::group_by(region) %>%
  #   dplyr::summarise(value = sum(value), .groups = "drop") %>%
  #   df_to_array(indexes = list(region = sets$regions))

  tariff_revenue <- data$trade %>%
    dplyr::group_by(region = importer) %>%
    dplyr::summarise(value = sum(value * tariff), .groups = "drop") %>%
    df_to_array(indexes = list(region = sets$regions))

  I_n1 <- wL_n + tariff_revenue + D_n

  # Model parameters
  parameters <- list(
    theta_j = theta_j,
    pi_nij0 = pi_nij0,
    gamma_nkj = gamma_nkj,
    gamma_nj = gamma_nj,
    alpha_nj = alpha_nj,
    tau_nij0 = tau_nij0,
    #d_nij0 = d_nij0,
    wL_n = wL_n/1e0
  )

  # Model variables
  variables <- list(
    c_nj_hat = c_nj_hat,
    P_nj_hat = P_nj_hat,
    pi_nij1 = pi_nij1,
    X_nj1 = X_nj1/1e0,
    I_n1 = I_n1/1e0,
    D_n = D_n_bln/1e0,
    D_n_cfl = D_n_cfl/1e0,
    w_n_hat = w_n_hat,
    tau_nij1 = tau_nij1,
    tau_nij1 = tau_nij1,
    tau_nij1_cfl = tau_nij1_cfl,
    d_nij_hat = d_nij_hat,
    d_nij_hat_cfl = d_nij_hat_cfl
  )

  list(
    sets = sets,
    parameters = parameters,
    variables = variables
  )
}