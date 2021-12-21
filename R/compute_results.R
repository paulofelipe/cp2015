#' Compute the results
#' 
#' @param sol_bln a baseline solution.
#' @param sol_cfl a counterfactual solution.
#' @keywords internal
compute_results <- function(sol_bln, sol_cfl) {
  c_hat <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$c_nj_hat),
    y = array_to_df(sol_cfl$variables$c_nj_hat),
    by = c("region", "sector"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::mutate(c_hat = value_cfl / value_bln) %>%
    dplyr::select(region, sector, c_hat)

  P_hat <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$P_nj_hat),
    y = array_to_df(sol_cfl$variables$P_nj_hat),
    by = c("region", "sector"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::mutate(P_hat = value_cfl / value_bln) %>%
    dplyr::select(region, sector, P_hat)

  pi_ <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$pi_nij1),
    y = array_to_df(sol_cfl$variables$pi_nij1),
    by = c("importer", "exporter", "sector"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::rename(
      pi_bln = value_bln,
      pi_cfl = value_cfl
    )

  X <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$X_nj1),
    y = array_to_df(sol_cfl$variables$X_nj1),
    by = c("region", "sector"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::rename(
      X_bln = value_bln,
      X_cfl = value_cfl
    )

  tau <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$tau_nij1),
    y = array_to_df(sol_cfl$variables$tau_nij1),
    by = c("importer", "exporter", "sector"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::rename(
      tau_bln = value_bln,
      tau_cfl = value_cfl
    )

  trade <- dplyr::left_join(
    x = pi_,
    y = tau,
    by = c("importer", "exporter", "sector")
  ) %>%
    dplyr::left_join(
      y = X,
      by = c("importer" = "region", "sector")
    ) %>%
    dplyr::mutate(
      trade_bln = X_bln / (1 + tau_bln) * pi_bln,
      trade_cfl = X_cfl / (1 + tau_cfl) * pi_cfl
    ) %>% 
    dplyr::select(
      importer:sector, tau_bln, tau_cfl, trade_bln, trade_cfl
    )

  # trade %>%
  #   dplyr::group_by(importer, exporter) %>%
  #   dplyr::summarise(
  #     dplyr::across(
  #       dplyr::starts_with("trade_"),
  #       sum
  #     )
  #   ) %>%
  #   dplyr::mutate(change = round((trade_cfl/trade_bln - 1) * 100, 2)) %>%
  #   dplyr::filter(importer == "USA", exporter == "Mexico") %>%
  #   dplyr::pull(change)

  I_n <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$I_n),
    y = array_to_df(sol_cfl$variables$I_n),
    by = "region",
    suffix = c("_bln", "_cfl")
  ) %>% 
    dplyr::rename(
      I_bln = value_bln,
      I_cfl = value_cfl
    )

  P_n_hat <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$P_n_hat),
    y = array_to_df(sol_cfl$variables$P_n_hat),
    by = c("region"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::mutate(P_n_hat = value_cfl / value_bln) %>%
    dplyr::select(region, P_n_hat)

  w_hat <- dplyr::left_join(
    x = array_to_df(sol_bln$variables$w_n_hat),
    y = array_to_df(sol_cfl$variables$w_n_hat),
    by = c("region"),
    suffix = c("_bln", "_cfl")
  ) %>%
    dplyr::mutate(w_hat = value_cfl / value_bln) %>%
    dplyr::select(region, w_hat)

  tot <- trade %>%
    dplyr::select(importer, exporter, sector, exp_bln = trade_bln) %>%
    dplyr::left_join(
      y = trade %>%
        dplyr::select(importer, exporter, sector, imp_bln = trade_bln),
      by = c("exporter" = "importer", "importer" = "exporter", "sector")
    ) %>%
    dplyr::left_join(
      y = c_hat,
      by = c("exporter" = "region", "sector")
    ) %>%
    dplyr::left_join(
      y = c_hat,
      by = c("importer" = "region", "sector"),
      suffix = c("_exp", "_imp")
    ) %>%
    dplyr::mutate(
      tot = exp_bln * (c_hat_exp - 1) - imp_bln * (c_hat_imp - 1)
    ) %>%
    dplyr::left_join(
      y = I_n,
      by = c("exporter" = "region")
    ) %>%
    dplyr::mutate(
      tot = tot / I_bln * 100
    ) %>%
    dplyr::select(partner = importer, region = exporter, sector, tot)

  vot <- trade %>%
    dplyr::left_join(
      y = c_hat,
      by = c("exporter" = "region", "sector")
    ) %>%
    dplyr::mutate(
      vot = tau_bln * trade_bln * (trade_cfl / (trade_bln + 1e-8) - c_hat)
    ) %>%
    dplyr::left_join(
      y = I_n,
      by = c("importer" = "region")
    ) %>%
    dplyr::mutate(
      vot = vot / I_bln * 100
    ) %>%
    dplyr::select(region = importer, partner = exporter, sector, vot)

  tot_total <- tot %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(tot = sum(tot)) 

  vot_total <- vot %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(vot = sum(vot)) 

  real_wage <- w_hat %>% 
    dplyr::left_join(
      P_n_hat,
      by = "region"
    ) %>% 
    dplyr::mutate(
      realwage = (w_hat/P_n_hat - 1) * 100
    ) %>% 
    dplyr::select(region, realwage)

  welfare <- dplyr::left_join(
    x = tot_total,
    y = vot_total,
    by = "region"
  ) %>%
    dplyr::mutate(
      welfare = tot + vot
    ) %>% 
    dplyr::left_join(
      y = real_wage,
      by = "region"
    )


  list(
    c_hat = c_hat,
    P_hat = P_hat,
    pi = pi_,
    X = X,
    I = I_n,    
    P_n_hat = P_n_hat,
    w_hat = w_hat,
    trade = trade,
    tot = tot,
    vot = vot,
    welfare = welfare
  )
}