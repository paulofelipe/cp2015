## code to prepare `cp2015_data` dataset goes here
# load("/mnt/hdd/Documentos/secex/int_comercial/2021_09_21_intro_age_r/data/cp_data.RData")

convert_factor_to_character <- function(data) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        c(where(is.factor)),
        as.character
      )
    )
}

# load("/mnt/hdd/Documentos/github/intro_emr/dados/caliendo_parro.RData", verbose = TRUE)
# Y_df <- x_df %>%
#   dplyr::left_join(
#     y = tau_ij_93_df %>%
#       dplyr::rename(tau = value),
#     by = c("exporter", "importer", "sector")
#   ) %>%
#   dplyr::group_by(exporter, sector) %>%
#   dplyr::summarise(
#     Y = sum(value / tau),
#     .groups = "drop"
#   )

# TR_n <- x_df %>%
#   dplyr::left_join(
#     y = tau_ij_93_df %>%
#       dplyr::rename(tau = value),
#     by = c("exporter", "importer", "sector")
#   ) %>%
#   dplyr::mutate(
#     tr = value / tau * (tau - 1)
#   ) %>%
#   dplyr::group_by(region = importer) %>%
#   dplyr::summarise(
#     tr = sum(tr),
#     .groups = "drop"
#   )

# I_n <- TR_n %>%
#   dplyr::left_join(
#     y = L_df %>%
#       dplyr::rename(wL = value),
#     by = c("region" = "country")
#   ) %>%
#   dplyr::left_join(
#     y = D_df %>%
#       dplyr::rename(D = value),
#     by = c("region" = "country")
#   ) %>%
#   dplyr::mutate(
#     I = tr + wL + D
#   ) %>%
#   dplyr::select(region, I)

# intermediate_consumption <- gammas_ii_df %>%
#   dplyr::select(input, sector, region = country, value) %>%
#   dplyr::left_join(
#     y = Y_df,
#     by = c("region" = "exporter", "sector")
#   ) %>%
#   dplyr::mutate(
#     value = value * Y
#   ) %>%
#   dplyr::select(input, sector, region, value) %>%
#   dplyr::arrange(input, sector, region) %>%
#   convert_factor_to_character() %>%
#   dplyr::mutate(value = value / 1e6)

# final_consumption <- alphas_df %>%
#   dplyr::rename(region = "country") %>%
#   dplyr::left_join(
#     y = I_n,
#     by = "region"
#   ) %>%
#   dplyr::mutate(
#     value = I * value
#   ) %>%
#   dplyr::select(region, sector, value) %>%
#   convert_factor_to_character() %>%
#   dplyr::mutate(value = value / 1e6)

# value_added <- gammas_va_df %>%
#   dplyr::rename(region = country) %>%
#   dplyr::left_join(
#     y = Y_df,
#     by = c("region" = "exporter", "sector")
#   ) %>%
#   dplyr::mutate(
#     value = value * Y
#   ) %>%
#   convert_factor_to_character() %>%
#   dplyr::select(-Y) %>%
#   dplyr::mutate(value = value / 1e6)

# nafta <- c("Canada", "Mexico", "USA")
# trade <- x_df %>%
#   dplyr::left_join(
#     y = tau_ij_93_df %>%
#       dplyr::mutate(tarifa_93 = value - 1) %>%
#       dplyr::select(exporter:sector, tarifa_93),
#     by = c("exporter", "importer", "sector")
#   ) %>%
#   dplyr::left_join(
#     y = tau_ij_05_df %>%
#       dplyr::mutate(tarifa_05 = value - 1) %>%
#       dplyr::select(exporter:sector, tarifa_05),
#     by = c("exporter", "importer", "sector")
#   ) %>%
#   dplyr::mutate(value = value / (1 + tarifa_93)) %>%
#   dplyr::mutate(
#     tariff = tarifa_93,
#     tariff_bln = tarifa_93,
#     tariff_cfl = dplyr::case_when(
#       exporter %in% nafta & importer %in% nafta ~ tarifa_05,
#       TRUE ~ tarifa_93
#     ),
#     # Iceberg trade costs
#     d = 1,
#     d_bln = 1,
#     d_cfl = 1
#   ) %>%
#   dplyr::select(
#     sector, exporter, importer, value,
#     dplyr::starts_with("tariff"), d, dplyr::starts_with("d_")
#   ) %>%
#   convert_factor_to_character() %>%
#   dplyr::mutate(value = value / 1e6)

# deficit <- D_df %>%
#   dplyr::mutate(value = value / 1e6) %>%
#   dplyr::mutate(
#     D = value,
#     D_bln = value,
#     D_cfl = value
#   ) %>%
#   convert_factor_to_character() %>%
#   dplyr::select(region = country, dplyr::starts_with("D"))

# theta <- theta_df %>%
#   dplyr::rename(sector = sectors)

# # Conjuntos
# regions <- unique(intermediate_consumption$region)
# sectors <- unique(intermediate_consumption$sector)


# sets <- list(
#   regions = regions,
#   sectors = sectors
# )

# cp2015_nafta <- list(
#   sets = sets,
#   intermediate_consumption = intermediate_consumption,
#   final_consumption = final_consumption,
#   value_added = value_added,
#   trade = trade,
#   deficit = deficit,
#   theta = theta
# )

# usethis::use_data(cp2015_nafta, overwrite = TRUE)

load("data-raw/cp_data.RData")

# Consumo intermediário
intermediate_consumption <- tabelas$consumo_intermediario %>%
  dplyr::rename(
    input = origin_sector,
    sector = destination_sector,
    region = importer,
    value = value_ci
  ) %>%
  dplyr::arrange(input, sector, region) %>%
  convert_factor_to_character()

# Consumo final
final_consumption <- tabelas$consumo_final %>%
  dplyr::rename(
    sector = origin_sector,
    region = importer,
    value = value_fd
  ) %>%
  convert_factor_to_character()

# Valor adicionado
value_added <- tabelas$valor_adicionado %>%
  dplyr::rename(
    region = exporter,
    value = value_va
  ) %>%
  dplyr::select(-trabalho) %>%
  convert_factor_to_character()

# Comércio bilateral
nafta <- c("Canada", "Mexico", "USA")
trade <- tabelas$comercio_bilateral %>%
  dplyr::mutate(
    tariff = tarifa_93,
    tariff_bln = tarifa_93,
    tariff_cfl = dplyr::case_when(
      exporter %in% nafta & importer %in% nafta ~ tarifa_05,
      TRUE ~ tarifa_93
    ),
    # Iceberg trade costs
    d = 1,
    d_bln = 1,
    d_cfl = 1
  ) %>%
  dplyr::select(
    sector, exporter, importer, value,
    dplyr::starts_with("tariff"), d, dplyr::starts_with("d_")
  ) %>%
  convert_factor_to_character()

# Déficits
# Imports
M_n <- trade %>%
  dplyr::group_by(region = importer) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")

# Exports
E_n <- trade %>%
  dplyr::group_by(region = exporter) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")

# Déficit
deficit <- dplyr::left_join(
  x = M_n,
  y = E_n,
  by = "region",
  suffix = c("_import", "_export")
) %>%
  dplyr::mutate(
    D = value_import - value_export,
    D_bln = D,
    D_cfl = D
  ) %>%
  dplyr::select(region, dplyr::starts_with("D"))

# Elasticidades
theta <- tabelas$theta_df %>%
  dplyr::rename(sector = sectors)

theta

# Conjuntos

regions <- unique(intermediate_consumption$region)
sectors <- unique(intermediate_consumption$sector)


sets <- list(
  regions = regions,
  sectors = sectors
)

cp2015_nafta <- list(
  sets = sets,
  intermediate_consumption = intermediate_consumption,
  final_consumption = final_consumption,
  value_added = value_added,
  trade = trade,
  deficit = deficit,
  theta = theta
)

usethis::use_data(cp2015_nafta, overwrite = TRUE)