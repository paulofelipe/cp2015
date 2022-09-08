## code to prepare `cp2015_data` dataset goes here
library(magrittr, include.only = "%>%")

convert_factor_to_character <- function(data) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        c(where(is.factor)),
        as.character
      )
    )
}

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
  convert_factor_to_character() %>%
  dplyr::mutate(value = value / 1e0)

# Consumo final
final_consumption <- tabelas$consumo_final %>%
  dplyr::rename(
    sector = origin_sector,
    region = importer,
    value = value_fd
  ) %>%
  convert_factor_to_character() %>%
  dplyr::mutate(value = value / 1e0)

# Valor adicionado
value_added <- tabelas$valor_adicionado %>%
  dplyr::rename(
    region = exporter,
    value = value_va
  ) %>%
  dplyr::select(-trabalho) %>%
  convert_factor_to_character() %>%
  dplyr::mutate(value = value / 1e0)

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
    #d = 1,
    d_bln = 1,
    d_cfl = 1
  ) %>%
  dplyr::select(
    sector, exporter, importer, value,
    dplyr::starts_with("tariff"), dplyr::starts_with("d_")
  ) %>%
  convert_factor_to_character() %>%
  dplyr::mutate(value = value / 1e0)

# Déficits
# Imports
M_n <- trade %>%
  dplyr::group_by(region = importer) %>%
  dplyr::summarise(value = sum(value), .groups = "drop") %>%
  dplyr::mutate(value = value / 1e0)

# Exports
E_n <- trade %>%
  dplyr::group_by(region = exporter) %>%
  dplyr::summarise(value = sum(value), .groups = "drop") %>%
  dplyr::mutate(value = value / 1e0)

# Déficit
deficit <- dplyr::left_join(
  x = M_n,
  y = E_n,
  by = "region",
  suffix = c("_import", "_export")
) %>%
  dplyr::mutate(
    D = value_import - value_export,
    # D_bln = D,
    # D_cfl = D
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
