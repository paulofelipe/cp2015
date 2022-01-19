## code to prepare `cp2015_data` dataset goes here
#load("/mnt/hdd/Documentos/secex/int_comercial/2021_09_21_intro_age_r/data/cp_data.RData")
load("data-raw/cp_data.RData")


convert_factor_to_character <- function(data) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        c(where(is.factor)),
        as.character
      )
    )
}

tabelas$consumo_intermediario %>%
  dplyr::group_by(destination_sector, importer) %>%
  dplyr::summarise(value_ci = sum(value_ci)) %>%
  dplyr::filter(importer == "Brazil")


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
  convert_factor_to_character() #%>%
  # pivot_longer(
  #   cols = "value_added",
  #   names_to = "factor",
  #   values_to = "value"
  # )

head(value_added)

# Comércio bilateral
nafta <- c("Canada", "Mexico", "USA")
trade <- tabelas$comercio_bilateral %>%
  dplyr::mutate(
    tariff = tarifa_93,
    tariff_bln = tarifa_93,
    tariff_cfl = dplyr::case_when(
      exporter %in% nafta & importer %in% nafta ~ tarifa_05,
      TRUE ~ tarifa_93
    )
  ) %>% 
  dplyr::select(sector, exporter, importer, value, dplyr::starts_with("tariff")) %>%
  convert_factor_to_character()

head(trade)


# Tarifas de 2005 usadas para o contrafactual
tariff_2005 <- tabelas$comercio_bilateral %>%
  dplyr::select(sector, exporter, importer, tariff = tarifa_05) %>%
  convert_factor_to_character()

head(tariff_2005)

# Elasticidades
theta <- tabelas$theta_df %>%
  dplyr::rename(sector = sectors)

theta

# Conjuntos

regions <- unique(intermediate_consumption$region)
sectors <- unique(intermediate_consumption$sector)
#factors <- unique(value_added$factor)


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
  theta = theta,
  tariff_2005 = tariff_2005  
)

usethis::use_data(cp2015_nafta, overwrite = TRUE)
