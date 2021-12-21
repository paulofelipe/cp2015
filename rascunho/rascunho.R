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


library(cp2015)

cp2015_nafta <- cp2015::cp2015_nafta

# str(cp2015_nafta)

cp2015_nafta$deficit <- cp2015_nafta$deficit %>%
  dplyr::mutate(
    D_bln = 0,
    D_cfl = 0
  )

results <- run_cp2015(data = cp2015_nafta)

nafta <- c("Canada", "Mexico", "USA")

results$welfare %>%
  dplyr::filter(
    region %in% nafta
  )

tot_bilateral <- results$tot %>% 
  dplyr::filter(
    region %in% nafta
  ) %>% 
  dplyr::mutate(
    partner = dplyr::case_when(
      partner %in% nafta ~ "NAFTA",
      TRUE ~ "ROW"
    ),
    category  = "ToT"
  ) %>% 
  dplyr::group_by(region, partner, category) %>% 
  dplyr::summarise(
    value = sum(tot),
    .groups = "drop"
  )

vot_bilateral <- results$vot %>% 
  dplyr::filter(
    region %in% nafta
  ) %>% 
  dplyr::mutate(
    partner = dplyr::case_when(
      partner %in% nafta ~ "NAFTA",
      TRUE ~ "ROW"
    ),
    category = "VoT"
  ) %>% 
  dplyr::group_by(region, partner, category) %>% 
  dplyr::summarise(
    value = sum(vot),
    .groups = "drop"
  )

dplyr::bind_rows(tot_bilateral, vot_bilateral) %>% 
  tidyr::unite(partner_cat, partner, category) %>% 
  tidyr::pivot_wider(
    names_from = "partner_cat",
    values_from = "value"
  )
  


# round((sol_cfl$variables$w_n_hat / sol_bln$variables$w_n_hat *
#   sol_bln$variables$P_n_hat / sol_cfl$variables$P_n_hat - 1) * 100, 2)

# round((sol_cfl$variables$I_n1 / sol_bln$variables$I_n1 *
#   sol_bln$variables$P_n_hat / sol_cfl$variables$P_n_hat - 1) * 100, 2)

# Baseline ---------------------------------------------------------------------
data_bln <- cp2015_data

# 2005 tariffs are not necessary in baseline
data_bln[["tariff_2005"]] <- NULL

cenario_bln <- prepare_cp2015(data_bln)
cenario_bln$variables$D_n[] <- 0
sol_bln <- solve_model(cenario_bln)

# Counterfactual ---------------------------------------------------------------
# Use 2005 tarrifs for NAFTA countries
nafta <- c("Canada", "Mexico", "USA")
data_cfl <- cp2015_data

data_cfl$trade <- data_cfl$trade %>%
  dplyr::left_join(
    x = .,
    y = data_cfl$tariff_2005,
    by = c("sector", "exporter", "importer"),
    suffix = c("_1993", "_2005")
  ) %>% 
  dplyr::mutate(
    tariff = ifelse(
      exporter %in% nafta & importer %in% nafta,
      yes = tariff_2005,
      no = tariff_1993
    )
  ) %>% 
  dplyr::select(sector:value, tariff)

cenario_cfl <- cenario_bln
cenario_cfl$variables$tau_nij1[] <- prepare_cp2015(data_cfl) %>%
  .[["variables"]] %>%
  .[["tau_nij1"]]  

sol_cfl <- solve_model(cenario_cfl)

round((sol_cfl$variables$w_n_hat / sol_bln$variables$w_n_hat *
  sol_bln$variables$P_n_hat / sol_cfl$variables$P_n_hat - 1) * 100, 2)

round((sol_cfl$variables$I_n1 / sol_bln$variables$I_n1 *
  sol_bln$variables$P_n_hat / sol_cfl$variables$P_n_hat - 1) * 100, 2)

################################################################################
################################################################################
################################################################################

devtools::check()
devtools::document()
devtools::install() 

devtools::load_all()
x <- prepare_cp2015(data = cp2015_data)
str(x)

Rcpp::compileAttributes()
devtools::install()
library(cp2015)
data <- cp2015_data
model_data <- prepare_cp2015(data)
tictoc::tic()
sol <- solve_model(model_data, nthreads = 4, maxiter = 2000)
tictoc::toc()

tictoc::tic()
sol <- solve_model2(model_data, maxiter = 2000)
tictoc::toc()

sol$variables$X_nj1
sol$variables$c_nj_hat
sol$variables$w_n_hat

model_data$variables$X_nj1[1, 1]/1e6
sol$variables$X_nj1[1,1]/1e6

model_data$parameters$theta_j
sol$theta_j
str(cp2015_data)


library(tidyverse)

load("/mnt/hdd/Documentos/secex/int_comercial/2021_09_21_intro_age_r/data/cp_data.RData")

convert_factor_to_character <- function(data) {
  data %>%
    mutate(
      across(
        c(where(is.factor)),
        as.character
      )
    )
}

str(tabelas)

head(
  tabelas$valor_producao %>%
    filter(exporter == "Brazil")
)

tabelas$consumo_intermediario %>%
  group_by(destination_sector, importer) %>%
  summarise(value_ci = sum(value_ci)) %>%
  filter(importer == "Brazil")


# Consumo intermediário
intermediate_consumption <- tabelas$consumo_intermediario %>%
  rename(
    input = origin_sector,
    sector = destination_sector,
    region = importer,
    value = value_ci
  ) %>%
  arrange(input, sector, region) %>%
  convert_factor_to_character()

head(intermediate_consumption)

# Consumo final
final_consumption <- tabelas$consumo_final %>%
  rename(
    sector = origin_sector,
    region = importer,
    value = value_fd
  ) %>%
  convert_factor_to_character()

head(final_consumption)

# Valor adicionado
value_added <- tabelas$valor_adicionado %>%
  rename(
    region = exporter,
    value = value_va
  ) %>%
  select(-trabalho) %>%
  convert_factor_to_character() #%>%
  # pivot_longer(
  #   cols = "value_added",
  #   names_to = "factor",
  #   values_to = "value"
  # )

head(value_added)

# Comércio bilateral
trade <- tabelas$comercio_bilateral %>%
  select(sector, exporter, importer, value, tariff = tarifa_93) %>%
  convert_factor_to_character()

head(trade)


# Tarifas de 2005 usadas para o contrafactual
tariff_2005 <- tabelas$comercio_bilateral %>%
  select(sector, exporter, importer, tariff = tarifa_05) %>%
  convert_factor_to_character()

head(tariff_2005)

# Elasticidades
theta <- tabelas$theta_df %>%
  rename(sector = sectors)

theta

# Conjuntos

regions <- unique(intermediate_consumption$region)
sectors <- unique(intermediate_consumption$sector)
#factors <- unique(value_added$factor)


sets <- list(
  regions = regions,
  sectors = sectors
)


cp2015_data <- list(
  sets = sets,
  intermediate_consumption = intermediate_consumption,
  final_consumption = final_consumption,
  value_added = value_added,
  trade = trade,
  theta = theta,
  tariff_2005 = tariff_2005  
)

str(cp2015_data)

save(data, file = "data/cp2015_data.rda", compress = "bzip2", version = 2)


trade %>%
  group_by(importer) %>%
  summarise(value = sum(value)) %>%
  left_join(
    trade %>%
      group_by(exporter) %>%
      summarise(value = sum(value)),
    by = c("importer" = "exporter")
  ) %>%
  mutate(deficit = value.x - value.y)

tabelas$deficit

data <- cp2015_data # cp2015_data is directly load from the package

# Model components (sets, coefficients, variables)
model_data <- prepare_cp2015(cp2015_data)

model_data_bln <- model_data


# // // [[Rcpp::plugins(openmp)]]

# // //' Solve model
# // //'
# // //' @param data a List with the model data.
# // //' @param ufactor an update factor number between (0, 1].
# // //' 
# // //' @export
# //[[Rcpp::export]]
# // List
# // solve_model2(
# //   List data,
# //   double ufactor = 0.8,
# //   int maxiter = 1000,
# //   int triter = 100,
# //   bool trace = true)
# // {
# //   List data_ = clone(data);

# //   // Sets
# //   List sets = data_["sets"];
# //   CharacterVector regions = sets["regions"];
# //   CharacterVector sectors = sets["sectors"];

# //   int N = regions.size();
# //   int J = sectors.size();

# //   // Parameters
# //   List params = data_["parameters"];
# //   NumericVector theta_j = params["theta_j"];
# //   NumericVector pi_nij0 = params["pi_nij0"];
# //   NumericVector gamma_nkj = params["gamma_nkj"];
# //   NumericVector gamma_nj = params["gamma_nj"];
# //   NumericVector alpha_nj = params["alpha_nj"];
# //   NumericVector tau_nij0 = params["tau_nij0"];
# //   NumericVector wL_n = params["wL_n"];

# //   // Variables
# //   List vars = data_["variables"];
# //   NumericVector c_nj_hat = vars["c_nj_hat"];
# //   NumericVector P_nj_hat = vars["P_nj_hat"];
# //   NumericVector pi_nij1 = vars["pi_nij1"];
# //   NumericVector X_nj1 = vars["X_nj1"];
# //   NumericVector I_n1 = vars["I_n1"];
# //   NumericVector D_n = vars["D_n"];
# //   NumericVector w_n_hat = vars["w_n_hat"];
# //   NumericVector tau_nij1 = vars["tau_nij1"];
# //   NumericVector d_nij1 = vars["d_nij1"];
 
# //   // Intermediate variables
# //   NumericVector kappa_nij_hat(Dimension(N, N, J));
# //   NumericVector Y_nj1(Dimension(N, J));

# //   // Check - Variables
# //   NumericVector res_c(Dimension(N, J));
# //   NumericVector res_X(Dimension(N, J));
# //   NumericVector res_w(N);

# //   // Scale
# //   // wL_n = wL_n / 1e6;
# //   // X_nj1 = X_nj1 / 1e6;
# //   // I_n1 = I_n1 / 1e6;
# //   // D_n = D_n / 1e6;

# //   // Equations
# //   for (int n = 0; n < N; n++) {
# //     for (int i = 0; i < N; i++) {
# //       for (int j = 0; j < J; j++) {
# //         int nij = n + i * N + j * N * N;
# //         kappa_nij_hat[nij] = (1 + tau_nij1[nij]) / (1 + tau_nij0[nij]) * d_nij1[nij];
# //       }
# //     }
# //   }

# //   omp_set_num_threads(3);

# //   for (int iter = 0; iter < maxiter; iter++) {

# // #pragma omp parallel for
# //     for (int j = 0; j < J; j++) {
# //       for (int n = 0; n < N; n++) {
# //         double sum_ = 0;
# //         for (int i = 0; i < N; i++) {
# //           int nij = n + i * N + j * N * N;
# //           int ij = i + j * N;
# //           sum_ += pi_nij0[nij] * std::pow(kappa_nij_hat[nij] * c_nj_hat[ij], -theta_j[j]);
# //         }
# //         int nj = n + j * N;
# //         P_nj_hat[nj] = std::pow(sum_, -1 / theta_j[j]);
# //       }
# //     }

# // #pragma omp parallel for
# //     for (int j = 0; j < J; j++) {
# //       for (int i = 0; i < N; i++) {
# //         for (int n = 0; n < N; n++) {
# //           int nij = n + i * N + j * N * N;
# //           int ij = i + j * N;
# //           int nj = n + j * N;
# //           pi_nij1[nij] = pi_nij0[nij] *
# //                          std::pow(c_nj_hat[ij] * kappa_nij_hat[nij] / P_nj_hat[nj],
# //                                   -theta_j[j]);
# //         }
# //       }
# //     }

# // #pragma omp parallel for
# //     for (int n = 0; n < N; n++) {
# //       double tariff_revenue = 0;
# //       for (int j = 0; j < J; j++) {
# //         for (int i = 0; i < N; i++) {
# //           int nij = n + i * N + j * N * N;
# //           int nj = n + j * N;
# //           tariff_revenue += tau_nij1[nij] * pi_nij1[nij] / (1 + tau_nij1[nij]) * X_nj1[nj];
# //         }
# //       }
# //       I_n1[n] = w_n_hat[n] * wL_n[n] + tariff_revenue + D_n[n];
# //     }

# // #pragma omp parallel for
# //     for (int j = 0; j < J; j++) {
# //       for (int n = 0; n < N; n++) {
# //         double sum_ = 0;
# //         for (int i = 0; i < N; i++) {
# //           int inj = i + n * N + j * N * N;
# //           int ij = i + j * N;
# //           sum_ += pi_nij1[inj] / (1 + tau_nij1[inj]) * X_nj1[ij];
# //         }
# //         int nj = n + j * N;
# //         Y_nj1[nj] = sum_;
# //       }
# //     }

# // #pragma omp parallel for
# //     for (int j = 0; j < J; j++) {
# //       for (int n = 0; n < N; n++) {
# //         double mat_price = 1;
# //         for (int k = 0; k < J; k++) {
# //           int nk = n + k * N;
# //           int nkj = n + k * N + j * N * J;
# //           mat_price *= std::pow(P_nj_hat[nk], gamma_nkj[nkj]);
# //         }
# //         int nj = n + j * N;
# //         res_c[nj] = c_nj_hat[nj] - std::pow(w_n_hat[n], gamma_nj[nj]) * mat_price;
# //       }
# //     }

# // #pragma omp parallel for
# //     for (int j = 0; j < J; j++) {
# //       for (int n = 0; n < N; n++) {
# //         double int_expenditure = 0;
# //         for (int k = 0; k < J; k++) {
# //           int njk = n + j * N + k * N * J;
# //           int nk = n + k * N;
# //           int_expenditure += gamma_nkj[njk] * Y_nj1[nk];
# //         }
# //         int nj = n + j * N;
# //         res_X[nj] = X_nj1[nj] - (int_expenditure + alpha_nj[nj] * I_n1[n]);
# //       }
# //     }

# // #pragma omp parallel for
# //     for (int n = 0; n < N; n++) {
# //       double labor_demand = 0;
# //       for (int j = 0; j < J; j++) {
# //         int nj = n + j * N;
# //         labor_demand += gamma_nj[nj] * Y_nj1[nj];
# //       }      
# //       res_w[n] = w_n_hat[n] - labor_demand / wL_n[n];
# //     }

# //     double sum1_ = 0;
# //     double sum2_ = 0;

# //     for (int n = 0; n < N; n++) {
# //       sum1_ += w_n_hat[n] * wL_n[n];
# //       sum2_ += wL_n[n];
# //     }
# //     // res_w[N - 1] = sum1_ / sum2_ - 1;

# //     double mean_X = mean(X_nj1);

# //     double norm;
# //     norm = sqrt(sum(pow(res_c, 2)) +
# //                 sum(pow(res_w, 2)) +
# //                 sum(pow(res_X, 2)) / mean_X);

# //     //     double norm_c, norm_X, norm_w;
# //     //     norm_c = sum(xt::pow(res_c, 2))();
# //     //     norm_X = xt::sum(xt::pow(res_X, 2))();
# //     //     norm_w = xt::sum(xt::pow(res_w, 2))();

# //     //     double mean_X = xt::mean(X_nj1)();

# //     //     double norm = std::sqrt(norm_c + norm_X / mean_X + norm_w);

# //     if ((trace == true) & ((iter == 1) | (iter % triter == 0))) {
# //       Rcout << "||x||: " << norm << "\n";
# //     }

# //     //     // norm = xt::sum(xt::pow(res_c, 2))() +
# //     //     //        xt::sum(xt::pow(res_X, 2))() +
# //     //     //        xt::sum(xt::pow(res_w, 2))();
# //     //     // norm = std::sqrt(norm);

# //     //     // Rcout << "||c||: " << norm_c << "\n";
# //     //     // Rcout << "||X||: " << norm_X/mean_X << "\n";
# //     //     // Rcout << "||w||: " << norm_w << "\n";

# //     c_nj_hat = c_nj_hat - ufactor * res_c;
# //     X_nj1 = X_nj1 - ufactor * res_X;
# //     w_n_hat = w_n_hat - ufactor * res_w;
# //   }

# //   // Update the returned object
# //   vars["c_nj_hat"] = c_nj_hat;
# //   vars["P_nj_hat"] = P_nj_hat;
# //   vars["pi_nij1"] = pi_nij1;
# //   vars["X_nj1"] = X_nj1;
# //   vars["I_n1"] = I_n1;
# //   vars["D_n"] = D_n;
# //   vars["w_n_hat"] = w_n_hat;
# //   vars["tau_nij1"] = tau_nij1;
# //   vars["d_nij1"] = d_nij1;

# //   data_["variables"] = vars;

# //   return data_;
# // }

