#' Run a simulation using the Caliendo and Parro (2015) quantitative trade model
#'  
#' @description 
#' 
#'  
#' Caliendo and Parro (2015) develop a Ricardian quantitative trade model that
#' considers multiple countries and multiple sectors. The model allows interactions
#' accross sectors based on Input-Output linkages. 
#' 
#' The model is originally used to study the trade and welfare effects of NAFTA
#' given the observed tariff changes. However, from data provided by the user, this model
#' can be used for simulations to assess the effects of different trade
#' policies (changes in tariffs and/or iceberg trade costs).   
#' 
#' @param data a List with the model data. Run \code{help(cp2015_nafta)} to see
#' the required data.
#' @param zero_aggregate_deficit a boolean indicating whether the simulation
#' should impose zero aggregate deficits.
#' @param ufactor an update factor between (0, 1]. This value is used
#' to update the value of variables at each iteration.
#' @param tol a tolerance number for convergence. The default is 1e-7.
#' @param maxiter the number of maximum iterations.
#' @param verbose a boolean indicating whether convergence information should be printed.
#' @param triter an integer indicating that information should be printed for each
#' multiple of that number.
#' @param nthreads an integer indicating the number of threads to use.
#' 
#' @return 
#' 
#' A list with 13 elements:
#' 
#'  * c_nj_hat (changes in cost an input bundle) - a data.frame with 3 columns:
#'    * region
#'    * sector
#'    * c_nj_hat (relative change)
#'  * P_nj_hat (changes in sectoral price index) - a data.frame with 3 columns:
#'    * region
#'    * sector
#'    * P_nj_hat (relative change)
#'  * pi_nij (bilateral trade share) - a data.frame with 5 columns:
#'    * importer
#'    * exporter
#'    * sector
#'    * pi_bln (trade share in the baseline scenario)
#'    * pi_cfl (trade share in the counterfactual scenarion)
#'  * X_nj (total expenditure) - a data.frame with 4 columns:
#'    * region
#'    * sector
#'    * X_bln (expenditure in the baseline scenario)
#'    * X_cfl (expenditure in the counterfactual scenario)
#'  * I_n (regional income) - a data.frame with 3 columns:
#'    * region
#'    * I_bln (regional income in the baseline scenario)
#'    * I_cfl (regional income in the counterfactual scenario)
#'  * P_n_hat (consumer price index) - a data.frame with 2 columns:
#'    * region
#'    * P_n_hat (relative change)
#'  * w_n_hat ("wages") - a data.frame with 2 columns:
#'    * region
#'    * w_hat (relative change)
#'  * trade (trade data) - a data.frame with 9 columns:
#'    * importer
#'    * exporter
#'    * sector
#'    * tau_bln (tarrifs in the baseline scenario)
#'    * tau_cfl (tarrifs in the counterfactual scenario)
#'    * d_bln (relative changes of the iceberg trade costs in the baseline scenario)
#'    * d_cfl (relative changes of the iceberg trade costs in the counterfactual scenario)
#'    * trade_bln (trade value, net of tariffs, in the baseline scenario)
#'    * trade_cfl (trade value, net of tariffs, in the counterfactual scenario)
#'  * tot (terms of trade) - a data.frame with 4 columns:
#'    * partner
#'    * region
#'    * sector
#'    * tot (contribution of terms of trade to welfare in %)
#'  * vot (volume of trade) - a data.frame with 4 columns:
#'    * region
#'    * partner
#'    * sector
#'    * vot (contribution of volume of trade to welfare in %)
#'  * tech (technical efficiency) - a data.frame with 4 columns:
#'    * region
#'    * partner
#'    * sector
#'    * tech (contribution of technical efficiency to welfare in %)
#'  * welfare (total welfare by region) - a data.frame with 6 columns:
#'    * region
#'    * tot (contribution of terms of trade to welfare in %)
#'    * vot (contribution of volume of trade to welfare in %)
#'    * tech (contribution of technical efficiency to welfare in %)
#'    * welfare (total welfare in %)
#'    * realwage (relative change in the real wage).
#'  * convergence_info (info about the solution) - a daata.frame with 3 variables:
#'    * scenario
#'    * criteria_value
#'    * message
#' 
#' @references 
#' Lorenzo Caliendo, Fernando Parro, Estimates of the Trade and Welfare Effects of NAFTA,
#' \emph{The Review of Economic Studies}, Volume 82, Issue 1, January 2015, Pages 1–44, https://doi.org/10.1093/restud/rdu035
#'
#' @examples 
#' 
#' data("cp2015_nafta")
#' 
#' results_without_deficits <- run_cp2015(
#'   data = cp2015_nafta,
#'   zero_aggregate_deficit = TRUE,
#'   verbose = TRUE
#' )
#' 
#' results_with_deficits <- run_cp2015(
#'   data = cp2015_nafta,
#'   zero_aggregate_deficit = FALSE,
#'   verbose = TRUE
#' )
#'
#' @export
run_cp2015 <- function(data,
                       zero_aggregate_deficit = FALSE,
                       ufactor = 0.5,
                       tol = 1e-7,
                       maxiter = 10000,
                       verbose = TRUE,
                       triter = 100,
                       nthreads = 1) { 

  if (zero_aggregate_deficit) {
    data$deficit <- data$deficit %>%
      dplyr::mutate(
        D_bln = 0,
        D_cfl = 0
      )
  } else {
    data$deficit <- data$deficit %>%
      dplyr::mutate(
        D_bln = D,
        D_cfl = D
      )
  }

  data <- prepare_cp2015(data)

  if (verbose) {
    cat(crayon::green("Solving the baseline.\n"))
  }

  sol_bln <- solve_model(
    data = data,
    ufactor = ufactor,
    maxiter = maxiter,
    tol = tol,
    triter = triter,
    trace = verbose,
    nthreads = nthreads
  )

  data$variables$tau_nij1 <- data$variables$tau_nij1_cfl
  data$variables$d_nij_hat <- data$variables$d_nij_hat_cfl
  data$variables$D_n <- data$variables$D_n_cfl

  if (verbose) {
    cat(crayon::green("Solving the counterfactual.\n"))
  }

  sol_cfl <- solve_model(
    data = data,
    ufactor = ufactor,
    maxiter = maxiter,
    tol = tol,
    triter = triter,
    trace = verbose,
    nthreads = nthreads
  )

  results <- compute_results(sol_bln, sol_cfl)

  results
}