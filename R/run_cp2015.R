#' Run a simulation using the Caliendo and Parro (2015) model
#' @param data a List with the model data. Run \code{help(cp2015_nafta)} to see
#' the required data.
#' @param zero_aggregate_deficit a boolean indicating whether the simulation
#' should impose zero aggregate deficits.
#' @param ufactor a double with an update factor number between (0, 1]. This value is used
#' to update the value of variables at each iteration.
#' @param tol a double with the tolerance criteria.
#' @param maxiter an integer with the number of maximum iterations.
#' @param verbose a boolean indicating whether convergence information should be printed.
#' @param triter an integer indicating that information should be printed for each
#' multiple of that number.
#' @param nthreads an integer indicating the number of threads to use.
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