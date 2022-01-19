#include <cp2015.h>
#include <Rcpp.h>
#include <omp.h>

using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

//' Solve model
//'
//' @param data a List with the model data.
//' @param ufactor a double with an update factor number between (0, 1]. This value is used
//' to update the value of variables at each iteration.
//' @param tol a double with the tolerance criteria.
//' @param maxiter an integer with the number of maximum iterations.
//' @param triter an integer indicating that information should be printed for each
//' multiple of that number.
//' @param trace a boolean indicating whether convergence information should be printed. 
//' @param nthreads an integer indicating the number of threads to use.
//' @export
//' @keywords internal
//[[Rcpp::export]]
List
solve_model(
  List data,
  double ufactor = 0.8,
  double tol = 1e-7,
  int maxiter = 10000,
  int triter = 100,
  bool trace = true,
  int nthreads = 1)
{
  List data_ = clone(data);

  // Sets
  List sets = data_["sets"];
  CharacterVector regions = sets["regions"];
  CharacterVector sectors = sets["sectors"];

  int N = regions.size();
  int J = sectors.size();

  // Parameters
  List params = data_["parameters"];
  xt::rtensor<double, 1> theta_j = params["theta_j"];
  xt::rtensor<double, 3> pi_nij0 = params["pi_nij0"];
  xt::rtensor<double, 3> gamma_nkj = params["gamma_nkj"];
  xt::rtensor<double, 2> gamma_nj = params["gamma_nj"];
  xt::rtensor<double, 2> alpha_nj = params["alpha_nj"];
  xt::rtensor<double, 3> tau_nij0 = params["tau_nij0"];
  // xt::rtensor<double, 3> d_nij0 = params["d_nij0"];
  xt::rtensor<double, 1> wL_n = params["wL_n"];

  // Variables
  List vars = data_["variables"];
  xt::rtensor<double, 2> c_nj_hat = vars["c_nj_hat"];
  xt::rtensor<double, 2> P_nj_hat = vars["P_nj_hat"];
  xt::rtensor<double, 3> pi_nij1 = vars["pi_nij1"];
  xt::rtensor<double, 2> X_nj1 = vars["X_nj1"];
  xt::rtensor<double, 1> I_n1 = vars["I_n1"];
  xt::rtensor<double, 1> D_n = vars["D_n"];
  xt::rtensor<double, 1> w_n_hat = vars["w_n_hat"];
  xt::rtensor<double, 3> tau_nij1 = vars["tau_nij1"];  
  xt::rtensor<double, 3> d_nij_hat = vars["d_nij_hat"];
  
  // Additional variables
  xt::rtensor<double, 3> kappa_nij_hat = xt::ones<double>({ N, N, J });
  xt::rtensor<double, 2> Y_nj1 = xt::zeros<double>({ N, J });
  xt::rtensor<double, 1> P_n_hat = clone(w_n_hat);

  // Check - Variables
  xt::rtensor<double, 2> res_c = xt::zeros<double>({ N, J });
  xt::rtensor<double, 2> res_X = xt::zeros<double>({ N, J });
  xt::rtensor<double, 1> res_w = xt::zeros<double>({ N });

  // Equations
  for (int n = 0; n < N; n++) {
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < J; j++) {
        kappa_nij_hat(n, i, j) = (1 + tau_nij1(n, i, j)) / (1 + tau_nij0(n, i, j)) *
                                 d_nij_hat(n, i, j);
      }
    }
  }

  double norm;
  String message("Unsuccessful convergence");
  omp_set_num_threads(nthreads);

  for (int iter = 1; iter <= maxiter; iter++) {

    R_CheckUserInterrupt();

#pragma omp parallel for
    for (int j = 0; j < J; j++) {
      for (int n = 0; n < N; n++) {
        double sum_ = 0;
        for (int i = 0; i < N; i++) {
          sum_ += pi_nij0(n, i, j) * std::pow(kappa_nij_hat(n, i, j) * c_nj_hat(i, j), -theta_j(j));
        }
        P_nj_hat(n, j) = std::pow(sum_, -1 / theta_j(j));
      }
    }

#pragma omp parallel for
    for (int j = 0; j < J; j++) {
      for (int i = 0; i < N; i++) {
        for (int n = 0; n < N; n++) {
          pi_nij1(n, i, j) = pi_nij0(n, i, j) *
                             pow(c_nj_hat(i, j) * kappa_nij_hat(n, i, j) / P_nj_hat(n, j),
                                      -theta_j(j));
        }
      }
    }

#pragma omp parallel for
    for (int n = 0; n < N; n++) {
      double tariff_revenue = 0;
      for (int j = 0; j < J; j++) {
        for (int i = 0; i < N; i++) {
          tariff_revenue += tau_nij1(n, i, j) * pi_nij1(n, i, j) / (1 + tau_nij1(n, i, j)) * X_nj1(n, j);
        }
      }
      I_n1(n) = w_n_hat(n) * wL_n(n) + tariff_revenue + D_n(n);
    }

#pragma omp parallel for
    for (int j = 0; j < J; j++) {
      for (int n = 0; n < N; n++) {      
        double sum_ = 0;
        for (int i = 0; i < N; i++) {
          sum_ += pi_nij1(i, n, j) / (1 + tau_nij1(i, n, j)) * X_nj1(i, j);
        }
        Y_nj1(n, j) = sum_;
      }
    }

#pragma omp parallel for
    for (int j = 0; j < J; j++) {
      for (int n = 0; n < N; n++) {      
        double mat_price = 1;
        for (int k = 0; k < J; k++) {
          mat_price *= std::pow(P_nj_hat(n, k), gamma_nkj(n, k, j));
        }
        res_c(n, j) = c_nj_hat(n, j) - std::pow(w_n_hat(n), gamma_nj(n, j)) * mat_price;
      }
    }

#pragma omp parallel for
    for (int j = 0; j < J; j++) {
      for (int n = 0; n < N; n++) {      
        double int_expenditure = 0;
        for (int k = 0; k < J; k++) {
          int_expenditure += gamma_nkj(n, j, k) * Y_nj1(n, k);
        }
        res_X(n, j) = (X_nj1(n, j) - (int_expenditure + alpha_nj(n, j) * I_n1(n))) /
                      std::max(X_nj1(n, j), 1.0);
      }
    }

#pragma omp parallel for
    for (int n = 0; n < N; n++) {
      double labor_demand = 0;
      for (int j = 0; j < J; j++) {
        labor_demand += gamma_nj(n, j) * Y_nj1(n, j);
      }
      res_w(n) = w_n_hat(n) - labor_demand / wL_n(n);
    }

    double sum1_ = 0;
    double sum2_ = 0;

    for (int n = 0; n < N; n++) {
      sum1_ += w_n_hat(n) * wL_n(n);
      sum2_ += wL_n(n);
    }
    res_w(N - 1) = sum1_ / sum2_ - 1;

    // double mean_X = xt::mean(X_nj1)();

    double norm_c, norm_X, norm_w;
    norm_c = sum(xt::pow(res_c, 2))();
    norm_X = xt::sum(xt::pow(res_X, 2))();
    norm_w = xt::sum(xt::pow(res_w, 2))();    

    norm = std::sqrt(norm_c + norm_X + norm_w);

    if ((trace == true) & ((iter == 1) | (iter % triter == 0))) {
      Rcout << "Iteration: " << iter << " - ||F(x)||: " << norm << "\n";
    }

    if(iter == maxiter){
      warning("Maximum number of iteration reached before convergence.");
      message = "Maximum iteration reached";
      break;
    }

    if(norm < tol){
      Rcout << "Iteration: " << iter << " - ||F(x)||: " << norm << "\n";
      message = "Successful convergence";
      break;
    }

    for (int j = 0; j < J; j++) {
      for (int n = 0; n < N; n++) {
        c_nj_hat(n, j) = c_nj_hat(n, j) - ufactor * res_c(n, j);
        // res_X is relative to std::max(X_nj1(n, j), 1.0)
        X_nj1(n, j) = X_nj1(n, j) - ufactor * res_X(n, j) * std::max(X_nj1(n, j), 1.0);
      }
    }

    for (int n = 0; n < N; n++) {
      w_n_hat(n) = w_n_hat(n) - ufactor * res_w(n);
    }
  }

  for (int n = 0; n < N; n++) {
    double prod = 1;
    for (int j = 0; j < J; j++) {
      prod *= std::pow(P_nj_hat(n, j), alpha_nj(n, j));
    }
    P_n_hat(n) = prod;
  }

  // Update the returned object
  vars["c_nj_hat"] = c_nj_hat;
  vars["P_nj_hat"] = P_nj_hat;
  vars["pi_nij1"] = pi_nij1;
  vars["X_nj1"] = X_nj1;
  vars["I_n1"] = I_n1;
  vars["D_n"] = D_n;
  vars["w_n_hat"] = w_n_hat;
  vars["tau_nij1"] = tau_nij1;
  vars["d_nij_hat"] = d_nij_hat;
  vars["P_n_hat"] = P_n_hat;

  data_["variables"] = vars;

  data_["convergence_criteria"] = norm;
  data_["message"] = message;

  return data_;
}

