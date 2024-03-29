#' Data for NAFTA's tariff reductions simulation in Caliendo and Parro (2015)
#' 
#' @description 
#' This data is used to replicate the results of Caliendo and Parro (2015) for 
#' (only) NAFTA's tariff reductions. In addition, it shows the data structure
#' needed to use this package.
#' 
#' The data structure has been modified for compatibility with the package.
#' Therefore, the data are not in the original format provided by Caliendo and Parro (2015).
#' The original data is available at: \url{https://academic.oup.com/restud/article/82/1/1/1547758#supplementary-data}.
#' 
#' @format A named list of 7 elements: sets, intermediate_consumption, final_consumption, 
#' trade, deficit, and theta.
#' \itemize{
#'   \item sets. A named list of vectors containing all regions and all sectors in data.
#'   \item intermediate_consumption. A data.frame with 4 columns containing intermediate consumption data:
#'     \itemize{
#'        \item input. The input sector.
#'        \item sector. The user sector.
#'        \item sector. The user region.
#'        \item value. The intermediate consumption value (tariffs-inclusive).
#'     }
#'   \item final_consumption. A data.frame with 3 columns containing final consumption data:
#'     \itemize{
#'       \item sector. The sector.
#'       \item region. The consumer region.
#'       \item value. The final consumption value (tariffs-inclusive).
#'     }
#'   \item value_added. A data.frame with 3 column containing value added ("labor") data:
#'     \itemize{
#'       \item sector. The user sector.
#'       \item region. The user region.
#'       \item value. The value added.
#'   }
#'   \item trade. A data.frame with 5 columns containing the bilateral trade and tariff data:
#'     \itemize{
#'       \item sector. The sector.
#'       \item exporter. The exporter region.
#'       \item importer. The importer region.
#'       \item value. The trade value (net of tariffs).
#'       \item tariff. The original tariffs in the database.
#'       \item tariff_bln. The tariffs in the baseline scenario. Usually the same
#'        original tariffs are used, but this is not mandatory.
#'       \item tariff_cfl. The tariffs in the counterfactual scenario.
#'       \item d_bln. The relative change in iceberg trade costs in the baseline scenario (1 = no change).
#'       \item d_cfl. The relative change in iceberg trade costs in the counterfactual scenario (1 = no change).
#'        As the model solution is in exact changes, the initial value can be set
#'        to 1 and changes can be set proportionally. 
#'     }
#'   \item deficit. A data.frame with 4 columns containing the deficit data?
#'     \itemize{
#'       \item region. The region.
#'       \item D. The original deficit in the database.
#'     }
#'   \item theta. A data.frame with 2 columns containing the trade elasticities:
#'     \itemize{
#'        \item sector. The sector.
#'        \item value. The theta (trade elasticity) parameter value.
#'     }
#' }
#' 
#' @references 
#' Lorenzo Caliendo, Fernando Parro, Estimates of the Trade and Welfare Effects of NAFTA,
#' \emph{The Review of Economic Studies}, Volume 82, Issue 1, January 2015, Pages 1–44, https://doi.org/10.1093/restud/rdu035
"cp2015_nafta"