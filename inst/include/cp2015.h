#include <numeric>           // Standard library import for std::accumulate
#define STRICT_R_HEADERS     // Otherwise a PI macro is defined in R
#include "xtensor/xmath.hpp" // xtensor import for the C++ universal functions
#include "xtensor/xarray.hpp"
#include "xtensor/xtensor.hpp"
#include "xtensor-r/rarray.hpp"  // R bindings
#include "xtensor-r/rtensor.hpp" // R bindings
#include <xtensor/xview.hpp>