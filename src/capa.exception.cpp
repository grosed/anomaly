
#ifdef R_NO_REMAP
#undef R_NO_REMAP
#endif

#include <string>
#include "Rcpp.h"

using namespace Rcpp;

void throw_capa_exception(const std::string& msg)
{
  stop(msg);
  return;
}
