#include <Rcpp.h>
//#include "mfrs.h"
using namespace Rcpp;

// Algorithm 1 (Wang et al, 2013)
// A depth-first search algorithm for enumerating MFRs in DAGs
// [[Rcpp::export]]
List mfrs_dag() {
  
  // initialization
  
  int mfr_count = 0; // "count"
  std::set< std::vector<int> > mfr_set; // "MFR"
  //std::stack<int> link_stack; // "edge_list"
  
  // return list
  return List::create(
    _["mfr_count"] = mfr_count,
    _["mfr_set"] = mfr_set
  );
}

// Algorithm 2 (Wang et al, 2013)


// Algorithm 3 (Wang et al, 2013)

