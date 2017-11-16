#include <Rcpp.h>
#include <set>
#include <stack>
#include <map>
#include "mfrs.h"
using namespace Rcpp;

// Algorithm 1 (Wang et al, 2013)
// A depth-first search algorithm for enumerating MFRs in DAGs
// [[Rcpp::export]]
List mfrs_dag(int node_count, IntegerVector composite_nodes) {
  
  // initialization
  
  int mfr_count = 0; // "count"
  std::set< std::vector<int> > mfr_set; // "MFR"
  std::stack<int> link_stack; // "edge_list"
  
  // "visit_num([node])"
  std::map<int,int> node_visit_count;
  for (int i = 0; i < node_count; i++) node_visit_count[i] = 0;
  // "partialMFR([node])"
  std::map<int, std::set< std::vector<int> > > node_partial_mfr_map;
  
  std::map<int,int> link_visit_count; // "visit_num([link])"
  for (int i = 0; i < node_count; i++) link_visit_count[i] = 0;
  // "partialMFR([link])"
  std::map<int, std::set< std::vector<int> > > clink_partial_mfr_map;
  for (int i = 0; i < composite_nodes.size(); i++) {
    //clink_partial_mfr_map[composite_nodes[i]] = 0;
  }
  
  // ... more stuff ...
  
  // return list
  return List::create(
    _["mfr_count"] = mfr_count,
    _["mfr_set"] = mfr_set
  );
}

// Algorithm 2 (Wang et al, 2013)


// Algorithm 3 (Wang et al, 2013)

