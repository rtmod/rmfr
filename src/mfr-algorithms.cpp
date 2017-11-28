#include <Rcpp.h>
#include <set>
#include <stack>
#include <map>
#include "mfrs.h"
using namespace Rcpp;

// Algorithm 1 (Wang et al, 2013)
// A depth-first search algorithm for enumerating MFRs in DAGs
// [[Rcpp::export]]
List mfrs_dag(int node_count,
              IntegerMatrix link_array,
              IntegerVector composite_nodes,
              int source_node, int target_node) {
  
  // setup
  
  // sort 'composite_nodes' and ensure that it has no duplicates
  std::sort(composite_nodes.begin(), composite_nodes.end());
  //auto af = std::adjacent_find(composite_nodes.begin(), composite_nodes.end());
  //if (af != composite_nodes.end()) {
  //  stop("Vector of composite nodes contains duplicates.");
  //}
  // ensure that 'link_array' has two columns (and node index entries?)
  if (link_array.ncol() != 2) {
    stop("Matrix of links does not have two columns.");
  }
  
  // initialization
  
  int mfr_count = 0; // "count"
  route_set mfr_set; // "MFR"
  std::stack<int> link_stack; // "edge_list"
  
  // "visit_num([node])"
  //std::map<int,int> node_visit_count;
  //int node_visit_count[node_count];
  std::vector<int> node_visit_count;
  // "partialMFR([node])"
  std::map<int, route_set> node_pmfr_map;
  // initialize maps and fill in-link vectors
  for (int i = 0; i < node_count; i++) {
    //node_visit_count[i] = 0;
    node_visit_count.push_back(0);
    node_pmfr_map[i];
    //for (int j = 0; j < link_array.nrow(); j++) {
    //  if (link_array(j, 1) == i) {
    //    composite_inlinks[i].push_back(j);
    //  }
    //}
  }
  
  // in-links to composite nodes
  std::map<int, std::vector<int> > composite_inlinks;
  // "visit_num([link])"
  //std::vector< std::vector<int> > inlink_visit_counts;
  std::map<int, std::map<int, int> > inlink_visit_counts;
  // "partialMFR([link])"
  std::map<int, std::map<int, route_set> > inlink_pmfr_maps;
  for (int i = 0; i < composite_nodes.size(); i++) {
    composite_inlinks[composite_nodes[i]];
    for (int j = 0; j < link_array.nrow(); j++) {
      if (link_array(j, 1) == composite_nodes[i]) {
        composite_inlinks[composite_nodes[i]].push_back(j);
      }
    }
    //std::vector<int> inlink_visit_count;
    std::map<int, int> inlink_visit_count;
    std::map<int, route_set> inlink_pmfr_map;
    for (int j = 0; j < composite_inlinks[i].size(); j++) {
      //inlink_visit_count.push_back(0);
      inlink_visit_count.insert(std::map<int, int>::value_type(
        composite_inlinks[i][j], 0
      ));
      inlink_pmfr_map[composite_inlinks[i][j]];
    }
    //inlink_visit_counts.push_back(inlink_visit_count);
    inlink_visit_counts.insert(std::map<int, std::map<int, int> >::value_type(
      composite_nodes[i], inlink_visit_count
    ));
    //inlink_pmfr_maps.push_back(inlink_pmfr_map);
    inlink_pmfr_maps.insert(std::map<int, std::map<int, route_set> >::value_type(
      composite_nodes[i], inlink_pmfr_map
    ));
  }
  
  // visiting the source node
  
  node_visit_count[source_node] = 1;
  for (int i = 0; i < link_array.nrow(); i++) {
    if (link_array(i, 0) == source_node) {
      link_stack.push(i);
    }
  }
  
  // visiting the lower branches
  
  while (link_stack.size() > 0) {
    
    // pop out the latest link
    int e = link_stack.top();
    link_stack.pop();
    int u = link_array(e, 0);
    int w = link_array(e, 1);
    
    // is 'w' a composite node?
    int *i = std::find(std::begin(composite_nodes),
                       std::end(composite_nodes),
                       w);
    int w_composite = (i != std::end(composite_nodes));
    // concatenate 'e' to the set of partial routes terminating at 'u'
    route_set temp_route_set = node_pmfr_map[u];
    //for (int j = 0; j < temp_route_set.size(); j++) {
    //  temp_route_set[j].insert(e);
    //}
    route_set::iterator it;
    for (it = temp_route_set.begin(); it != temp_route_set.end(); it++) {
      route temp_route = *it;
      temp_route.push_back(e);
    }
    
    std::map<int, int> w_inlink_visit_count;
    std::map<int, route_set> w_inlink_pmfr_map;
    // if 'w' is a composite node...
    if (w_composite) {
      // Temp1 and Temp2
      w_inlink_visit_count = inlink_visit_counts[w];
      w_inlink_pmfr_map = inlink_pmfr_maps[w];
      // increment the in-link visit count
      //link_visit_count[e] = link_visit_count[e] + node_visit_count[u];
      inlink_visit_counts[w][e] += node_visit_count[u];
      // append the concatenated routes to the appropriate route set
      inlink_pmfr_maps[w][e].insert(temp_route_set.begin(),
                                    temp_route_set.end());
    }
    
    // if 'w' is the sink node...
    if (w == target_node) {
      mfr_count += node_visit_count[u];
      mfr_set.insert(temp_route_set.begin(),
                     temp_route_set.end());
    } else {
      
      // have all links to 'w' been visited?
      int w_inlinks_visited = 1;
      for (int l = 0; l < link_array.nrow(); l++) {
        if (link_array(l, 1) == w) {
          if (inlink_visit_counts[w][l] == 0) {
            w_inlinks_visited = 0;
          }
        }
      }
      
      // if 'w' is original or all links to 'w' have been visited...
      if ((!w_composite) | (w_inlinks_visited)) {
        
        // append all links from 'w' to the link stack
        for (int l = 0; l < link_array.nrow(); l++) {
          if (link_array(l, 0) == w) {
            link_stack.push(l);
          }
        }
        
        // set the visit count and route set of 'w'
        if (!w_composite) {
          node_visit_count[w] = node_visit_count[u];
          // https://codeyarns.com/2011/01/11/c-insert-or-update-a-map/
          node_pmfr_map[w] = temp_route_set;
        } else {
          // visit_num(w) = Pi_v visit_num(e_{v,w}) - Pi_v Temp1(e_{v,w})
          int curr_prod = 1;
          int prev_prod = 1;
          for (int k = 0; k < inlink_visit_counts[w].size(); k++) {
            curr_prod *= inlink_visit_counts[w][composite_inlinks[w][k]];
            prev_prod *= w_inlink_visit_count[composite_inlinks[w][k]];
          }
          node_visit_count[w] = curr_prod - prev_prod;
          // set-difference:
          // partialMFR(w) = Otimes_v partialMFR(e_{v,w}) - Otimes_v Temp2(e_{v,w})
          route_set curr_combn;
          route_set prev_combn;
          for (int k = 0; k < inlink_pmfr_maps[w].size(); k++) {
            curr_combn.insert(inlink_pmfr_maps[w][composite_inlinks[w][k]].begin(),
                              inlink_pmfr_maps[w][composite_inlinks[w][k]].end());
            prev_combn.insert(w_inlink_pmfr_map[composite_inlinks[w][k]].begin(),
                              w_inlink_pmfr_map[composite_inlinks[w][k]].end());
          }
          //node_pmfr_map[w] = std::set_difference();
          node_pmfr_map[w] = curr_combn;
          route_set::iterator it;
          for (it = prev_combn.begin(); it != prev_combn.end(); it++) {
            route prev_route = *it;
            node_pmfr_map[w].erase(prev_route);
          }
        }
        
      }
      
    }
    
  }
  
  // return list
  return List::create(
    _["mfr_count"] = mfr_count,
    _["mfr_set"] = mfr_set
  );
}

// Algorithm 2 (Wang et al, 2013)


// Algorithm 3 (Wang et al, 2013)

