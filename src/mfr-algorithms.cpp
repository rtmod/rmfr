#include <Rcpp.h>
#include <set>
#include <stack>
#include <map>
#include "mfrs.h"
using namespace Rcpp;

void print_route_set(route_set rs, IntegerMatrix la) {
  Rcout << rs.size() << " (partial) MFR(s):" << std::endl;
  for (route_set::iterator it = rs.begin();
       it != rs.end();
       it++) {
    route r = *it;
    for (int i = 0; i < r.size(); i++) {
      Rcout << " | " << r[i] << ": " << la(r[i], 0) << " -> " << la(r[i], 1);
    }
    Rcout << std::endl;
  }
}

// Algorithm 1 (Wang et al, 2013)
// A depth-first search algorithm for enumerating MFRs in DAGs
// [[Rcpp::export]]
List mfrs_dag(int node_count,
              IntegerMatrix link_array,
              IntegerVector composite_nodes,
              int source_node, int target_node,
              bool silent = true) {
  
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
  int node_visit_count[node_count];
  // "partialMFR([node])"
  std::map<int, route_set> node_pmfr_map;
  // initialize maps and fill in-link vectors
  for (int i = 0; i < node_count; i++) {
    node_visit_count[i] = 0;
    //route empty_route;
    route_set empty_route_set;
    //empty_route_set.insert(empty_route);
    if (i + 1 == source_node) {
      route empty_route;
      empty_route_set.insert(empty_route);
    }
    node_pmfr_map.insert(std::map<int, route_set>::value_type(
        i, empty_route_set
    ));
    if (!silent) {
      Rcout << "node " << i << ": ";
      print_route_set(node_pmfr_map[i], link_array);
    }
  }
  
  // in-links to composite nodes
  std::map<int, std::vector<int> > composite_inlinks;
  // "visit_num([link])"
  std::map<int, std::map<int,int> > inlink_visit_counts;
  // "partialMFR([link])"
  std::map<int, std::map<int, route_set> > inlink_pmfr_maps;
  for (int i = 0; i < composite_nodes.size(); i++) {
    composite_inlinks[composite_nodes[i]];
    for (int j = 0; j < link_array.nrow(); j++) {
      if (link_array(j, 1) == composite_nodes[i]) {
        composite_inlinks[composite_nodes[i]].push_back(j);
      }
    }
    std::map<int,int> inlink_visit_count;
    std::map<int, route_set> inlink_pmfr_map;
    for (int j = 0; j < composite_inlinks[composite_nodes[i]].size(); j++) {
      inlink_visit_count.insert(std::map<int,int>::value_type(
          composite_inlinks[composite_nodes[i]][j], 0
      ));
      route empty_route;
      route_set empty_route_set;
      //empty_route_set.insert(empty_route);
      inlink_pmfr_map.insert(std::map<int, route_set>::value_type(
          composite_inlinks[composite_nodes[i]][j], empty_route_set
      ));
      if (!silent) {
        Rcout << "in-link " << composite_inlinks[composite_nodes[i]][j] << ": ";
        print_route_set(inlink_pmfr_map[composite_inlinks[composite_nodes[i]][j]],
                        link_array);
      }
    }
    inlink_visit_counts.insert(std::map<int, std::map<int,int> >::value_type(
        composite_nodes[i], inlink_visit_count
    ));
    inlink_pmfr_maps.insert(std::map<int, std::map<int, route_set> >::value_type(
        composite_nodes[i], inlink_pmfr_map
    ));
  }
  
  // visiting the source node
  
  // set 'source_node' visit count to 1
  node_visit_count[source_node - 1] = 1;
  // append to 'link_stack' each link from 'source_node'
  // APPARENTLY THE ORDER REALLY MATTERS
  for (int i = link_array.nrow() - 1; i >= 0; i--) {
    if (link_array(i, 0) == source_node) {
      link_stack.push(i);
    }
  }
  
  // visiting the lower branches
  
  while (link_stack.size() > 0) {
    
    if (!silent) {
      Rcout << "... (pop link stack) ..." << std::endl;
    }
    
    // store the most recent link as 'e'
    int e = link_stack.top();
    // pop out 'e' from 'link_stack'
    link_stack.pop();
    int u = link_array(e, 0);
    int w = link_array(e, 1);
    if (!silent) {
      Rcout << "link e = " << e << "; nodes u = " << u << ", w = " << w <<
        std::endl;
    }
    
    // is 'w' a composite node?
    int *i = std::find(std::begin(composite_nodes),
                       std::end(composite_nodes),
                       w);
    int w_composite = (i != std::end(composite_nodes));
    // concatenate 'e' to the set of partial routes terminating at 'u'
    if (!silent) {
      //Rcout << "node " << u << " route set: ";
      //print_route_set(node_pmfr_map[u - 1], link_array);
    }
    route_set u_pmfrs_e;
    route_set::iterator it;
    for (it = node_pmfr_map[u - 1].begin();
         it != node_pmfr_map[u - 1].end();
         it++) {
      route r = *it;
      r.push_back(e);
      u_pmfrs_e.insert(r);
    }
    if (!silent) {
      //Rcout << "node " << u << " route set with link " << e << " concatenated: ";
      //print_route_set(u_pmfrs_e, link_array);
    }
    
    std::map<int,int> w_inlink_visit_count;
    std::map<int, route_set> w_inlink_pmfr_map;
    
    // if 'w' is a composite node...
    if (w_composite) {
      
      // Temp1 and Temp2
      w_inlink_visit_count = inlink_visit_counts[w];
      w_inlink_pmfr_map = inlink_pmfr_maps[w];
      
      // increment the in-link visit count
      inlink_visit_counts[w][e] += node_visit_count[u - 1];
      
      // append the concatenated routes to the appropriate route set
      route_set we_inlink_pmfrs = inlink_pmfr_maps[w][e];
      route_set::iterator it;
      for (it = u_pmfrs_e.begin();
           it != u_pmfrs_e.end();
           it++) {
        route r = *it;
        we_inlink_pmfrs.insert(r);
      }
      inlink_pmfr_maps[w][e] = we_inlink_pmfrs;
      if (!silent) {
        //Rcout << "node " << w << " in-link " << e << " route set: ";
        //print_route_set(inlink_pmfr_maps[w][e], link_array);
      }
      
    }
    
    // if 'w' is the sink node...
    if (w == target_node) {
      
      mfr_count += node_visit_count[u - 1];
      mfr_set.insert(u_pmfrs_e.begin(), u_pmfrs_e.end());
      if (!silent) {
        Rcout << "'mfr_count' = " << mfr_count << std::endl;
        Rcout << "MFR set: ";
        print_route_set(mfr_set, link_array);
      }
      
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
        for (int l = link_array.nrow() - 1; l >= 0; l--) {
          if (link_array(l, 0) == w) {
            link_stack.push(l);
          }
        }
        
        // set the visit count and route set of 'w'
        if (!w_composite) {
          
          // reset 'partialMFR' at 'w' to the concatenated 'partialMFR' at 'u'
          if (!silent) {
            //Rcout << "old node " << w << " route set: ";
            //print_route_set(node_pmfr_map[w - 1], link_array);
          }
          node_visit_count[w - 1] = node_visit_count[u - 1];
          route_set w_node_pmfrs;
          route_set::iterator it;
          for (it = u_pmfrs_e.begin();
               it != u_pmfrs_e.end();
               it++) {
            route r = *it;
            w_node_pmfrs.insert(r);
          }
          node_pmfr_map[w - 1] = w_node_pmfrs;
          if (!silent) {
            //Rcout << "new node " << w << " route set: ";
            //print_route_set(node_pmfr_map[w - 1], link_array);
          }
          
        } else {
          
          if (!silent) {
            //Rcout << "all node " << w << " in-links visited." << std::endl;
          }
          int curr_prod = 1;
          int prev_prod = 1;
          for (int i = 0; i < inlink_visit_counts[w].size(); i++) {
            curr_prod *= inlink_visit_counts[w][composite_inlinks[w][i]];
            prev_prod *= w_inlink_visit_count[composite_inlinks[w][i]];
          }
          node_visit_count[w - 1] = curr_prod - prev_prod;
          if (!silent) {
            Rcout << "product difference: " << curr_prod << " - " <<
              prev_prod << " = " << node_visit_count[w - 1] << std::endl;
          }
          
          // set-difference:
          // pMFR(w) = Otimes_v pMFR(e_{v,w}) - Otimes_v Temp2(e_{v,w})
          
          if (!silent) {
            Rcout << "all node " << w << " in-link route sets:" << std::endl;
            for (int i = 0; i < inlink_pmfr_maps[w].size(); i++) {
              int il = composite_inlinks[w][i];
              Rcout << "in-link " << il << ": " <<
                link_array(il, 0) << " -> " << link_array(il, 1) << ": ";
              print_route_set(inlink_pmfr_maps[w][il], link_array);
            }
          }
          // combine pairs of PMFRs, one through each link to 'w'
          // NEED TO ALLOW FOR MORE THAN TWO IN-LINKS
          route_set::iterator it0;
          route_set::iterator it1;
          route_set curr_combn;
          for (it0 = inlink_pmfr_maps[w][composite_inlinks[w][0]].begin();
               it0 != inlink_pmfr_maps[w][composite_inlinks[w][0]].end();
               it0++) {
            route r0 = *it0;
            for (it1 = inlink_pmfr_maps[w][composite_inlinks[w][1]].begin();
                 it1 != inlink_pmfr_maps[w][composite_inlinks[w][1]].end();
                 it1++) {
              route r1 = *it1;
              route r = r0;
              r.insert(r.end(), r1.begin(), r1.end());
              std::sort(r.begin(), r.end());
              curr_combn.insert(r);
            }
          }
          if (!silent) {
            Rcout << "current combination at w = " << w << ": ";
            print_route_set(curr_combn, link_array);
          }
          route_set prev_combn;
          for (it0 = w_inlink_pmfr_map[composite_inlinks[w][0]].begin();
               it0 != w_inlink_pmfr_map[composite_inlinks[w][0]].end();
               it0++) {
            route r0 = *it0;
            for (it1 = w_inlink_pmfr_map[composite_inlinks[w][1]].begin();
                 it1 != w_inlink_pmfr_map[composite_inlinks[w][1]].end();
                 it1++) {
              route r1 = *it1;
              route r = r0;
              r.insert(r.end(), r1.begin(), r1.end());
              std::sort(r.begin(), r.end());
              prev_combn.insert(r);
            }
          }
          if (!silent) {
            Rcout << "previous combination at w = " << w << ": ";
            print_route_set(prev_combn, link_array);
          }
          if (0) {
            route_set curr_combn;
            route_set prev_combn;
            for (int i = 0; i < inlink_pmfr_maps[w].size(); i++) {
              curr_combn.insert(
                inlink_pmfr_maps[w][composite_inlinks[w][i]].begin(),
                inlink_pmfr_maps[w][composite_inlinks[w][i]].end()
              );
              prev_combn.insert(
                w_inlink_pmfr_map[composite_inlinks[w][i]].begin(),
                w_inlink_pmfr_map[composite_inlinks[w][i]].end()
              );
            }
          }
          if (!silent) {
            //Rcout << "current route set: " << curr_combn.size() << std::endl;
            //Rcout << "previous route set: " << prev_combn.size() << std::endl;
          }
          route_set::iterator it;
          for (it = prev_combn.begin(); it != prev_combn.end(); it++) {
            route r = *it;
            curr_combn.erase(r);
          }
          node_pmfr_map[w - 1] = curr_combn;
          if (!silent) {
            Rcout << "updated (set-difference) node " << w << " route set: ";
            print_route_set(node_pmfr_map[w - 1], link_array);
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

