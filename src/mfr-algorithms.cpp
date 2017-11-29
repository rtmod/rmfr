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
  int node_visit_count[node_count];
  // "partialMFR([node])"
  std::map<int, route_set> node_pmfr_map;
  // initialize maps and fill in-link vectors
  for (int i = 0; i < node_count; i++) {
    node_visit_count[i] = 0;
    route empty_route;
    route_set empty_route_set;
    empty_route_set.insert(empty_route);
    node_pmfr_map.insert(std::map<int, route_set>::value_type(
        i, empty_route_set
    ));
    Rcout << "node " << i << " pMFR size = " <<
      node_pmfr_map[i].size() << std::endl;
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
      empty_route_set.insert(empty_route);
      inlink_pmfr_map.insert(std::map<int, route_set>::value_type(
          composite_inlinks[composite_nodes[i]][j], empty_route_set
      ));
      Rcout << "in-link " << composite_inlinks[composite_nodes[i]][j] <<
        " pMFR size = " <<
          inlink_pmfr_map[composite_inlinks[composite_nodes[i]][j]].size() <<
            std::endl;
    }
    inlink_visit_counts.insert(std::map<int, std::map<int,int> >::value_type(
        composite_nodes[i], inlink_visit_count
    ));
    inlink_pmfr_maps.insert(std::map<int, std::map<int, route_set> >::value_type(
        composite_nodes[i], inlink_pmfr_map
    ));
  }
  
  if (0) {
    Rcout << "in-link visit counts:" << std::endl;
    for (int i = 0; i < composite_nodes.size(); i++) {
      int cn = composite_nodes[i];
      for (std::map<int,int>::iterator it = inlink_visit_counts[cn].begin();
           it != inlink_visit_counts[cn].end();
           ++it) {
        Rcout << it->first << " => " << it->second << std::endl;
      }
    }
  }
  
  // visiting the source node
  
  // set 'source_node' visit count to 1
  node_visit_count[source_node - 1] = 1;
  // append to 'link_stack' each link from 'source_node'
  for (int i = 0; i < link_array.nrow(); i++) {
    if (link_array(i, 0) == source_node) {
      link_stack.push(i);
    }
  }
  
  // visiting the lower branches
  
  while (link_stack.size() > 0) {
    
    if (0) {
      //Rcout << link_stack << std::endl;
      Rcout << "link stack: ";
      std::stack<int> temp;
      temp = link_stack;
      while (!temp.empty()) {
        int w = temp.top();
        Rcout << w << " ";
        temp.pop();
      }
      Rcout << std::endl;
      
      Rcout << "node visit counts: ";
      for (int i = 0; i < node_count; i++) {
        Rcout << node_visit_count[i] << " ";
      }
      Rcout << std::endl;
    }
    
    // pop out the latest link
    int e = link_stack.top();
    link_stack.pop();
    int u = link_array(e, 0);
    int w = link_array(e, 1);
    Rcout << "link e = " << e << "; nodes u = " << u << ", w = " << w << std::endl;
    
    // is 'w' a composite node?
    int *i = std::find(std::begin(composite_nodes),
                       std::end(composite_nodes),
                       w);
    int w_composite = (i != std::end(composite_nodes));
    // concatenate 'e' to the set of partial routes terminating at 'u'
    //route_set temp_route_set = node_pmfr_map[u - 1];
    route_set temp_route_set;
    route_set::iterator it;
    Rcout << "'temp_route_set' route lengths:" << std::endl;
    for (it = node_pmfr_map[u - 1].begin();
         it != node_pmfr_map[u - 1].end();
         it++) {
      route r = *it;
      Rcout << r.size();
      r.push_back(e);
      Rcout << " (" << r.size() << ") ";
      temp_route_set.insert(r);
    }
    Rcout << std::endl;
    Rcout << "'temp_route_set' size: " << temp_route_set.size() << std::endl;
    
    std::map<int,int> w_inlink_visit_count;
    std::map<int, route_set> w_inlink_pmfr_map;
    
    // if 'w' is a composite node...
    if (w_composite) {
      // Temp1 and Temp2
      w_inlink_visit_count = inlink_visit_counts[w];
      w_inlink_pmfr_map = inlink_pmfr_maps[w];
      // increment the in-link visit count
      //int we_inlink_count = inlink_visit_counts[w][e] + node_visit_count[u - 1];
      //inlink_visit_counts[w][e] = we_inlink_count;
      inlink_visit_counts[w][e] += node_visit_count[u - 1];
      
      // append the concatenated routes to the appropriate route set
      route_set we_inlink_pmfrs;
      route_set::iterator it;
      for (it = inlink_pmfr_maps[w][e].begin();
           it != inlink_pmfr_maps[w][e].end();
           it++) {
        route r = *it;
        r.push_back(e);
        we_inlink_pmfrs.insert(r);
      }
      //route_set we_inlink_pmfrs = inlink_pmfr_maps[w][e];
      //we_inlink_pmfrs.insert(temp_route_set.begin(),
      //                       temp_route_set.end());
      inlink_pmfr_maps[w][e] = we_inlink_pmfrs;
      
      if (0) {
        Rcout << "in-link visit counts:" << std::endl;
        for (int i = 0; i < composite_nodes.size(); i++) {
          int cn = composite_nodes[i];
          for (std::map<int,int>::iterator it = inlink_visit_counts[cn].begin();
               it != inlink_visit_counts[cn].end();
               ++it) {
            Rcout << it->first << " => " << it->second << std::endl;
          }
        }
      }
      
    }
    
    // if 'w' is the sink node...
    if (w == target_node) {
      
      mfr_count += node_visit_count[u - 1];
      Rcout << "'mfr_count' = " << mfr_count << std::endl;
      mfr_set.insert(temp_route_set.begin(),
                     temp_route_set.end());
      Rcout << "'mfr_set' size = " << mfr_set.size() << std::endl;
      
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
      //Rcout << "all in-links visited: " << w_inlinks_visited << std::endl;
      
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
          
          // reset 'partialMFR' at 'w' to the concatenated 'partialMFR' at 'u'
          node_visit_count[w - 1] = node_visit_count[u - 1];
          route_set w_node_pmfrs = node_pmfr_map[u - 1];
          route_set::iterator it;
          for (it = w_node_pmfrs.begin();
               it != w_node_pmfrs.end();
               it++) {
            route r = *it;
            r.push_back(e);
            Rcout << "route length: " << r.size() << std::endl;
          }
          node_pmfr_map[w - 1] = w_node_pmfrs;
          //node_pmfr_map[w - 1] = temp_route_set;
          
        } else {
          
          // visit_num(w) = Pi_v visit_num(e_{v,w}) - Pi_v Temp1(e_{v,w})
          int curr_prod = 1;
          int prev_prod = 1;
          for (int i = 0; i < inlink_visit_counts[w].size(); i++) {
            curr_prod *= inlink_visit_counts[w][composite_inlinks[w][i]];
            prev_prod *= w_inlink_visit_count[composite_inlinks[w][i]];
          }
          node_visit_count[w - 1] = curr_prod - prev_prod;
          Rcout << "product difference: " << curr_prod << " - " <<
            prev_prod << " = " << node_visit_count[w - 1] << std::endl;
          
          // set-difference:
          // pMFR(w) = Otimes_v pMFR(e_{v,w}) - Otimes_v Temp2(e_{v,w})
          
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
              Rcout << "add to 'curr_combn': ";
              for (int i = 0; i < r.size(); i++) {
                Rcout << r[i] << " ";
              }
              Rcout << std::endl;
            }
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
              Rcout << "add to 'prev_combn': ";
              for (int i = 0; i < r.size(); i++) {
                Rcout << r[i] << " ";
              }
              Rcout << std::endl;
            }
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
          Rcout << "current route set: " << curr_combn.size() << std::endl;
          Rcout << "previous route set: " << prev_combn.size() << std::endl;
          route_set::iterator it;
          for (it = prev_combn.begin(); it != prev_combn.end(); it++) {
            route r = *it;
            curr_combn.erase(r);
          }
          node_pmfr_map[w - 1] = curr_combn;
          Rcout << "new node_pmfr_map[w-1]: " << std::endl;
          for (it = node_pmfr_map[w - 1].begin();
               it != node_pmfr_map[w - 1].end();
               it++) {
            route r = *it;
            for (int i = 0; i < r.size(); i++) {
              Rcout << r[i] << " ";
            }
            Rcout << std::endl;
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

