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

void print_innet(innet xs) {
  Rcout << xs.size() << " in-ego(s):" << std::endl;
  for (innet::iterator it = xs.begin();
       it != xs.end();
       it++) {
    inego x = *it;
    for (int i = 0; i < x.second.size(); i++) {
      Rcout << x.second[i];
      if (i < x.second.size() - 1) {
        Rcout << ",";
      }
    }
    Rcout << " -> " << x.first << std::endl;
  }
}

// Algorithm 1 (Wang et al, 2013)
// A depth-first search algorithm for enumerating MFRs in DAGs
// [[Rcpp::export]]
List mfrs_dfs_C(int node_count,
                IntegerMatrix link_array,
                IntegerVector composite_nodes,
                int source_node, int target_node,
                bool silent = true) {

  // setup

  // sort 'composite_nodes' and ensure that it has no duplicates
  std::sort(composite_nodes.begin(), composite_nodes.end());
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
      //Rcout << "node " << u << " route set " <<
      //  "with link " << e << " concatenated: ";
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
              route::iterator it;
              // remove adjacent duplicates
              it = std::unique(r.begin(), r.end());
              r.resize(std::distance(r.begin(), it));
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
// A subgraph-growing algorithm for enumerating MFRs in directed graphs
// [[Rcpp::export]]
List mfrs_sgg_C(int node_count,
                List invadj_list,
                LogicalVector node_composition,
                IntegerVector source_node, int target_node,
                bool silent = true) {

  // setup

  // net
  innet net;
  for (int v = 0; v < node_count; v++) {
    inego to_v;
    to_v.first = v;
    to_v.second = invadj_list[v];
    net.push_back(to_v);
  }
  if (!silent) {
    Rcout << std::endl << "Net complete: ";
    print_innet(net);
  }

  // pointer to the current partial MFR
  int pointer = 0;
  // number of MFRs
  int mfr_count = 1;
  // indicator that current partial MFR is complete
  bool flag = false;
  // vector of MFRs
  std::vector<innet> mfrs;
  // node at which to grow current partial MFR
  std::vector<int> tags;
  // initialize 'mfrs' and 'tags' with empty partial MFR and target node

  inego to_target;
  to_target.first = target_node;
  to_target.second = invadj_list[target_node];
  innet mfr;
  mfr.push_back(to_target);
  mfrs.push_back(mfr);
  if (!silent) {
    Rcout << std::endl << "Initial MFRs:" << std::endl;
    for (int i = 0; i < mfrs.size(); i++) {
      Rcout << "initial MFR #" << i << ":" << std::endl;
      print_innet(mfrs[i]);
    }
  }

  tags.push_back(0);

  while (pointer < mfr_count) {

    flag = false;
    bool missing_source = false;
    innet cmfr = mfrs[pointer];
    if (!silent) {
      Rcout << std::endl << "Current MFR: ";
      print_innet(cmfr);
    }
    int ctag = tags[pointer];

    while (!flag) {

      int cnode = cmfr[ctag].first;
      std::vector<int> cpred(cmfr[ctag].second);
      if (!silent) {
        Rcout << std::endl << "cnode = " << cnode << "; ";
        Rcout << "cpred = ";
        for (int i = 0; i < cpred.size(); i++) {
          if (i > 0) Rcout << ",";
          Rcout << cpred[i];
        }
        Rcout << std::endl;
      }

      if (cpred.size() == 0) {

        if (!silent) {
          Rcout << std::endl << "cmfr.size() = " << cmfr.size() << "; ";
          Rcout << "ctag = " << ctag << std::endl;
        }
        if (ctag == cmfr.size() - 1) {
          flag = true;
        } else {
          ctag++;
        }

      } else {

        if (node_composition[cnode] == false) {
          // split current MFR into many with one in-link each

          int m = cpred.size();
          if (!silent) {
            Rcout << std::endl << "m = " << m << std::endl;
          }

          cmfr[ctag].second.clear();
          cmfr[ctag].second.push_back(cpred[0]);
          //mfrs[pointer] = cmfr; // SHOULD BE UNNECESSARY
          mfrs.erase(mfrs.begin() + pointer);
          tags.erase(tags.begin() + pointer);

          for (int i = 0; i < m; i++) {
            innet temp1 = cmfr;
            temp1[ctag].second.clear();
            temp1[ctag].second.push_back(cpred[i]);
            if (!silent) {
              Rcout << std::endl << "Temp1 MFR: ";
              print_innet(temp1);
            }
            mfrs.insert(mfrs.begin() + pointer + i, temp1);
            tags.insert(tags.begin() + pointer + i, ctag);
          }

          mfr_count = mfr_count + m - 1;

          if (!silent) {
            Rcout << std::endl << "MFR count: " << mfr_count << std::endl;
            Rcout << std::endl << "MFRs:" << std::endl;
            for (int i = 0; i < mfrs.size(); i++) {
              Rcout << "MFR #" << i << ":" << std::endl;
              print_innet(mfrs[i]);
            }
          }
          if (mfr_count != mfrs.size()) {
            stop("MFR count disagrees with size of MFR vector.");
          }
        }

        if (!silent) {
          Rcout << std::endl << "cpred = ";
          for (int i = 0; i < cpred.size(); i++) {
            if (i > 0) Rcout << ",";
            Rcout << cpred[i];
          }
          Rcout << "; cmfr[ctag].second = ";
          for (int i = 0; i < cmfr[ctag].second.size(); i++) {
            if (i > 0) Rcout << ",";
            Rcout << cmfr[ctag].second[i];
          }
          Rcout << std::endl;
        }
        cpred = cmfr[ctag].second;

        bool all_done = true;

        bool chas;
        for (int i = 0; i < cpred.size(); i++) {

          int v = cpred[i];

          chas = false;
          for (int j = 0; j < cmfr.size(); j++) {
            if (cmfr[j].first == v) {
              chas = true;
            }
          }
          if (chas) continue;

          all_done = false;
          inego temp2 = net[v];

          //cmfr.insert(std::end(cmfr), std::begin(temp2), std::end(temp2));
          cmfr.push_back(temp2);
          if (!silent) {
            Rcout << std::endl << "Current MFR w/ Temp2: ";
            print_innet(cmfr);
          }

          // check whether `temp2.first` is a source node
          //bool is_source = temp2.first == source_node;
          bool is_source = false;
          for (int j = 0; j < source_node.size(); j++) {
            if (temp2.first == source_node[j]) {
              is_source = true;
            }
          }
          if ((temp2.second.size() == 0) & !is_source) {
            missing_source = true;
          }

        }

        if (all_done) {

          if (!silent) {
            Rcout << std::endl << "cmfr.size() = " << cmfr.size() << "; ";
            Rcout << "ctag = " << ctag << std::endl;
          }
          if (ctag == cmfr.size() - 1) {
            flag = true;
          } else {
            ctag++;
          }

        } else {

          ctag++;

        }

        // stop running if the partial MFR cannot become an actual MFR
        if (missing_source) {
          flag = true;
        }

      }

    }

    // drop this MFR if a source node is missing; otherwise add and increment
    if (missing_source) {
      mfrs.erase(mfrs.begin() + pointer);
      tags.erase(tags.begin() + pointer);
      mfr_count--;
    } else {
      mfrs[pointer] = cmfr;
      pointer++;
    }

    if (!silent) {
      Rcout << std::endl << "pointer = " << pointer << std::endl;
    }

  }

  if (!silent) {
    Rcout << std::endl << "Final MFRs:" << std::endl;
    for (int i = 0; i < mfrs.size(); i++) {
      Rcout << "final MFR #" << i << ":" << std::endl;
      print_innet(mfrs[i]);
    }
  }

  // require source node
  for (int i = mfrs.size() - 1; i >= 0; i--) {
    bool has_source = false;
    for (int j = 0; j < mfrs[i].size(); j++) {
      for (int k = 0; k < source_node.size(); k++) {
        if (mfrs[i][j].first == source_node[k]) {
          has_source = true;
        }
      }
    }
    if (!has_source) {
      mfrs.erase(mfrs.begin() + i);
      mfr_count--;
    }
  }

  // return list
  std::vector<egos> mfrs_egos;
  for (int i = 0; i < mfrs.size(); i++) {
    egos mfr_egos;
    std::transform(mfrs[i].begin(), mfrs[i].end(),
                   std::back_inserter(mfr_egos),
                   get_ego);
    mfrs_egos.push_back(mfr_egos);
  }
  List mfrs_links;
  for (int i = 0; i < mfrs.size(); i++) {
    innet mfr = mfrs[i];
    // number of links
    int link_count = 0;
    for (int j = 0; j < mfr.size(); j++) {
      link_count += mfr[j].second.size();
    }
    IntegerMatrix mfr_links(link_count, 2);
    // fill matrix with links
    int link_row = 0;
    for (int j = 0; j < mfr.size(); j++) {
      for (int k = 0; k < mfr[j].second.size(); k++) {
        mfr_links(link_row, 0) = mfr[j].second[k];
        mfr_links(link_row, 1) = mfr[j].first;
        link_row++;
      }
    }
    mfrs_links[std::to_string(i + 1)] = mfr_links;
  }
  return List::create(
    _["mfr_count"] = mfr_count,
    _["mfrs_links"] = mfrs_links
  );

}
