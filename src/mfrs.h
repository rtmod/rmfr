#include <Rcpp.h>
using namespace Rcpp;

//typedef int node_id;
//typedef int link_id;
typedef struct node_id {int id;} node_id;
typedef struct link_id {int id;} link_id;

//typedef std::vector<link_id> route;
//typedef std::set<route> route_set;
typedef std::vector<int> route;
typedef std::set<route> route_set;
typedef std::vector<route> route_vec;

//typedef std::map<int, std::vector<int> > invadj;
typedef std::pair<int, std::vector<int> > inego;
typedef std::vector<inego> innet;
typedef std::vector<int> egos;
int get_ego( const inego &p ) {
  return p.first;
}
