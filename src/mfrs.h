#include <Rcpp.h>
using namespace Rcpp;

//typedef int node_id;
//typedef int link_id;
typedef struct node_id {int id;} node_id;
typedef struct link_id {int id;} link_id;

typedef std::vector<link_id> route;
typedef std::set<route> route_set;
