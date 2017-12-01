#' Recover minimal functional routes
#' 
#' This function takes a graph \code{graph} (understood to be expanded if 
#' necessary) with designated \code{source} and \code{target} nodes and finds 
#' the minimal functional routes (MFRs) from \code{source} to \code{target}.
#' 
#' @template Wang2013
#'   
#' @import igraph
#' @param graph An object of class \code{"igraph"}.
#' @param source,target Nodes of \code{graph}, as integer indices or character 
#'   names.
#' @param algorithm An algorithm from Wang et al (2013) from among the 
#'   following: \code{"dfs"} (depth-first search; Algorithm 1), \code{"ilp"}
#'   (iterative integer linear programming; Algorithm 2), or \code{"sgg"}
#'   (subgraph-growing; Algorithm 3). \strong{Currently only Algorithm 1 is
#'   implemented.}
#' @param silent Whether to print updates on the progress of the algorithm 
#'   (deprecated).
#' @param output Whether to return the list of MFRs as \code{"sequences"} of
#'   link IDs or as \code{"matrices"} of head and tail node IDs.
#' @example inst/examples/ex-get-mfrs.r
#' @seealso expand_graph
#' @export
get_mfrs <- function(
  graph,
  source, target,
  algorithm = NULL,
  silent = TRUE,
  output = "sequences"
) {
  output <- match.arg(output, c("sequences", "matrices"))
  
  graph_is_dag <- is_dag(graph)
  if (is.null(algorithm)) {
    algorithm <- if (graph_is_dag) "dfs" else "sgg"
  }
  algorithm <- match.arg(algorithm, c("dfs", "ilp", "sgg"))
  if (algorithm == "dfs" & !graph_is_dag) {
    warning("Depth-first search algorithm 'dfs' is proved only for DAGs.")
  }
  mfrs_fun <- get(paste0("mfrs_", algorithm))
  
  mfrs <- mfrs_fun(
    node_count = vcount(graph),
    link_array = as_edgelist(graph, names = FALSE),
    composite_nodes = which(vertex_attr(graph, "composite")),
    source_node = as.integer(V(graph)[source]),
    target_node = as.integer(V(graph)[target]),
    silent = silent
  )
  # THIS SHOULD BE DONE IN THE C++ SOURCE
  stopifnot(mfrs$mfr_count == length(mfrs$mfr_set))
  mfrs <- mfrs$mfr_set
  
  mfrs <- mfrs %>% lapply(function(x) x + 1)
  
  if (output == "matrices") {
    el <- as_edgelist(graph, names = FALSE)
    mfrs <- lapply(mfrs, function(x) {
      el[x, , drop = FALSE]
    })
  }
  
  mfrs
}

get_minimal_functional_routes <- get_mfrs
