#' Expand a graph with dependent links to one with composite nodes
#' 
#' The function \code{expand_graph()} takes as input an \strong{igraph} object 
#' and information on *link synergy* (either as a link attribute or as a 
#' separate list of link subsets) and returns the *expanded graph* as an 
#' \strong{igraph} object with a logical node attribute \code{"composite"} 
#' indicating which nodes are composite nodes.
#' 
#' The function \code{contract_graph()} takes as input an \strong{igraph} object
#' and information on *node composition* (either as a node attribute or as a 
#' separate vector of node IDs) and returns the *contracted graph* as an 
#' \strong{igraph} object with a logical link attribute \code{"synergy"} 
#' indicating which subsets of links are synergistic.
#' 
#' Only composite nodes are added or deleted, so node IDs are preserved. 
#' However, link IDs are not.
#' 
#' @template Wang2013
#'   
#' @import igraph
#' @param graph An object of class \code{"igraph"}.
#' @param synergy Either a character string indicating the attribute of 
#'   \code{graph} that encodes link synergy or a list of vectors of dependent 
#'   link IDs. If \code{NULL}, uses the first available link attribute.
#' @example inst/examples/ex-expand-contract.r
#' @export
expand_graph <- function(graph, synergy = NULL) {
  
  # if 'synergy' is NULL, use the first link attribute
  if (is.null(synergy)) {
    if (length(edge_attr(graph)) == 0) {
      warning("Synergy not provided.")
      return(graph)
    } else {
      synergy <- edge_attr_names(graph)[1]
    }
  }
  # if 'synergy' is a list, incorporate it as a link attribute
  if (!is.character(synergy)) {
    E(graph)$synergy <- NA
    E(graph)$synergy[unlist(synergy)] <- rep(
      1:length(synergy),
      each = sapply(synergy, length)
    )
    synergy <- "synergy"
  }
  
  # ensure that synergy is only among links to the same target node
  synergy_matrix <- cbind(
    ends(graph, E(graph), names = FALSE),
    as.numeric(factor(edge_attr(graph, synergy),
                      sort(unique(edge_attr(graph, synergy)))))
  )
  synergy_matrix <- synergy_matrix[stats::complete.cases(synergy_matrix), ]
  if (any(duplicated(unique(synergy_matrix[, 2:3])[, 2]))) {
    stop("Synergy between links to different target nodes.")
  }
  
  # introdue a composite node attribute
  n_nodes <- vcount(graph)
  V(graph)$composite <- FALSE
  # add composite nodes
  synergies <- setdiff(unique(edge_attr(graph, synergy)), NA)
  graph <- add_vertices(graph, length(synergies))
  V(graph)$composite[is.na(V(graph)$composite)] <- TRUE
  V(graph)$name[V(graph)$composite] <- paste0("c", 1:length(synergies))
  # associate each synergy with a composite node
  el <- rbind(
    # incoming links
    cbind(synergy_matrix[, 1], synergy_matrix[, 3] + n_nodes),
    # outgoing links
    sweep(unique(synergy_matrix[, c(3, 2)]), 2, c(n_nodes, 0), `+`)
  )
  graph <- add_edges(graph, as.vector(t(el)))
  # remove the dependent links
  graph <- delete_edges(graph, which(!is.na(edge_attr(graph, synergy))))
  graph <- delete_edge_attr(graph, synergy)
  
  graph
}

#' @param composite Either a character string indicating the attribute of
#'   \code{graph} that tags compposite nodes or a vector of the composite node
#'   IDs. If \code{NULL}, uses the first available node attribute.
#' @rdname expand_graph
#' @export
contract_graph <- function(graph, composite = NULL) {
  
  # if 'composite' is NULL, use the first logical node attribute
  if (is.null(composite)) {
    logical_attr <- sapply(vertex_attr(graph), is.logical)
    if (length(vertex_attr(graph)) == 0 | !any(logical_attr)) {
      warning("Composite nodes not provided.")
      return(graph)
    } else {
      composite <- vertex_attr_names(graph)[which(logical_attr)[1]]
    }
  }
  # if 'composite' is a vector, incorporate it as a node attribute
  if (!is.character(composite)) {
    V(graph)$composite <- 1:vcount(graph) %in% composite
    composite <- "composite"
  }
  
  # compile link data for composite nodes
  composite_matrix <- as.matrix(unname(suppressWarnings(merge(
    ends(graph,
         unlist(incident_edges(graph,
                               vertex_attr(graph, composite),
                               mode = "in")),
         names = FALSE),
    ends(graph,
         unlist(incident_edges(graph,
                               vertex_attr(graph, composite),
                               mode = "out")),
         names = FALSE),
    by.x = 2, by.y = 1
  ))))
  # ensure that composite nodes have only one out-link each to an original node
  if (any(composite_matrix[, 1] %in% composite_matrix[, 2:3])) {
    stop("Links between composite nodes.")
  }
  
  # add dependent links and add synergy link attribute
  n_links <- ecount(graph)
  graph <- add_edges(graph, as.vector(t(composite_matrix[, 2:3])))
  E(graph)$synergy <- c(rep(NA, n_links),
                        as.numeric(as.factor(composite_matrix[, 1])))
  # remove the composite nodes
  graph <- delete_vertices(graph, vertex_attr(graph, composite))
  graph <- delete_vertex_attr(graph, composite)
  
  graph
}
