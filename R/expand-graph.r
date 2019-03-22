#' @title Expand a graph with dependent links to one with composite nodes
#'
#' @description These functions transform between graphs with synergistic links
#'   and "expanded" graphs with composite nodes that represent these synergies.
#'
#' @details
#'
#' The function `expand_graph()` takes as input an [igraph::igraph] object and
#' information on _link synergy_ (either as a link attribute or as a separate
#' list of link subsets) and returns the _expanded graph_ as an `"igraph"`
#' object with a logical node attribute `"composite"` indicating which nodes are
#' composite nodes.
#'
#' The function `contract_graph()` takes as input an `"igraph"` object and
#' information on _node composition_ (either as a node attribute or as a
#' separate vector of node IDs) and returns the _contracted graph_ as an
#' `"igraph"` object with a logical link attribute `"synergy"` indicating which
#' subsets of links are synergistic.
#'
#' Only composite nodes are added or deleted, so node IDs are preserved.
#' However, link IDs are not.
#'
#' @template ref-wang2013
#'
#' @import igraph
#' @param graph An [igraph::igraph] object.
#' @param synergy Either a character string indicating the attribute of `graph`
#'   that encodes link synergy or a list of vectors of dependent link IDs. If
#'   `NULL`, uses the first available link attribute.
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
  
  # initialize link permutation
  link_permutation <- 1:ecount(graph)
  
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
  synergies <- sort(setdiff(unique(edge_attr(graph, synergy)), NA))
  
  # complete link permutation
  nonsyn_shift_down <- cumsum(!is.na(edge_attr(graph, synergy)))
  link_permutation[is.na(edge_attr(graph, synergy))] <-
    (link_permutation - nonsyn_shift_down)[is.na(edge_attr(graph, synergy))]
  max_id <- ecount(graph) - length(which(!is.na(edge_attr(graph, synergy))))
  for (i in synergies) {
    wh <- which(edge_attr(graph, synergy) == i)
    link_permutation[wh] <- max_id + seq_along(wh)
    max_id <- max_id + length(wh)
  }
  
  # introdue a composite node attribute
  n_nodes <- vcount(graph)
  V(graph)$composite <- FALSE
  # add composite nodes
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
  
  # add link ID permutation as attribute
  graph <- set_graph_attr(graph, "link_permutation", link_permutation)
  
  graph
}

#' @param composite Either a character string indicating the attribute of
#'   `graph` that tags compposite nodes or a vector of the composite node
#'   IDs. If `NULL`, uses the first available node attribute.
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
