#' Expand a graph with dependent links to one with composite nodes
#' 
#' This function takes as input an \strong{igraph} object and information on
#' *link synergy* (either as a link attribute or as a separate list of link
#' subsets) and returns a the *expanded graph* as an \strong{igraph} object with
#' a logical node attribute \code{"composite"} indicating which nodes are 
#' composite nodes. See Wang et al (2013) for definitions.
#' 
#' @references
#' Wang, R.-S., Sun, Z. & Albert, R. (2013) Minimal functional routes in 
#' directed graphs with dependent links. \emph{Intl. Trans. in Op. Res.}, 20, 
#' 391--409.
#' 
#' @import igraph
#' @param graph An object of class \code{"igraph"}.
#' @param synergy Either a character vector indicating the attribute of 
#'   \code{graph} that encodes link synergy or a list of vectors of dependent
#'   link IDs. If \code{NULL}, uses the first available link attribute.
#' @example inst/examples/examples.r
#' @export
expand_graph <- function(graph, synergy = NULL) {
  
  # If 'synergy' is NULL, use the first link attribute
  if (is.null(synergy)) {
    if (length(edge_attr(graph)) == 0) {
      stop("Synergy not provided and no link attributes to use.")
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
    as.numeric(factor(E(graph)$synergy, sort(unique(E(graph)$synergy))))
  )
  synergy_matrix <- synergy_matrix[stats::complete.cases(synergy_matrix), ]
  if (any(duplicated(unique(synergy_matrix[, 2:3])[, 2]))) {
    stop("Synergy between links to different target nodes.")
  }
  
  # introdue a composite node attribute
  n_graph <- vcount(graph)
  V(graph)$composite <- FALSE
  # add composite nodes and
  synergies <- setdiff(unique(E(graph)$synergy), NA)
  graph <- add_vertices(graph, length(synergies))
  V(graph)$composite[is.na(V(graph)$composite)] <- TRUE
  V(graph)$name[V(graph)$composite] <- paste0("c", 1:length(synergies))
  # associate each synergy with a composite node
  el <- rbind(
    # incoming links
    cbind(synergy_matrix[, 1], synergy_matrix[, 3] + n_graph),
    # outgoing links
    sweep(unique(synergy_matrix[, c(3, 2)]), 2, c(n_graph, 0), `+`)
  )
  graph <- add_edges(graph, as.vector(t(el)))
  # remove the dependent links
  graph <- delete_edges(graph, which(!is.na(E(graph)$synergy)))
  graph <- delete_edge_attr(graph, synergy)
  
  graph
}
