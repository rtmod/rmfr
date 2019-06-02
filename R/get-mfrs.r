#' @title Recover minimal functional routes
#'
#' @description The function `get_mfrs()` takes a graph `graph` (by default
#'   assumed to be expanded) with designated `source` and `target` nodes and
#'   finds the minimal functional routes (MFRs) from `source` to `target`.
#'   `get_minimal_functional_routes()` is an alias. The function
#'   `get_minimal_paths` trivializes the functional annotation of a graph and
#'   uses `get_mfrs()` to find minimal paths.
#'
#' @template ref-wang2013
#'   

#' @name mfrs
#' @import igraph
#' @param graph An [igraph::igraph] object.
#' @param source,target Vector of nodes of `graph`, as class `"igraph.vs"`,
#'   integer indices, or character names. `source` must have in-degree zero.
#' @param method An algorithm from Wang et al (2013) from among the following:
#'   `"dfs"` (depth-first search; Algorithm 1), `"ilp"` (iterative integer
#'   linear programming; Algorithm 2), or `"sgg"` (subgraph-growing; Algorithm
#'   3). **Currently Algorithms 1 and 3 are implemented in C++; Algorithm 3 also
#'   has an implementation in R, employed by passing `"sggR"`.**
#' @param expand Whether to expand `graph` before finding the MFRs and, if so,
#'   to return the list of MFRs in terms of the node and link IDs of `graph`
#'   rather than of its expanded graph. If `NULL`, `graph` will be expanded only
#'   if it has a `"synergy"` link attribute.
#' @param add.source Whether to add to `graph` a source node with a link to each
#'   node passed to `source`, and to treat this new node as the source. This may
#'   simplify target and allows to use the subgraph-growing algorithm when some
#'   source nodes are not themselves sources. If `NULL`, the source node will be
#'   added only in case the subgraph-growing algorithm is called and at least
#'   one `source` node is not a source.
#' @param silent Whether to print updates on the progress of the algorithm
#'   (deprecated).
#' @param format Whether to return the list of MFRs as vectors of edge IDs
#'   (`"indices"`), as edge sequences from `graph` (`"sequences"`), or as
#'   matrices of head and tail node IDs (`"matrices"`).
#' @example inst/examples/ex-mfrs.r
#' @example inst/examples/ex-minimal-paths.r
#' @seealso expand_graph
NULL

#' @rdname mfrs
#' @export
get_mfrs <- function(
  graph,
  source, target,
  method = NULL,
  expand = NULL, add.source = NULL,
  silent = TRUE,
  format = "indices"
) {
  format <- match.arg(format, c("indices", "sequences", "matrices"))
  
  # if not instructed, decide whether to expand based on link attributes
  if (is.null(expand)) {
    expand <- ! is.null(edge_attr(graph, "synergy"))
    if (expand && ! is.null(vertex_attr(graph, "composite"))) warning(
      "`graph` has both a 'composite' node attribute and",
      "a 'synergy' link attribute; it will be expanded.",
      immediate. = TRUE
    )
  }
  if (expand) {
    graph_orig <- graph
    graph <- expand_graph(graph)
  }
  
  graph_is_dag <- is_dag(graph)
  if (is.null(method)) {
    method <- if (graph_is_dag) "dfs" else "sgg"
  }
  method <- match.arg(method, c("dfs", "ilp", "sgg", "sggR"))
  if (method == "dfs" & ! graph_is_dag) {
    #warning(
    #  "Depth-first search algorithm 'dfs' is proved only for DAGs.",
    #  immediate. = TRUE
    #)
    ans <- utils::menu(c("yes", "no"), title = c(
      "Depth-first search algorithm 'dfs' is proved only for DAGs. ",
      "Are you sure you want to continue?"
    ))
    if (ans == 2) {
      warning("Avoided running DFS on a non-DAG; returning `NULL`.")
      return(NULL)
    }
  }
  mfrs_fun <- get(paste0("mfrs_", method))
  
  # check that source node(s) have in-degree zero
  if (method == "sgg" && any(degree(graph, source, "in") > 0)) {
    if (is.null(add.source)) {
      add.source <- TRUE
    } else if (! add.source) {
      stop(
        "Subgraph-growing algorithm 'sgg' requires",
        "`source` node to have out-degree zero."
      )
    }
  } else if (is.null(add.source)) {
    add.source <- FALSE
  }
  if (add.source) {
    graph <- add_vertices(graph, 1, attr = list(name = "rmfr_source"))
    source_names <- names(V(graph)[source])
    graph <- add_edges(graph, unlist(rbind("rmfr_source", source_names)))
    source <- "rmfr_source"
  }
  
  # calculate MFRs using the specified algorithm
  mfrs <- mfrs_fun(graph, source, target, silent)
  
  # check and correction for C++ implementations
  if (method == "dfs") {
    stopifnot(mfrs$mfr_count == length(mfrs$mfr_set))
    mfrs <- lapply(mfrs$mfr_set, function(x) x + 1)
  } else if (method == "sgg") {
    stopifnot(mfrs$mfr_count == length(mfrs$mfrs_links))
    mfrs <- mfrs$mfrs_links %>% unname() %>%
      lapply(function(x) x + 1) %>%
      lapply(t) %>% lapply(as.vector) %>%
      lapply(get.edge.ids, graph = graph) %>%
      lapply(sort)
  }
  
  # correct MFRs to original graph
  if (add.source) {
    mfrs <- lapply(
      mfrs,
      function(mfr) mfr[mfr <= ecount(graph) - length(source_names)]
    )
  }
  if (expand) {
    mfrs <- mfrs %>%
      lapply(match, table = graph_attr(graph, "link_permutation")) %>%
      lapply(function(x) x[!is.na(x)])
    graph <- graph_orig
  }
  
  # return MFRs in desired format
  if (format == "matrices") {
    return(lapply(mfrs, get.edges, graph = graph))
  } else if (format == "sequences") {
    # no idea why `lapply()` doesn't work here
    for (i in seq_along(mfrs)) mfrs[[i]] <- E(graph)[mfrs[[i]]]
    return(mfrs)
  } else {
    return(mfrs)
  }
}

#' @rdname mfrs
#' @export
get_minimal_functional_routes <- get_mfrs

#' @rdname mfrs
#' @export
get_minimal_paths <- function(
  graph,
  source, target,
  method = NULL,
  add.source = NULL,
  silent = TRUE,
  format = "indices"
) {
  graph <- set_edge_attr(graph, "synergy", value = NA)
  graph <- set_vertex_attr(graph, "composite", value = FALSE)
  get_mfrs(
    graph = graph, source = source, target = target, method = method,
    expand = FALSE, add.source = add.source, silent = silent, format = format
  )
}

# depth-first search algorithm (Algorithm 1)
mfrs_dfs <- function(graph, source, target, silent) {
  mfrs_dfs_C(
    node_count = vcount(graph),
    link_array = as_edgelist(graph, names = FALSE),
    composite_nodes = which(vertex_attr(graph, "composite")),
    source_node = as.integer(V(graph)[source]),
    target_node = as.integer(V(graph)[target]),
    silent = silent
  )
}

# subgraph-growing algorithm (Algorithm 3)
mfrs_sgg <- function(graph, source, target, silent) {
  mfrs_sgg_C(
    node_count = vcount(graph),
    invadj_list = lapply(adjacent_vertices(graph, v = V(graph), mode = "in"),
                         function(x) as.numeric(x) - 1),
    node_composition = vertex_attr(graph, "composite"),
    source_node = as.integer(V(graph)[source]) - 1L,
    target_node = as.integer(V(graph)[target]) - 1L,
    silent = silent
  )
}

# rough implementation in R, to be later implemented in C++
mfrs_sggR <- function(graph, source, target, silent) {
  
  # setup
  source <- as.integer(V(graph)[source])
  target <- as.integer(V(graph)[target])
  source_nodes <- as.integer(V(graph)[degree(graph, mode = "in") == 0])
  composite_nodes = which(vertex_attr(graph, "composite"))
  if ("name" %in% vertex_attr_names(graph)) {
    graph <- delete_vertex_attr(graph, "name")
  }
  net <- list(
    as.numeric(V(graph)),
    unname(lapply(as_adj_list(graph, "in"), as.numeric))
  )
  if (!silent) {
    print("Net complete:")
    print(net)
  }
  
  # initialization
  pointer <- 1
  n_mfrs <- 1
  flag <- FALSE
  mfrs <- list(list(net[[1]][target], net[[2]][target]))
  tag <- c(1)
  if (!silent) {
    print("Initial MFRs:")
    for (mfr in mfrs) {
      print(mfr)
    }
  }
  
  # while some partial MFRs remain
  while (pointer <= n_mfrs) {
    
    flag <- FALSE
    cmfr <- mfrs[[pointer]]
    ctag <- tag[pointer]
    if (!silent) {
      print("Current MFR:")
      print(cmfr)
    }
    
    # while the current MFR is incomplete
    while (flag == FALSE) {
      
      cnode <- cmfr[[1]][[ctag]]
      cpred <- cmfr[[2]][[ctag]]
      if (!silent) {
        print(paste0("cnode = ", cnode, "; cpred = ",
                     paste(cpred, collapse = ",")))
      }
      
      # if no predecessors remain,
      # then either increment the tag or mark the current partial MFR complete
      if (length(cpred) == 0) {
        
        # if the current node is the last node of the current partial MFR
        if (!silent) {
          print(paste0("length(cmfr) = ", length(cmfr), "; ctag = ", ctag))
        }
        if (ctag == length(cmfr[[1]])) {
          flag <- TRUE
        } else {
          ctag <- ctag + 1
        }
        
      } else {
        
        # if the current node is an original node
        # (don't update the current tag because the current partial MFR is
        # replaced by the first of the new partial MFRs)
        if (!(cnode %in% composite_nodes)) {
          
          m <- length(cpred)
          if (!silent) {
            print(paste0("m = ", m))
          }
          cmfr[[2]][[ctag]] <- cpred[1]
          
          i <- 0
          dmfrs <- list() ###
          while (i < m) {
            temp1 <- cmfr
            temp1[[2]][[ctag]] <- cpred[i + 1]
            #mfrs[[n_mfrs + i]] <- temp1 ### SHOULD THIS BE 'pointer'?
            dmfrs[[i + 1]] <- temp1 ###
            #tag[n_mfrs + i] <- ctag
            if (!silent) {
              print("Temp1 MFR:")
              print(temp1)
            }
            i <- i + 1
          }
          # instert new MFRs at the location of the current one
          mfrs <- c(
            if (pointer > 1) mfrs[1:(pointer - 1)] else NULL,
            dmfrs,
            if (n_mfrs > pointer) mfrs[(pointer + 1):n_mfrs] else NULL
          ) ###
          tag <- c(
            if (pointer > 1) tag[1:(pointer - 1)] else NULL,
            rep(ctag, m),
            if (n_mfrs > pointer) tag[(pointer + 1):n_mfrs] else NULL
          ) ###
          n_mfrs <- n_mfrs + m - 1
          if (!silent) {
            print(paste("MFR count:", n_mfrs))
            print("MFRs:")
            for (mfr in mfrs) print(mfr)
          }
          
        }
        
        if (!silent) {
          print(paste0("cpred = ", paste(cpred, collapse = ","), "; ",
                       "cmfr[[2]][[ctag]] = ",
                       paste(cmfr[[2]][[ctag]], collapse = ",")))
        }
        cpred <- cmfr[[2]][[ctag]]
        
        # if all current predecessors are in the current partial MFR,
        # and once all nodes in the current partial MFR have been exhausted,
        # then the current partial MFR is considered complete;
        # if some current predecessors are not in the current partial MFR,
        # then append the current MFR with the missing predecessors
        if (all(cpred %in% cmfr[[1]])) {
          
          if (ctag == length(cmfr[[1]])) {
            flag <- TRUE
          } else {
            ctag <- ctag + 1
          }
          if (!silent) {
            print(paste0("length(cmfr) = ", length(cmfr), "; ctag = ", ctag))
          }
          
        } else {
          
          for (v in cpred) {
            if (v %in% cmfr[[1]]) next
            i <- which(net[[1]] == v)
            temp2 <- list(net[[1]][i], net[[2]][i])
            cmfr[[1]] <- c(cmfr[[1]], temp2[[1]])
            cmfr[[2]] <- c(cmfr[[2]], temp2[[2]])
            if (!silent) {
              print("Current MFR w/ Temp2:")
              print(cmfr)
            }
          }
          ctag <- ctag + 1
          
        }
        
      }
      
    }
    
    mfrs[[pointer]] <- cmfr ###
    pointer <- pointer + 1
    if (!silent) {
      print(paste0("pointer = ", pointer))
    }
    
  }
  
  if (!silent) {
    print("Final MFRs:")
    for (mfr in mfrs) print(mfr)
  }
  # require that the source node is among the MFR nodes
  #mfrs <- mfrs[which(!is.na(sapply(mfrs, function(mfr) {
  #  match(source, mfr[[1]])
  #})))]
  # require that all source nodes in each MFR have in-degree zero
  source_indeg0 <- sapply(mfrs, function(mfr) {
    all(intersect(source_nodes, mfr[[1]]) %in% source)
  })
  mfrs <- mfrs[which(source_indeg0)]
  
  # result as a list of edge sequences
  lapply(mfrs, function(m) {
    sort(unlist(mapply(function(v0, v1s) {
      if (length(v1s) == 0) return(NULL)
      get.edge.ids(graph, rbind(v1s, v0))
    }, m[[1]], m[[2]], SIMPLIFY = FALSE)))
  })
}
