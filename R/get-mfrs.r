#' Recover minimal functional routes
#'
#' This function takes a graph \code{graph} (by default assumed to be expanded)
#' with designated \code{input} and \code{output} nodes and finds the minimal
#' functional routes (MFRs) from \code{input} to \code{output}.
#'
#' @template Wang2013
#'

#' @import igraph
#' @param graph An object of class \code{"igraph"}.
#' @param input,output Nodes of \code{graph}, as \strong{igraph} vertices,
#'   integer indices, or character names. \code{input} must have in-degree zero.
#' @param method An algorithm from Wang et al (2013) from among the
#'   following: \code{"dfs"} (depth-first search; Algorithm 1), \code{"ilp"}
#'   (iterative integer linear programming; Algorithm 2), or \code{"sgg"}
#'   (subgraph-growing; Algorithm 3). \strong{Currently Algorithms 1 and 3 are
#'   implemented in C++; Algorithm 3 also has an implementation in R, employed
#'   by passing \code{"sggR"}.}
#' @param expand Whether to expand \code{graph} before finding the MFRs and, if
#'   so, to return the list of MFRs in terms of the node and link IDs of
#'   \code{graph} rather than of its expanded graph. If \code{NULL},
#'   \code{graph} will be expanded only if it has a \code{'synergy'} link
#'   attribute.
#' @param add.source Whether to add to \code{graph} a source node with a link to
#'   each node passed to \code{input}, and to treat this new node as the input.
#'   This may simplify output and allows to use the subgraph-growing algorithm
#'   when some input nodes are not themselves sources. If \code{NULL}, the
#'   source node will be added only in case the subgraph-growing algorithm is
#'   called and at least one \code{input} node is not a source.
#' @param silent Whether to print updates on the progress of the algorithm
#'   (deprecated).
#' @param format Whether to return the list of MFRs as \code{"sequences"} of
#'   link IDs or as \code{"matrices"} of head and tail node IDs.
#' @param source,target Deprecated; aliases of \code{input} and \code{output}.
#' @param algorithm Deprecated; alias of \code{method}.
#' @example inst/examples/ex-get-mfrs.r
#' @seealso expand_graph
#' @export
get_mfrs <- function(
  graph,
  input, output,
  method = NULL,
  expand = NULL, add.source = NULL,
  silent = TRUE,
  format = "sequences",
  source = NULL, target = NULL, algorithm = NULL
) {
  format <- match.arg(format, c("sequences", "matrices"))
  
  if (! is.null(source) | ! is.null(target)) {
    warning(
      "Parameters `source` and `target` are deprecated.\n",
      "Use `input` and `output` instead."
    )
    input <- source
    output <- target
  }
  if (! is.null(algorithm)) {
    warning(
      "Parameter `algorithm` is deprecated.\n",
      "Use `method` instead."
    )
  }
  
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
  
  # check that input node(s) have in-degree zero
  if (method == "sgg" && any(degree(graph, input, "in") > 0)) {
    if (is.null(add.source)) {
      add.source <- TRUE
    } else if (! add.source) {
      stop(
        "Subgraph-growing algorithm 'sgg' requires",
        "`input` node to have out-degree zero."
      )
    }
  } else if (is.null(add.source)) {
    add.source <- FALSE
  }
  if (add.source) {
    graph <- add_vertices(graph, 1, attr = list(name = "rmfr_input_source"))
    input_names <- names(V(graph)[input])
    graph <- add_edges(graph, unlist(rbind("rmfr_input_source", input_names)))
    input <- "rmfr_input_source"
  }
  
  # calculate MFRs using the specified algorithm
  mfrs <- mfrs_fun(graph, input, output, silent)
  
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
      function(mfr) mfr[mfr <= ecount(graph) - length(input_names)]
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
    el <- as_edgelist(graph, names = FALSE)
    mfrs <- lapply(mfrs, function(x) {
      el[x, , drop = FALSE]
    })
  }
  
  mfrs
}

get_minimal_functional_routes <- get_mfrs

# depth-first search algorithm (Algorithm 1)
mfrs_dfs <- function(graph, input, output, silent) {
  mfrs_dfs_C(
    node_count = vcount(graph),
    link_array = as_edgelist(graph, names = FALSE),
    composite_nodes = which(vertex_attr(graph, "composite")),
    input_node = as.integer(V(graph)[input]),
    output_node = as.integer(V(graph)[output]),
    silent = silent
  )
}

# subgraph-growing algorithm (Algorithm 3)
mfrs_sgg <- function(graph, input, output, silent) {
  source_nodes <- as.integer(V(graph)[degree(graph, mode = "in") == 0])
  mfrs_sgg_C(
    node_count = vcount(graph),
    invadj_list = lapply(adjacent_vertices(graph, v = V(graph), mode = "in"),
                         function(x) as.numeric(x) - 1),
    node_composition = vertex_attr(graph, "composite"),
    input_node = as.integer(V(graph)[input]) - 1L,
    output_node = as.integer(V(graph)[output]) - 1L,
    source_nodes = source_nodes - 1L,
    silent = silent
  )
}

# rough implementation in R, to be later implemented in C++
mfrs_sggR <- function(graph, input, output, silent) {
  
  # setup
  input <- as.integer(V(graph)[input])
  output <- as.integer(V(graph)[output])
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
  mfrs <- list(list(net[[1]][output], net[[2]][output]))
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
  # require that the input node is among the MFR nodes
  #mfrs <- mfrs[which(!is.na(sapply(mfrs, function(mfr) {
  #  match(input, mfr[[1]])
  #})))]
  # require that all source nodes in each MFR are input nodes
  source_input <- sapply(mfrs, function(mfr) {
    all(intersect(source_nodes, mfr[[1]]) %in% input)
  })
  mfrs <- mfrs[which(source_input)]
  
  # result as a list of edge sequences
  lapply(mfrs, function(m) {
    sort(unlist(mapply(function(v0, v1s) {
      if (length(v1s) == 0) return(NULL)
      get.edge.ids(graph, rbind(v1s, v0))
    }, m[[1]], m[[2]], SIMPLIFY = FALSE)))
  })
}
