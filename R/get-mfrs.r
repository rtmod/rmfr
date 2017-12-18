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
#' @param expand Whether to expand \code{graph} before finding the MFRs and, if 
#'   so, to return the list of MFRs in terms of the node and link IDs of
#'   \code{graph} rather than of its expanded graph.
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
  graph, expand = FALSE,
  source, target,
  algorithm = NULL,
  silent = TRUE,
  output = "sequences"
) {
  output <- match.arg(output, c("sequences", "matrices"))
  
  if (expand) {
    graph_orig <- graph
    graph <- expand_graph(graph)
  }
  
  graph_is_dag <- is_dag(graph)
  if (is.null(algorithm)) {
    algorithm <- if (graph_is_dag) "dfs" else "sgg"
  }
  algorithm <- match.arg(algorithm, c("dfs", "ilp", "sgg"))
  if (algorithm == "dfs" & !graph_is_dag) {
    warning("Depth-first search algorithm 'dfs' is proved only for DAGs.")
  }
  mfrs_fun <- get(paste0("mfrs_", algorithm))
  
  mfrs <- mfrs_fun(graph, source, target, silent)
  # THIS SHOULD BE DONE IN THE C++ SOURCE
  stopifnot(mfrs$mfr_count == length(mfrs$mfr_set))
  mfrs <- mfrs$mfr_set
  mfrs <- mfrs %>%
    lapply(function(x) x + 1)
  
  if (expand) {
    mfrs <- mfrs %>%
      lapply(match, table = graph_attr(graph, "link_permutation")) %>%
      lapply(function(x) x[!is.na(x)])
    graph <- graph_orig
  }
  
  if (output == "matrices") {
    el <- as_edgelist(graph, names = FALSE)
    mfrs <- lapply(mfrs, function(x) {
      el[x, , drop = FALSE]
    })
  }
  
  mfrs
}

get_minimal_functional_routes <- get_mfrs

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

# rough implementation in R, to be later implemented in C++
mfrs_sgg <- function(graph, source, target, silent) {
  
  # setup
  source <- as.numeric(V(graph)[source])
  target <- as.numeric(V(graph)[target])
  composite_nodes = which(vertex_attr(graph, "composite"))
  graph <- delete_vertex_attr(graph, "name")
  net <- list(
    as.numeric(V(graph)),
    unname(lapply(as_adj_list(graph, "in"), as.numeric))
  )
  
  # initialization
  pointer <- 1
  n_mfrs <- 1
  flag <- FALSE
  mfrs <- list(list(net[[1]][target], net[[2]][target]))
  tag <- c(1)
  
  # while some partial MFRs remain
  while (pointer <= n_mfrs) {
    
    flag <- FALSE
    cmfr <- mfrs[[pointer]]
    ctag <- tag[pointer]
    
    # while the current MFR is incomplete
    while (flag == FALSE) {
      
      cnode <- cmfr[[1]][[ctag]]
      cpred <- cmfr[[2]][[ctag]]
      
      # if no predecessors remain
      if (length(cpred) == 0) {
        
        # if the current node is the last node of the current partial MFR
        if (ctag == length(cmfr[[1]])) {
          flag <- TRUE
        } else {
          ctag <- ctag + 1
        }
        
      } else {
        
        # if the current node is an original node
        if (!(cnode %in% composite_nodes)) {
          
          m <- length(cpred)
          cmfr[[2]][[ctag]] <- cpred[1]
          
          i <- 1
          while (i < m) {
            temp1 <- cmfr
            temp1[[2]][[ctag]] <- cpred[i + 1]
            mfrs[[n_mfrs + i]] <- temp1
            tag[n_mfrs + i] <- ctag
            i <- i + 1
          }
          n_mfrs <- n_mfrs + m - 1
          
        }
        
        cpred <- cmfr[[2]][[ctag]]
        
        if (all(cpred %in% cmfr[[1]])) {
          
          if (ctag == length(cmfr[[1]])) {
            flag <- TRUE
          } else {
            ctag <- ctag + 1
          }
          
        } else {
          
          for (v in cpred) {
            if (v %in% cmfr[[1]]) next
            i <- which(net[[1]] == v)
            temp2 <- list(net[[1]][i], net[[2]][i])
            cmfr[[1]] <- c(cmfr[[1]], temp2[[1]])
            cmfr[[2]] <- c(cmfr[[2]], temp2[[2]])
            ctag <- ctag + 1
          }
          
        }
        
      }
      
    }
    
    pointer <- pointer + 1
    
  }
  
  # result
  mfrs
}

# adapted implementation in R
mfrs_sgg <- function(graph, source, target, silent) {
  
  # setup
  source <- as.numeric(V(graph)[source])
  target <- as.numeric(V(graph)[target])
  composite_nodes = which(vertex_attr(graph, "composite"))
  graph <- delete_vertex_attr(graph, "name")
  net <- list(
    as.numeric(V(graph)),
    unname(lapply(as_adj_list(graph, "in"), as.numeric))
  )
  
  # initialization
  pointer <- 1
  n_mfrs <- 1
  flag <- FALSE
  mfrs <- list(list(net[[1]][target], net[[2]][target]))
  tag <- c(1)
  
  # while some partial MFRs remain
  while (pointer <= n_mfrs) {
    
    flag <- FALSE
    cmfr <- mfrs[[pointer]]
    ctag <- tag[pointer]
    
    # while the current MFR is incomplete
    while (flag == FALSE) {
      
      cnode <- cmfr[[1]][[ctag]]
      cpred <- cmfr[[2]][[ctag]]
      
      # if no predecessors remain,
      # then either increment the tag or mark the current partial MFR complete
      if (length(cpred) == 0) {
        
        # if the current node is the last node of the current partial MFR
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
          #cmfr[[2]][[ctag]] <- cpred[1]
          
          i <- 0
          while (i < m) {
            temp1 <- cmfr
            temp1[[2]][[ctag]] <- cpred[i + 1]
            mfrs[[n_mfrs + i]] <- temp1
            tag[n_mfrs + i] <- ctag
            i <- i + 1
          }
          n_mfrs <- n_mfrs + m - 1
          
          print(mfrs)
        }
        
        # SHOULD ONLY BE NECESSARY IF THE CURRENT NODE IS ORIGINAL
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
          
        } else {
          
          for (v in cpred) {
            if (v %in% cmfr[[1]]) next
            i <- which(net[[1]] == v)
            temp2 <- list(net[[1]][i], net[[2]][i])
            cmfr[[1]] <- c(cmfr[[1]], temp2[[1]])
            cmfr[[2]] <- c(cmfr[[2]], temp2[[2]])
          }
          ctag <- ctag + 1
          
        }
        
      }
      
    }
    
    pointer <- pointer + 1
    
  }
  
  # result
  mfrs
}

# MINIMAL TOY EXAMPLE
graph <- graph(c( 1,2, 2,3, 1,3, 3,4 ))
plot(graph)
graph <- set_vertex_attr(graph, "composite", value = c(F,F,F,F))
source <- 1
target <- 4

graph <- graph(c( 1,2, 1,3, 2,4, 3,4, 4,5, 3,6, 6,5 ))
plot(graph)
graph <- set_vertex_attr(graph, "composite", value = c(F,F,F,T,F,F))
source <- 1
target <- 5
