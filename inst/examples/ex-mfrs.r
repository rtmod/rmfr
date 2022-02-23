# MFRs for acyclic example (constructs expanded graph internally)
g <- example_acyclic
g_mfrs <- get_mfrs(g, expand = TRUE, source = "I", target = "O")
par(mfrow = c(2, 3))
for (i in seq_along(g_mfrs)) {
  g <- set_edge_attr(g, "MFR",
                     value = (as.numeric(E(g))) %in% g_mfrs[[i]])
  plot(g,
       layout = layout_as_tree,
       vertex.color = ifelse(V(g)$composite, "gray", "white"),
       edge.color = ifelse(edge_attr(g, "MFR"), "red", "black"),
       edge.width = ifelse(edge_attr(g, "MFR"), 3, 1))
}
par(mfrow = c(1, 1))
# MFRs for its expanded graph directly
h <- expand_graph(g)
h_mfrs <- get_mfrs(h, source = 1, target = 10)
par(mfrow = c(2, 3))
for (i in seq_along(h_mfrs)) {
  h <- set_edge_attr(h, "MFR",
                     value = (as.numeric(E(h))) %in% h_mfrs[[i]])
  plot(h,
       layout = layout_as_tree,
       vertex.color = ifelse(V(h)$composite, "gray", "white"),
       edge.color = ifelse(edge_attr(h, "MFR"), "red", "black"),
       edge.width = ifelse(edge_attr(h, "MFR"), 3, 1))
}
par(mfrow = c(1, 1))
# MFRs from expanded graph converted back to contracted graph
g_ <- contract_graph(h)
par(mfrow = c(2, 3))
for (i in seq_along(h_mfrs)) {
  g_ <- set_edge_attr(g_, "MFR",
                      value = (as.numeric(E(g_))) %in% h_mfrs[[i]])
  plot(g_,
       layout = layout_as_tree,
       vertex.color = ifelse(V(g_)$composite, "gray", "white"),
       edge.color = ifelse(edge_attr(g_, "MFR"), "red", "black"),
       edge.width = ifelse(edge_attr(g_, "MFR"), 3, 1))
}
par(mfrow = c(1, 1))

# color duplicated link
g <- example_acyclic
h <- expand_graph(g)
mfrs <- get_mfrs(h, source = 1, target = 10, format = "sequences")
h <- set_edge_attr(h, "MFR", value = sapply(
  E(h),
  function(i) length(which(mfrs[[3]] == i))
))
plot(
  h, layout = layout_as_tree,
  vertex.color = ifelse(V(h)$composite, "gray", "white"),
  edge.color = c("black", "red", "blue")[unlist(edge_attr(h, "MFR")) + 1],
  edge.width = ifelse(edge_attr(h, "MFR"), 3, 1)
)

# subgraph-growing algorithm: toy examples
graph <- graph(c( 1,3, 2,3 ))
graph <- set_vertex_attr(graph, "composite", value = FALSE)
get_mfrs(graph, source = 1, target = 3, method = "sgg", silent = FALSE)
get_mfrs(graph, source = 1, target = 3, method = "sggR", silent = FALSE)

graph <- graph(c( 1,2, 2,3, 1,3, 3,4 ))
plot(graph, edge.label = E(graph))
graph <- set_vertex_attr(graph, "composite", value = FALSE)
get_mfrs(graph, source = 1, target = 4, method = "sgg", silent = TRUE)
get_mfrs(graph, source = 1, target = 4, method = "sggR", silent = TRUE)

graph <- graph(c( 1,2, 1,3, 2,4, 3,4, 4,5, 3,6, 6,5 ))
plot(graph, edge.label = E(graph))
graph <- set_vertex_attr(graph, "composite", value = as.logical(c(0,0,0,1,0,0)))
get_mfrs(graph, source = 1, target = 5, method = "sgg", format = "matrices")
get_mfrs(graph, source = 1, target = 5, method = "sggR", format = "matrices")

# subgraph-growing algorithm: acyclic example
acyclic_expansion <- expand_graph(example_acyclic)
plot(acyclic_expansion,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(acyclic_expansion, source = "I", target = "O", method = "dfs")
get_mfrs(acyclic_expansion, source = "I", target = "O", method = "sgg")
get_mfrs(acyclic_expansion, source = "I", target = "O", method = "sggR")

# subgraph-growing algorithm: cyclic example
cyclic_expansion <- expand_graph(example_cyclic)
plot(cyclic_expansion,
     layout = layout_with_fr,
     vertex.color = ifelse(V(cyclic_expansion)$composite, "gray", "white"),
     edge.label = E(cyclic_expansion))
get_mfrs(cyclic_expansion, source = "s", target = "t", method = "sgg")
get_mfrs(cyclic_expansion, source = "s", target = "t", method = "sggR")

\dontrun{
rbenchmark::benchmark(
  get_mfrs(cyclic_expansion, source = "s", target = "t", method = "sgg"),
  get_mfrs(cyclic_expansion, source = "s", target = "t", method = "sggR")
)
rbenchmark::benchmark(
  get_mfrs(acyclic_expansion, source = "I", target = "O", method = "dfs"),
  get_mfrs(acyclic_expansion, source = "I", target = "O", method = "sgg"),
  get_mfrs(acyclic_expansion, source = "I", target = "O", method = "sggR")
)
}
