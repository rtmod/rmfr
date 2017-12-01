# acyclic example
g <- example_acyclic
h <- expand_graph(g)
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(h)$composite, "gray", "white"))
\dontrun{
h_mfrs <- rmfr:::mfrs_dfs(
  node_count = vcount(h),
  link_array = as_edgelist(h, names = FALSE),
  composite_nodes = which(vertex_attr(h, "composite")),
  source_node = 1,
  target_node = 10
)
}
h_mfrs <- get_mfrs(h, source = 1, target = 10)

par(mfrow = c(2, 3))
for (i in seq_along(h_mfrs)) {
  h <- set_edge_attr(h, "MFR",
                     value = (as.numeric(E(h)) - 1) %in% h_mfrs[[i]])
  plot(h,
       layout = layout_as_tree,
       vertex.color = ifelse(V(h)$composite, "gray", "white"),
       edge.color = ifelse(edge_attr(h, "MFR"), "red", "black"),
       edge.width = ifelse(edge_attr(h, "MFR"), 3, 1))
}
par(mfrow = c(1, 1))

g_ <- contract_graph(h)

par(mfrow = c(2, 3))
for (i in seq_along(h_mfrs)) {
  g_ <- set_edge_attr(g_, "MFR",
                      value = (as.numeric(E(g_)) - 1) %in% h_mfrs[[i]])
  plot(g_,
       layout = layout_as_tree,
       vertex.color = ifelse(V(g_)$composite, "gray", "white"),
       edge.color = ifelse(edge_attr(g_, "MFR"), "red", "black"),
       edge.width = ifelse(edge_attr(g_, "MFR"), 3, 1))
}
par(mfrow = c(1, 1))
