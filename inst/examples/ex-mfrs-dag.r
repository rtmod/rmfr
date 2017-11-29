# acyclic example
g <- example_acyclic
h <- expand_graph(g)
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(h)$composite, "gray", "white"),
     vertex.label = as.numeric(V(h)))
\dontrun{
g_mfrs <- mfrs_dag(
  node_count = vcount(h),
  link_array = as_edgelist(h, names = FALSE),
  composite_nodes = which(vertex_attr(h, "composite")),
  source_node = 1,
  target_node = 10
)
}
