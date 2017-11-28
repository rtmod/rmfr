# acyclic example
g <- example_acyclic
h <- expand_graph(g)
\dontrun{
g_mfrs <- mfrs_dag(
  node_count = vcount(h),
  link_array = as_edgelist(h, names = FALSE),
  composite_nodes = which(vertex_attr(h, "composite")),
  source_node = which(vertex_attr(h, "name") == "s"),
  target_node = which(vertex_attr(h, "name") == "t")
)
}
