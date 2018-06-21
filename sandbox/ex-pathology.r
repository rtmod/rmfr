# construct example graph with two inputs
g <- graph(c(1,3,2,3), directed = TRUE) %>%
  set_vertex_attr("name", value = c("A", "B", "O")) %>%
  set_edge_attr("synergy", value = c(NA, NA))
h <- expand_graph(g)
# calculate minimal functional routes
get_mfrs(g, expand = TRUE, source = "A", target = "O")
get_mfrs(h, source = "A", target = "O")
# add synergy between the only two links
g_ <- set_edge_attr(g, "synergy", value = c(1, 1))
h_ <- expand_graph(g_)
# calculate minimal functional routes
get_mfrs(g_, expand = TRUE, source = "A", target = "O")
get_mfrs(h_, source = "A", target = "O")
