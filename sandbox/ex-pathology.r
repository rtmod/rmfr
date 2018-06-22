devtools::load_all()

par(mfrow = c(1, 2))
# construct example graph with two inputs
g <- graph(c(1,3,2,3), directed = TRUE) %>%
  set_vertex_attr("name", value = c("A", "B", "O")) %>%
  set_edge_attr("synergy", value = c(NA, NA))
h <- expand_graph(g)
# plot graphs
plot(g,# layout = layout_as_tree,
     vertex.color = ifelse(V(g)$composite, "gray", "white"),
     edge.label = E(g))
plot(h,# layout = layout_as_tree,
     vertex.color = ifelse(V(h)$composite, "gray", "white"),
     edge.label = E(h))
# calculate minimal functional routes using the depth-first search algorithm
get_mfrs(g, expand = TRUE, input = "A", output = "O", algorithm = "dfs")
get_mfrs(h, input = "A", output = "O", algorithm = "dfs")
# calculate minimal functional routes using the subgraph-growing algorithm
get_mfrs(g, expand = TRUE, input = "A", output = "O", algorithm = "sgg")
get_mfrs(h, input = "A", output = "O", algorithm = "sgg")
# add synergy between the only two links
g_ <- set_edge_attr(g, "synergy", value = c(1, 1))
h_ <- expand_graph(g_)
# plot graphs
plot(g_,# layout = layout_as_tree,
     vertex.color = ifelse(V(g_)$composite, "gray", "white"),
     edge.label = E(g_))
plot(h_,# layout = layout_as_tree,
     vertex.color = ifelse(V(h_)$composite, "gray", "white"),
     edge.label = E(h_))
# calculate minimal functional routes using the depth-first search algorithm
get_mfrs(g_, expand = TRUE, input = "A", output = "O", algorithm = "dfs")
get_mfrs(h_, input = "A", output = "O", algorithm = "dfs")
# calculate minimal functional routes using the subgraph-growing algorithm
get_mfrs(g_, expand = TRUE, input = "A", output = "O", algorithm = "sgg")
get_mfrs(h_, input = "A", output = "O", algorithm = "sgg")


g <- graph(c(1,3,2,3), directed = TRUE) %>%
  set_vertex_attr("name", value = c("A", "B", "O")) %>%
  set_edge_attr("synergy", value = c(NA, NA))
g_ <- set_edge_attr(g, "synergy", value = c(1, 1))
h_ <- expand_graph(g_)
devtools::load_all()
get_mfrs(h_, input = "A", output = "O", algorithm = "sgg", silent = FALSE)
