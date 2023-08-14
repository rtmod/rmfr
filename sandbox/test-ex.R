library(igraph)

g <- graph(c(0, 2, 1, 2, 2, 4, 3, 4) + 1, directed = TRUE)
g <- set_vertex_attr(g, "name", value = LETTERS[seq(5L)])
g <- set_edge_attr(g, "synergy", value = c(1L, 1L, 2L, 2L))

devtools::load_all()

h <- expand_graph(g)
plot(h)

g_mfrs <-
  get_mfrs(g, source = "A", target = "E", method = "sgg", silent = FALSE)
