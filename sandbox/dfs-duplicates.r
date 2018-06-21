# check behavior with acyclic example
get_mfrs(example_acyclic, expand = TRUE,
         source = "I", target = "O", algorithm = "dfs")
acyclic_expansion <- expand_graph(example_acyclic)
plot(acyclic_expansion,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(acyclic_expansion, source = "I", target = "O", algorithm = "dfs")

# preserve behavior with smaller graph
h <- acyclic_expansion
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
h <- h %>%
  delete_vertices(c("A", "D", "G", "c1")) %>%
  add_edges(c("B", "E"))
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
h <- h %>%
  delete_vertices(c("H", "E")) %>%
  add_edges(c("B", "c2"))
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
h <- h %>%
  delete_vertices(c("F")) %>%
  add_edges(c("C", "c2"))
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
h <- h %>%
  delete_edges(1)
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
h <- h %>%
  delete_vertices(c("C")) %>%
  add_edges(c("I", "B", "I", "c2"))
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")

h <- acyclic_expansion %>%
  delete_vertices(c("A", "D", "G", "c1", "H", "E", "F")) %>%
  delete_edges(1) %>%
  add_edges(c("B", "c2", "C", "c2"))
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(acyclic_expansion)$composite, "gray", "white"),
     edge.label = E(acyclic_expansion))
get_mfrs(h, source = "I", target = "O", algorithm = "dfs")
get_mfrs(h, source = "I", target = "O", algorithm = "dfs", silent = FALSE)
