# acyclic example
g <- example_acyclic
plot(g,
     layout = layout_as_tree,
     edge.color = ifelse(is.na(E(g)$synergy), "black", E(g)$synergy),
     edge.width = ifelse(is.na(E(g)$synergy), 1, 3))
h <- expand_graph(g)
plot(h,
     layout = layout_as_tree,
     vertex.color = ifelse(V(h)$composite, "gray", "white"))
# cyclic example
g <- example_cyclic
plot(g,
     layout = layout_nicely,
     edge.color = ifelse(is.na(E(g)$synergy), "black", E(g)$synergy),
     edge.width = ifelse(is.na(E(g)$synergy), 1, 3))
h <- expand_graph(g)
plot(h,
     layout = layout_nicely,
     vertex.color = ifelse(V(h)$composite, "gray", "white"))
