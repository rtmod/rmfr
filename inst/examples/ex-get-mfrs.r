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
mfrs <- get_mfrs(h, source = 1, target = 10)
h <- set_edge_attr(h, "MFR", value = sapply(
  as.numeric(E(h)),
  function(i) length(which(mfrs[[3]] == i))
))
plot(
  h, layout = layout_as_tree,
  vertex.color = ifelse(V(h)$composite, "gray", "white"),
  edge.color = c("black", "red", "blue")[unlist(edge_attr(h, "MFR")) + 1],
  edge.width = ifelse(edge_attr(h, "MFR"), 3, 1)
)
