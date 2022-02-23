# use MFR functionality to enumerate minimal paths

# acyclic example
g <- example_acyclic
plot(g, edge.label = 1:ecount(g))
get_minimal_functional_routes(g, source = "I", target = "O")
get_minimal_paths(g, source = "I", target = "O")

# cyclic example
g <- example_cyclic
plot(g, edge.label = 1:ecount(g))
get_minimal_functional_routes(g, source = "s", target = "t")
get_minimal_paths(g, source = "s", target = "t")
