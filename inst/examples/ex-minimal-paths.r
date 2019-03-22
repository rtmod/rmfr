# use MFR functionality to enumerate minimal paths

# acyclic example
g <- example_acyclic
plot(g, edge.label = 1:ecount(g))
get_minimal_functional_routes(g, input = "I", output = "O")
get_minimal_paths(g, input = "I", output = "O")

# cyclic example
g <- example_cyclic
plot(g, edge.label = 1:ecount(g))
get_minimal_functional_routes(g, input = "s", output = "t")
get_minimal_paths(g, input = "s", output = "t")
