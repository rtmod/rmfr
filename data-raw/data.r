library(devtools)

example_acyclic <- make_graph(
  c(
    'I','A', 'I','B', 'I','C',
    'C','B',
    'A','D', 'A','E', 'B','E', 'C','F',
    'D','G', 'E','G', 'F','H',
    'E','O', 'F','O',
    'G','O', 'H','O'
  ),
  directed = TRUE
)
E(example_acyclic)$synergy <- NA
E(example_acyclic)$synergy[c( 6,7, 12,13 )] <- c(1, 1, 2, 2)
#plot(example_acyclic, layout = layout_as_tree)
use_data(example_acyclic)

example_cyclic <- make_graph(
  c(
    's','v1', 's','v2', 's','v3',
    'v3','v4', 'v4','v2',
    'v1','v5', 'v2','v5', 'v5','v4',
    'v4','v6', 'v6','v5',
    'v5','t', 'v6','t'
  ),
  directed = TRUE
)
E(example_cyclic)$synergy <- NA
E(example_cyclic)$synergy[c( 6,7, 2,5 )] <- c(1, 1, 2, 2)
#plot(example_cyclic, layout = layout_as_tree)
use_data(example_cyclic)
