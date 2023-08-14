library(tidyverse)
library(igraph)
devtools::load_all()
library(tidygraph)
library(ggraph)

# small example with loop
cbind(
  from = c(1, 1, 2, 2, 3),
  to   = c(2, 3, 2, 4, 4)
) |> 
  graph_from_edgelist() |> 
  as_tbl_graph() |> 
  activate(links) |> 
  mutate(synergy = c(NA, NA, NA, 1L, 1L), sign = 1L) |> 
  print() -> loopy
as_tibble(loopy)
all_shortest_paths(loopy, 1L, 4L)
get_mfrs(loopy, 1L, 4L, method = "sgg")
get_mfrs(loopy, 1L, 4L, method = "sgg", silent = FALSE)

# dendritic cell differentiation model
here::here("../mfrpy/sandbox/dendrite.bnet") |> 
  read_lines() |> 
  enframe(name = NULL, value = "line") |> 
  filter(! str_detect(line, "^#") & str_detect(line, ",[:space:]+")) |> 
  separate(line, into = c("target", "source"), sep = ",") |> 
  slice_tail(n = -1L) |> 
  mutate(across(c(target, source), ~ str_trim(.))) |> 
  mutate(source = str_split(source, pattern = " +\\| +")) |> 
  unnest(source) |> 
  # mutate(synergy = as.integer(fct_inorder(source))) |> 
  mutate(synergy = row_number()) |> 
  mutate(source = str_split(source, pattern = "\\&")) |> 
  unnest(source) |> 
  mutate(sign = ifelse(str_detect(source, "^!"), -1L, 1L)) |> 
  mutate(source = str_remove(source, "^!")) |> 
  print() -> dendrite_links
dendrite_links |> 
  select(source, target, everything()) |> 
  graph_from_data_frame(directed = TRUE) |> 
  print() -> dendrite_graph

# key nodes
V(dendrite_graph)[c(33L, 37L, 30L)]
# remove loops
dendrite_graph %>%
  # NOTE: multiple edges encode distinct synergies
  simplify(remove.multiple = FALSE, remove.loops = TRUE) ->
  dendrite_graph
# shortest paths: quicker, but partially because not all minimal paths captured
all_shortest_paths(dendrite_graph, 33L, 30L, mode = "out")
all_shortest_paths(dendrite_graph, 37L, 30L, mode = "out")
# minimal functional routes, using C++ (crashes)
mfrs_CSF2_NFKB2 <- get_mfrs(dendrite_graph, 33L, 30L, method = "sgg")
mfrs_IL4_NFKB2 <- get_mfrs(dendrite_graph, 37L, 30L, method = "sgg")
# minimal functional routes, in R (doom loop)
# mfrs_CSF2_NFKB2 <- get_mfrs(dendrite_graph, 33L, 30L, method = "sggR")
# mfrs_IL4_NFKB2 <- get_mfrs(dendrite_graph, 37L, 30L, method = "sggR")

default_arrow <- grid::arrow(
  angle = 20,
  length = unit(.01, "native"),
  ends = "last",
  type = "closed"
)
# plot graph
dendrite_graph %>%
  as_tbl_graph() %>%
  activate(links) %>%
  # make synergy undefined if it takes only one value
  group_by(synergy) %>%
  mutate(n_syn = n()) %>%
  ungroup() %>%
  mutate(synergy = ifelse(n_syn == 1L, NA_integer_, synergy)) %>%
  mutate(synergy = as.factor(xtfrm(synergy))) %>%
  mutate(synergy = as.factor(synergy)) %>%
  mutate(relate = ifelse(sign == 1L, "activate", "inhibit")) %>%
  ggraph(layout = "tree") +
  theme_graph() +
  geom_edge_link(
    aes(color = synergy, edge_linetype = relate),
    arrow = default_arrow,
    # start_cap = circle(.25, "cm"), end_cap = circle(.4, "cm")
    start_cap = circle(.12, "native"), end_cap = circle(.16, "native")
  ) +
  geom_node_label(aes(label = name), size = 2) +
  scale_edge_color_discrete(na.value = "black") +
  guides(edge_color = "none", edge_linetype = "none")
