context("depth-first search algorithm")

# one target with two factors
g <- graph(c(1,3,2,3), directed = TRUE) %>%
  set_vertex_attr("name", value = c("A", "B", "O")) %>%
  set_edge_attr("synergy", value = c(NA_integer_, NA_integer_))
h <- expand_graph(g)
# factors become synergistic
g_ <- set_edge_attr(g, "synergy", value = c(1L, 1L))
h_ <- expand_graph(g_)

test_that("synergy requires both sources", {
  # without synergy
  expect_equal(
    get_mfrs(g, source = "A", target = "O", method = "dfs"),
    list(1)
  )
  expect_equal(
    get_mfrs(h, source = "A", target = "O", method = "dfs"),
    list(1)
  )
  # with synergy
  expect_equal(
    get_mfrs(g_, source = "A", target = "O", method = "dfs"),
    list()
  )
  expect_equal(
    get_mfrs(h_, source = "A", target = "O", method = "dfs"),
    list()
  )
})

# one factor gets its own factor
p <- add_edges(add_vertices(g, 1), c(4, 1))
V(p)$name[4] <- "P"
q <- expand_graph(p)
p_ <- set_edge_attr(p, "synergy", value = c(1L, 1L, NA_integer_))
q_ <- expand_graph(p_)

test_that("sources with positive in-degree incur error if appropriate", {
  expect_error(
    get_mfrs(p, source = "A", target = "O", method = "dfs"),
    NA
  )
  expect_error(
    get_mfrs(q, source = "A", target = "O", method = "dfs"),
    NA
  )
  expect_error(
    get_mfrs(p_, source = "A", target = "O", method = "dfs"),
    NA
  )
  expect_error(
    get_mfrs(q_, source = "A", target = "O", method = "dfs"),
    NA
  )
  expect_error(
    get_mfrs(p, source = "A", target = "O", method = "dfs", add.source = FALSE),
    NA
  )
  expect_error(
    get_mfrs(q, source = "A", target = "O", method = "dfs", add.source = FALSE),
    NA
  )
  expect_error(
    get_mfrs(p_, source = "A", target = "O", method = "dfs", add.source = FALSE),
    NA
  )
  expect_error(
    get_mfrs(q_, source = "A", target = "O", method = "dfs", add.source = FALSE),
    NA
  )
})
