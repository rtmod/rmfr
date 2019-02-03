context("warnings")

example_acyclic_err <- set_vertex_attr(
  example_acyclic,
  "composite", value = FALSE
)
example_cyclic_err <- set_vertex_attr(
  example_cyclic,
  "composite", value = FALSE
)

test_that(
  "graph w/ 'synergy' link attr. is expanded w/ warning w/ 'composite' node attr.", {
    expect_warning(
      get_mfrs(example_acyclic, input = "I", output = "O"),
      NA
    )
    expect_warning(
      get_mfrs(example_cyclic, input = "s", output = "t"),
      NA
    )
    expect_warning(
      get_mfrs(example_acyclic_err, input = "I", output = "O"),
      "synergy.*expand"
    )
    expect_warning(
      get_mfrs(example_cyclic_err, input = "s", output = "t"),
      "synergy.*expand"
    )
  }
)
