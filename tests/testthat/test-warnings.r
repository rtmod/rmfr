context("warnings")

test_that("graph with 'synergy' link attribute is expanded if not instructed", {
  expect_warning(
    get_mfrs(example_acyclic, input = "I", output = "O"),
    "synergy.*expand"
  )
  expect_warning(
    get_mfrs(example_cyclic, input = "s", output = "t"),
    "synergy.*expand"
  )
})
