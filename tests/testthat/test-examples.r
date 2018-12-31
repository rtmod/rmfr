context("cyclic and acyclic example graphs")

sort_mfrs <- function(mfrs) {
  mfrs <- lapply(mfrs, sort)
  mfrs[order(sapply(mfrs, paste, collapse = ""))]
}

test_that("algorithms agree on acyclic example", {
  expect_equal(
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", method = "dfs")),
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", method = "sgg"))
  )
  expect_equal(
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", method = "sgg")),
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", method = "sggR"))
  )
})

test_that("default algorithm respects cyclicity", {
  expect_error(
    get_mfrs(example_cyclic, input = "s", output = "t"),
    NA
  )
})
