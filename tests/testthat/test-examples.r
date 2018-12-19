context("cyclic and acyclic example graphs")

sort_mfrs <- function(mfrs) {
  mfrs <- lapply(mfrs, sort)
  mfrs[order(sapply(mfrs, paste, collapse = ""))]
}

test_that("algorithms agree on acyclic example", {
  expect_equal(
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", algorithm = "dfs")),
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", algorithm = "sgg"))
  )
  expect_equal(
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", algorithm = "sgg")),
    sort_mfrs(get_mfrs(example_acyclic,
                       input = "I", output = "O", algorithm = "sggR"))
  )
})

test_that("default algorithm respects cyclicity", {
  expect_error(
    get_mfrs(example_cyclic, input = "s", output = "t"),
    NA
  )
})
