context("callr")

# Regression test: next time quickly identify behavior like callr 3.3.0
# https://github.com/r-lib/callr/commit/df85ab14fbbf8ca9b89f55e50089828e0ac86e9a
test_that("r_safe() does not write to global environment", {

  ls_global <- callr::r_safe(function() ls(.GlobalEnv))
  expect_true(length(ls_global) == 0)
})
