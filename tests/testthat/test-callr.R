context("callr")

# Regression test: next time quickly identify behavior like callr 3.3.0
# https://github.com/r-lib/callr/commit/df85ab14fbbf8ca9b89f55e50089828e0ac86e9a
test_that("r_safe() does not write to global environment", {

  if (utils::packageVersion("callr") == "3.3.0") skip("Known callr 3.3.0 bug")

  ls_global <- callr::r_safe(function() ls(.GlobalEnv))
  expect_true(length(ls_global) == 0)
})

test_that("check_environment warns about callr 3.3.0", {

  if (utils::packageVersion("callr") != "3.3.0") skip("Only for callr 3.3.0")

  result_environment <- callr::r_safe(function() workflowr:::check_environment())
  expect_false(result_environment$pass)
  expect_true(any(stringr::str_detect(result_environment$details, "callr")))
  expect_true(any(stringr::str_detect(result_environment$details, "3\\.3\\.0")))
})
