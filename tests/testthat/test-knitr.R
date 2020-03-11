context("knitr")

# Test include_graphics() ------------------------------------------------------

test_that("include_graphics() can create HTML tag for file it can't find", {
  # This is critical because the Rmd files are in analysis/, code chunks
  # executed in the project root (by default), and HTML files saved in docs/. In
  # order to reference image files relative to docs/, need to be able to create
  # the links even if knitr can't currently identify the files. The default
  # behavior changed in knitr 1.28.
  # https://github.com/yihui/knitr/issues/1717#issuecomment-583373829
  # https://jdblischak.github.io/workflowr/articles/wflow-05-faq.html#how-can-i-include-external-images-in-my-website

  version_knitr <- utils::packageVersion("knitr")

  if(version_knitr >= "1.28") {
    expect_silent(
      result <- knitr::include_graphics("docs/assets/external.png", error = FALSE)
    )
  } else {
    expect_silent(
      result <- knitr::include_graphics("docs/assets/external.png")
    )
  }
  expect_identical(as.character(result), "docs/assets/external.png")
})
