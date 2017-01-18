# Resources:
# .onAttach & zzz.R explained: http://r-pkgs.had.co.nz/r.html#r-differences

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sprintf("This is workflowr version %s",
                                utils::packageVersion("workflowr")))
  packageStartupMessage("Please send bug reports and feature requests to:")
  packageStartupMessage("https://github.com/jdblischak/workflowr/issues")
}
