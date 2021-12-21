## Test environments

* win-builder (devel)

* R-hub
    * solaris-x86-patched
    * ubuntu-gcc-devel

* CI services
    * Debian GNU/Linux 11 (bullseye), R 4.1.2 (CircleCI)
    * Debian GNU/Linux 11 (bullseye) (no pandoc), R 4.1.2 (CircleCI)
    * macOS High Sierra 10.13.6, R 4.1.2 (Travis-CI)
    * Windows Server 2012 R2 x64 (build 9600), R 4.1.2 (AppVeyor)

## R CMD check results

0 errors | 0 warnings | 1 note

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'John Blischak <jdblischak@gmail.com>'

New maintainer:
  John Blischak <jdblischak@gmail.com>
Old maintainer(s):
  John Blischak <jdblischak@uchicago.edu>
```

Explanation: I updated my email address since I am no longer affiliated with the
University of Chicago

## Previous CRAN checks

```
Version: 1.6.2
Check: LazyData
Result: NOTE
     'LazyData' is specified without a 'data' directory
```

I removed the field `LazyData`

```
== Failed tests ================================================================
-- Failure (test-wflow_run.R:58:3): wflow_run argument verbose controls code echoing --
`wflow_run(rmd, verbose = FALSE, project = path)` produced warnings.
```

This test failure was caused by the recent release of knitr 1.37. The maintainer
of knitr, Yihui Xie, kindly sent me a patch to fix the failing test.
