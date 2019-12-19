## Test environments

* Ubuntu 18.04, R 3.6.2 (local)
* Debian GNU/Linux bullseye/sid, R 3.6.2 (CircleCI)
* Debian GNU/Linux bullseye/sid (no pandoc), R 3.6.2 (CircleCI)
* Ubuntu 16.04, R 3.6.1 (r-hub)

* macOS 10.10.5, R 3.3.3 (local)
* macOS 10.13.6, R 3.6.2 (Travis-CI)
* macOS 10.11.6, R 3.6.2 (r-hub)

* Windows 10, R 3.6.1 (local)
* Windows Server 2012 R2 (x64/x64), R 3.6.2 (AppVeyor)
* Windows Server 2008 R2 SP1 (32/64 bit), R 3.6.2 (r-hub)
* winbuilder (release, devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Previous CRAN checks

Workflowr version 1.5.0 had one unit test that would sporadically fail the CRAN
checks (see output below). This is because it was a timezone-based error, and
thus would only fail if the checks were run at a certain time of day in the
given timezone.

I have fixed the timezone error in version 1.6.0. Furthermore, I contributed a
patch to the dependency git2r to prevent myself and other from making similar
timezone errors in the future.

https://github.com/ropensci/git2r/pull/408

 Running ‘testthat.R’ [42s/94s]
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
     > library("testthat")
     > library("workflowr")
     This is workflowr version 1.5.0
     Run ?workflowr for help getting started
     >
     > test_check("workflowr")
     ── 1. Failure: Conversion of git_time to character string of date is correct (@t
     as.character(Sys.Date()) not identical to as.character(as.Date(as.POSIXct(c1$author$when))).
     1/1 mismatches
     x[1]: "2019-12-19"
     y[1]: "2019-12-18"
    
     ══ testthat results ═══════════════════════════════════════════════════════════
     [ OK: 1100 | SKIPPED: 152 | WARNINGS: 0 | FAILED: 1 ]
     1. Failure: Conversion of git_time to character string of date is correct (@test-report.R#24)
    
     Error: testthat unit tests failed
     Execution halted 
