## Test environments

* Ubuntu 18.04, R 3.5.3 (local)
* Ubuntu 14.04, R 3.4.4, 3.5.3, devel (Travis-CI)
* Ubuntu 14.04, R 3.5.3, git2r 0.21.0 (Travis-CI)
* Ubuntu 16.04, R 3.5.3 (r-hub)
* Ubuntu 18.04 (no pandoc), R 3.5.3 (CircleCI)
* macOS 10.10.5, R 3.3.3 (local)
* macOS 10.13.3, R 3.4.4, 3.5.3 (Travis-CI)
* macOS 10.11, R 3.5.2 (r-hub)
* Windows 10, R 3.5.3 (local)
* Windows Server 2012 R2 (x64/x64, x86/i386), R 3.5.3 (AppVeyor)
* Windows Server 2008 R2 SP1, R 3.5.3, 32/64 bit (r-hub)
* winbuilder (release, devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## CRAN Package Check Results for previous version 1.2.0

Version 1.2.0 failed the check on CRAN servers r-patched-solaris-x86,
r-release-osx-x86_64, and r-oldrel-osx-x86_64 because one test required pandoc,
and these do not have pandoc installed. I have skipped this test and also setup
a CI build to catch these errors earlier in the future.

Version 1.2.0 failed the check on CRAN servers r-release-osx-x86_64 and
r-oldrel-osx-x86_64 due to a filepath issue. The tests passed on my local macOS,
Travis, and r-hub, so I don't know the cause of the failure. I have skipped
these tests.
