## Resubmission

This is a resubmission. In this version I have:

* Fixed the NOTE about the missing object

* Fixed the failing unit test and confirmed there are no errors by submitting to
win-builder

* Fixed the vignette warnings from the previous CRAN release (1.0.1)

## Background

The upcoming [git2r release][git2r] is backwards incompatible. This minor
release of workflowr is compatible both with previous versions of git2r (<=
0.21.0) and the upcoming release (0.22.0).

[git2r]: https://github.com/ropensci/git2r/releases/tag/v0.22.0-RC2

## Test environments

* Ubuntu 18.04, R 3.4.4 (local)
* Ubuntu 14.04, R 3.4.4, 3.5.0, devel (Travis-CI)
* Ubuntu 16.04, R 3.4.4 (r-hub)
* macOS 10.10.5, R 3.3.3 (local)
* macOS 10.12.6, R 3.4.4, 3.5.0 (Travis-CI)
* macOS 10.11, R 3.5.0 (r-hub)
* Windows 10, R 3.5.0 (local)
* Windows Server 2012 R2, R 3.5.0 (AppVeyor)
* Windows Server 2008 R2 SP1, R 3.5.0, 32/64 bit (r-hub)
* winbuilder (release, devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## Test environments with git2r release candidate

* Ubuntu 18.04, R 3.4.4 (local)
* Ubuntu 14.04, R 3.5.0 (Travis-CI)
* macOS 10.10.5, R 3.3.3 (local)
* Windows 10, R 3.5.0 (local)

## R CMD check results with git2r release candidate

0 errors | 0 warnings | 0 note
