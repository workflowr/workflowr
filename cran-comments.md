## Test environments

* Ubuntu 18.04, R 3.6.3 (local)
* Debian GNU/Linux bullseye/sid, R 3.6.3 (CircleCI)
* Debian GNU/Linux bullseye/sid (no pandoc), R 3.6.3 (CircleCI)

* macOS 10.10.5, R 3.3.3 (local)
* macOS 10.13.6, R 4.0.0 (Travis-CI)
* macOS 10.13.6, R 4.0.0 (r-hub)

* Windows 10, R 4.0.0 (local)
* Windows Server 2012 R2 (x64/x64), R 4.0.0 (AppVeyor)
* Windows Server 2008 R2 SP1 (32/64 bit), R 4.0.0 (r-hub)
* winbuilder (release, devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Previous CRAN checks

The check for workflowr version 1.6.1 failed on solaris because some tests were
checking for a filesystem issue that is only possible on Linux and macOS. These
tests are now skipped on solaris.

Version: 1.6.1
Check: tests
Result: ERROR
     Running ‘spelling.R’
     Running ‘system-info.R’
     Running ‘testthat.R’ [78s/94s]
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
     > library("testthat")
     > library("workflowr")
     This is workflowr version 1.6.1
     Run ?workflowr for help getting started
     >
     > test_check("workflowr")
     ── 1. Error: check_wd_exists throws error if working directory has been deleted
     [EINVAL] Failed to remove '/tmp/RtmpMBay9X/working_dir/RtmpheaWir/file224661335cfb': invalid argument
     Backtrace:
     1. fs::dir_delete(path)

     ── 2. Error: wflow_status fails early if deleted subdirectory is current working
     [EINVAL] Failed to remove '/tmp/RtmpMBay9X/working_dir/RtmpheaWir/file22461d523f12/sub': invalid argument
     Backtrace:
     1. fs::dir_delete(subdir)

     ── 3. Error: wflow_status fails early if deleted root directory is current worki
     [EINVAL] Failed to remove '/tmp/RtmpMBay9X/working_dir/RtmpheaWir/file224672d44faa': invalid argument
     Backtrace:
     1. fs::dir_delete(path)

     ── 4. Error: wflow_status fails early if deleted external directory is current w
     [EINVAL] Failed to remove '/tmp/RtmpMBay9X/working_dir/RtmpheaWir/file22466cff2a79': invalid argument
     Backtrace:
     1. fs::dir_delete(extdir)

     ══ testthat results ═══════════════════════════════════════════════════════════
     [ OK: 1207 | SKIPPED: 165 | WARNINGS: 0 | FAILED: 4 ]
     1. Error: check_wd_exists throws error if working directory has been deleted (@test-utility.R#711)
     2. Error: wflow_status fails early if deleted subdirectory is current working directory (@test-wflow_status.R#348)
     3. Error: wflow_status fails early if deleted root directory is current working directory (@test-wflow_status.R#366)
     4. Error: wflow_status fails early if deleted external directory is current working directory (@test-wflow_status.R#387)

     Error: testthat unit tests failed
     Execution halted
Flavor: r-patched-solaris-x86
