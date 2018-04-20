# workflowr 1.0.1

Various minor changes to documentation, tests, and package infrastructure to
prepare for CRAN submission.

# workflowr 1.0.0

The 1.0.0 release is a major change. Instead of relying on the external code chunks
in `chunks.R` to implement the reproducibility features, workflowr 1.0.0 replaces
`chunks.R` with the output format `wflow_html()` and site generator
`wflow_site()`. This enables a much more informative reproducibility report to
be created.

```
# Install from GitHub
devtools::install_github("jdblischak/workflowr")

# Start a new project to test out the new features
wflow_start("testproject")

# Learn about the new ways to customize your site
?wflow_html

# Update an existing project
wflow_update()
```

## Details

* Introduce `wflow_html()` and `wflow_site()` to overhaul the reproduciblity
features of workflowr
* Improve API consistency:
    * `wflow_commit()` -> `wflow_git_commit()`
    * `wflow_remotes()` -> `wflow_git_remote()`
* Remove some less commonly used infrastructure files
* Remove template infrastructure: `wflow_open()` and `wflow_convert()`
* Reimplement `wflow_update()` to udpate a pre-1.0 workflowr project to a
post-1.0 project
* Remove `create_links_page()` (not widely used, if at all)
* Note in documentation that setting the seed via `wflow_build()` or
`wflow_publish()` is ignored if using `wflow_html()`
* `wflow_publish(republish = TRUE)` does not build HTML files that have
unstaged/staged changes
* `wflow_build()` reports the total number of files that will be built
* Enable `wflow_start()` to set local Git user.name and user.email. Preferred
method for most users is still to set global Git user.name and user.email with
`wflow_git_config()`.

# workflowr 0.11.0

* `wflow_publish()` now views the website by default if run interactively (`view
= interactive()`) just like `wflow_build()`

* Pin the dependency git2r to version 0.21.0 or lower because of the
[forthcoming breaking changes in the git2r API][git2r-api-change]. This is not a
great solution because `install.packages()` always installs the latest version,
which will cause an error in the installation. If you are having trouble with
this, first install git2r with `devtools::install_version("git2r", "0.21.0")`
and then retry installing workflowr. If your machine is running macOS or
Windows, you may need to run `install_version("git2r", "0.21.0", type = "binary")`.

* Fix minor bug that affected the error message produced for a failed push or
pull using the SSH protocol

* When `wflow_git_push()` or `wflow_git_pull()` fails for an unknown reason, the
exact error message from `git2r::push()` or `git2r::pull()` is reported to
faciliate troubleshooting

* Multiple other internal changes to make workflowr more robust

[git2r-api-change]: https://github.com/ropensci/git2r/issues/312

# workflowr 0.10.2

Fix bug that caused `wflow_status()` (and thus `wflow_publish()`) to ignore R
Markdown files with the all lowercase file extension `.rmd` (reported by @LSun
and @pcarbo). This was recently introduced in commit
[a8ce711](https://github.com/jdblischak/workflowr/commit/a8ce711de4ea2939bf76f2c2403c1d631ec130b0#diff-08716e76a3c9aeb6ad641bb23e5e5ea6R126),
so it only affected versions 0.10.0 and 0.10.1.

# workflowr 0.10.1

Fix bug that prevented deleted files from being committed with `wflow_commit()`
or `wflow_publish()` (reported by @pcarbo)

# workflowr 0.10.0

## Integration with RStudio Viewer

* `wflow_view()` (or `wflow_build()`) now opens the website in the [RStudio
Viewer][rstudio-viewer] if run from within RStudio

## File globbing

* Main workflowr functions now accept [file globs][glob] as input (#84)

## Miscellaneous

* `wflow_build()` automatically removes unused figure files
* Improved documentation of `wflow_build()` arguments

[glob]: https://en.wikipedia.org/wiki/Glob_(programming)
[rstudio-viewer]: https://rstudio.github.io/rstudio-extensions/rstudio_viewer.html

# workflowr 0.9.0

## New RStudio project template

* New workflowr projects can now be created directly in RStudio using the
workflowr [project template][pt] (requires RStudio v1.1.28 or greater)

## Improved support for Windows

* Implemented continuous integration testing for Windows with
[AppVeyor][appveyor]
* Fixed unit test failures due to edge cases in Windows (#76)
* Skip evaluation of vignette code during installation if the machine does not
have the necessary configuration (#87, #88)

## Log files

* The log files by default are now written to the directory returned by
`tempdir()` instead of to `/tmp/workflowr`. This prevents failures due to
permission issues when mutliple workflowr users try to use the same machine
(e.g. a compute node on a HPC cluster) (#86)

## Miscellaneous

* Bug fix: When using `wflow_build()` to build the R Markdown files in an
external R process, it now runs `render_site()` in the global environment of the
external R process. This better mimics the results of the RStudio "Knit" button.

[appveyor]: https://ci.appveyor.com
[pt]: https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html

# workflowr 0.8.0

## Git installation is now optional

* It is no longer required to install Git prior to using workflowr. Git can be
installed at a later time if more advanced operations are needed. The Quick
Start and Getting Started vignette have been updated to reflect this.
* `wflow_git_config()` sets the Git options `user.name` and `user.email`
* `wflow_git_push()` pushes changes from the local computer to GitHub
* `wflow_git_pull()` pulls the changes from GitHub to the local computer

## Miscellaneous

* Added FAQ on sharing workflowr sites securely using Beaker Browswer
(@johnsonlab in #59 & #65)
* Added .Rprofile to automatically load workflowr (@vanAmsterdam in #73)
* Added FAQ about website not displaying (#70)
* Added tag to footer.html to adjust MathJax settings so that all math formulae
are shown using TeX fonts only. This will make the presentation more consistent
at the cost of the webpage sometimes taking slightly longer to load. (@pcarbo)
* Increase spacing between sections in HTML (@pcarbo in #57)
* Minor changes to improve error messages (#61)
* Minor updates to documentation

# workflowr 0.7.0

## wflow_build

* `wflow_build()` automatically opens the website after building files, thus
it's not necessary to always run `wflow_view()` manually afterwards. If one file
is built, then that file is opened. If more than one file is built, then the
index page is opened. (#41)
* `wflow_build()` adds objects to the global environment when building files
locally in the R console (i.e. `local = TRUE`) (#53)

## Miscellaneous

* Bug fix so workflowr can process Emacs backup files that contain a tilde (#47)
* Bug fix so print methods display properly in the R console (some did not end
with a newline character)

# workflowr 0.6.0

## New function wflow_remove

* `wflow_remove()` removes R Markdown files and all their associated files (e.g.
HTML and figures)
* If the files to be removed had been committed to the Git repository, the files
are also removed from the Git repository (analogous to `git rm`)
* `wflow_remove()` also works on non-Rmd files

## Improved support for Windows

* workflowr functions can now handle backslashes in filepaths
* `wflow_update()` and `wflow_convert()` fail gracefully if the `diff` utility is not available and inform the user to install [Rtools][]

## Miscellaneous

* Added continuous integration testing with [Travis CI][travis] for macOS
* Updated documentation in README, "Getting started" vignette, and FAQ

[Rtools]: https://cran.r-project.org/bin/windows/Rtools/

# workflowr 0.5.1

This minor release fixes a bug in how filepaths are resolved so that workflowr
can run on Windows.

# workflowr 0.5.0

This release changes the interface of some of the main workflowr functions. The 
functions `wflow_publish()` and `wflow_status()` are introduced, and the 
previous functions `wflow_build()` and `wflow_commit()` are re-designed.

## wflow_status

* New function `wflow_status()` reports which analysis files require user
attention (inspired by `git status`)

* Defines R Markdown files whose HTML has been committed as part of the Git repo
as "Published", R Markdown files which have been committed to the Git repo but 
not their HTML as "Unpublished", and R Markdown files that are untracked by Git 
as "Scratch". Furthermore, previously published files that have been 
subsequently edited are classified as "Modified". See the man page
`?wflow_status` for more details.

## wflow_publish

* This new function replaces the previous functionality of `wflow_commit()`. The
basic interface is much simpler.

* `wflow_publish("analysis/file.Rmd")` will 1) commit `analysis/file.Rmd`, 2) 
build `analysis/file.Rmd` in its own separate R session with `set.seed(12345)`, 
and 3) commit `docs/file.html` and any generated figures. These 3 steps are
referred to as "publishing a file".

* `wflow_publish(all = TRUE)` will publish all tracked analysis files, analogous
to `git commit -a`.

* To change the theme or make some other change to the entire site, run 
`wflow_publish("analysis/_site.yml", republish = TRUE)`, which will 1) commit 
the configuration file `analysis/_site.yml`, 2) re-build all the previously 
published analysis files using the new configuration options, and 3) commit the 
re-built HTML files.

## wflow_build

* By default, `wflow_build()` runs in "Make"-mode, only building R Markdown 
files that have been updated more recently than their corresponding HTML files. 
If instead files are specfically stated, those files will be built.

* By default, R Markdown files are now built each in their own separate R 
session (similar in function to the "Knit HTML" button in RStudio). This
prevents undesirable behavior like sharing variables and loaded packages across
separate files. Set `local = TRUE` to build the files in the local R console
(useful for debugging, but otherwise discouraged).

* By default, the seed for random number generation is set to the arbitrary 
number `12345` (using `set.seed()`). This ensures that any code that produces 
random numbers will be reproducible.

## wflow_commit

* `wflow_commit()` has been demoted to only being a wrapper for the equivalent 
functionality of `git add` and `git commit`. This can be useful for committing 
non-analysis files or R Markdown files that you aren't ready to publish yet. 
However, you should use `wflow_publish()` for the standard workflow.

* Set `all = TRUE` to run the equivalent of `git commit -a`.

## wflow_update

* Improved documentation of `wflow_update()` to better explain its 
functionality. It will attempt to convert all R Markdown files present to use 
the latest version of the workflowr R Markdown template; however, it will only 
commit R Markdown files that are tracked by Git.

## Miscellaneous

* All workflowr functions should now accept the file extesion `.rmd` in addition
to `.Rmd` (Issue #10)

* Replaced the shared argument `path` with `project` to clarify that this
argument specifies the path to a directory in the workflowr project

* `wflow_start()` now includes `docs/.nojekyll`

* Change dependency to R >= 3.2.5 (Issue #32)

* Change stringr dependency (>= 1.1.0)

* Started a vignette with Frequently Asked Questions

* Added sections to README (Quick start, Upgrading, Related work, and Citation)

# workflowr 0.4.0

## wflow_start

* The `wflow_start()` API has been simplified. Now the first (and only required) argument is the directory to be created. By default, the name of the project will be determined from the directory name. For example, `wflow_start("~/projects/proj-x")` will create a workflowr project with the name "proj-x". To specify a different name, provide the argument `name`
* New argument `change_wd = TRUE` changes the working directory to the newly created project. 
* New argument `existing = FALSE` is a safety feature so that by default workflowr projects are only created in new directories
* `wflow_start()` no longer adds "BuildType: Website" to RStudio project file.
Users should run `wflow_build()` instead. `wflow_update()` removes the build specification for existing workflowr projects
* Fixed bugs related to relative paths and nested paths

## workflowr R Markdown template

* Updated workflowr R Markdown template can be built even if the user does not have workflowr installed
* Use new function `wflow_convert()` to convert an R Markdown file based on the previous template to use the latest version
* New optional "standalone" version of the workflowr template for creating files outside of a workflowr project (set `standalone = TRUE` for `wflow_open()`)
* Can create a workflowr R Markdown file outside of a workflowr project by setting `path = NULL`

## Migrating an existing project to use workflowr

* New vignette on "Migrating an existing project to use workflowr"
* `wflow_update()` function updates workflowr projects to use the latest version of the template
* `wflow_convert()` converts an existing R Markdown file to use the workflowr template (can also update a file that uses a previous version of the template)

## Other new functions

* `wflow_view()` opens the website in the browser
* `wflow_remotes()` manages remote Git repositories on GitHub. It can add a remote, remove a remote, or update the URL for an existing remote

## Improvements to the workflowr R package itself

* Continuous integration testing with [Travis CI][travis]
* Code coverage with [covr][] and [Codecov][]
* Package documentation with [pkgdown][]

[pkgdown]: http://pkgdown.r-lib.org/
[travis]: https://travis-ci.org/
[covr]: https://github.com/jimhester/covr
[Codecov]: https://codecov.io/

# workflowr 0.3.0

* Second alpha release. Major API overhaul.

* Improved naming of functions: `start_project` -> `wflow_start`, `open_rmd` -> `wflow_open`, `build_site` -> `wflow_build`, `commit_site` -> `wflow_commit`, `create_results` -> `create_links_page`

* `wflow_commit` can optionally add and commit provided files (argument is `commit_files`) before re-building website and commiting HTML files

* `wflow_open` accepts multiple filenames

* Both `wflow_build` and `wflow_commit` have the argument `all` to optionally build all the HTML files

# workflowr 0.2.0

* First alpha release. Contains enough functions to create workflowr project from scratch.

* Main functions: `start_project`, `open_rmd`, `build_site`, `commit_site`, `create_results`

# workflowr 0.1.0

* Initial demonstration of idea.
