# workflowr 1.4.0.9001

* Require git2r >= 0.26.0 to support internal changes that increase speed and
robustness of workflowr Git functionality.

* Add option `suppress_report` to suppress the insertion of the workflowr report
at the top of the HTML file (@kaneplusplus, #168)

* `wflow_start()` adds a `.gitattributes` file that classifies R Markdown files
as R code for the official GitHub language statistics calculated via
[linguist][]. The default setting is to ignore R Markdown files.

[linguist]: https://github.com/github/linguist

* Address [callr 3.3.0 bug][callr-bug-3.3.0] that writes objects to the global
environment, causing the workflowr check of the global environment to fail. The
failed check now explains that the problem can be fixed by updating the callr
package.

[callr-bug-3.3.0]: https://github.com/r-lib/callr/commit/9f7665e1081da6f5134b214da694b4461d05659f

# workflowr 1.4.0

This minor release of workflowr features further GitHub integration, a new
reproducibility check, and various improvements and bug fixes.

## New features

* The initial GitHub setup always included the manual step of creating the
GitHub repository, but this is no longer the case! When you run
`wflow_use_github("username")`, it will offer to create the new GitHub
repository for you. If you sign-in to GitHub via your web browser and grant
workflowr permission, it will be created automatically.

* Absolute paths to files on your local computer are not reproducible. If you or
someone else tries to execute the code on a different machine, it will fail. Now
workflowr will automatically search for absolute paths to files that are inside
of the workflowr project. If it detects any, it will fail the reproducibility
check, and provide you with the equivalent relative paths to use.

* Now every time you push your latest changes with `wflow_git_push()`, a new
browser tab will automatically be opened to your online repository
(idea from @pcarbo)

## Minor improvements and bug fixes

* Mention `knitr::include_graphics()` as an option for including external images
(idea from @Zepeng-Mu, #162)
* Check if Git repository is locked and produce error message that explains how
to fix it (idea from @brimittleman)
* Document that `wflow_build()` and `wflow_publish()` build the files in the
given order if they are provided explicitly to the argument `files` (idea from
@antass and @pcarbo, #164)
* Handle spaces in chunk names for HTML targets. If a chunk name contains a
space, the name of the figure file will also has a space. This broke the
accompanying link for the HTML button with the table of previous versions of the
figure (because it uses the filename to be unique). Now the spaces are removed
in the HTML link.
* Add FAQ entry on installing packages in a workflowr project (idea from
@xiangzhu, #160)
* `wflow_use_github()`/`wflow_use_gitlab()` provide better guesses if the
arguments `username` or `repository` are left blank. If `repository` is `NULL`,
it is set to the name of the workflowr project directory (idea from @pcarbo).
* `wflow_git_push()` and `wflow_git_pull()` no longer accept direct URLs to
remote repositories. The argument `remote` must be `NULL` or the name of an
existing remote. The support for direct URLs was likely rarely used since it is
rarely used with Git as well, and it likely never worked given how the
underlying functions from [git2r][] work.
* Document in the GitLab vignette that the repository will be automatically
created the first time the repository is pushed with `wflow_git_push()` (this is
a feature unique to GitLab)

[git2r]: https://cran.r-project.org/package=git2r

# workflowr 1.3.0

This minor release of workflowr introduces two new functions, RStudio Addins,
and various minor improvements.

## New functions

* The new function `wflow_toc()` builds a table of contents of the published R
Markdown files in a workflowr project (@JiaxiangBU, #151, #155)

* The new function `wflow_rename_proj()` renames a workflowr project throughout
all its project files (idea from  @frm1789 and @kbroman, #148)

## RStudio Addins

[RStudio Addins][rstudio-addins] allow you to execute R code via the RStudio
Addins menu. For extra convenience, you can [bind the addins to keyboard
shortcuts][rstudio-addins-shortcuts]. The following workflowr functions have
addins:

* `wflow_build()`
* `wflow_publish()`
* `wflow_status()`
* `wflow_toc()`
* `wflow_view()`

Note that the addin for `wflow_publish()` is a Shiny Gadget that enables you to
interactively choose which files to publish and write a detailed commit message
(assistance from @zaynaib and @argdata, #143).

[rstudio-addins]: https://rstudio.github.io/rstudioaddins/
[rstudio-addins-shortcuts]: https://rstudio.github.io/rstudioaddins/#keyboard-shorcuts

## Minor improvements and bug fixes

* `wflow_build()` fails early if pandoc is not installed (@zaynaib, #75)

* `wflow_git_push()`/`wflow_git_pull()` fail early if user tries to use SSH
authentication when it is not supported by the current installation of
git2r/libgit2 (#144)

* Fix support for knitr chunk option `collapse` and `indent` (reported by
@pcarbo, #149)

* Fix support for rmarkdown option `keep_md` (see
[rmarkdown Issue 1558][rmarkdown-1558])

[rmarkdown-1558]: https://github.com/rstudio/rmarkdown/issues/1558

* Skip tests that only fail on CRAN servers (this is why there are no macOS
binaries for 1.2.0)

* Add a GitHub Pull Request template

* Rename reproducibility tab "Report" to "Checks" (idea from @pcarbo)

* Fix spacing issue with session information button (reported by @pcarbo, #157)

* `wflow_status()` reports if the configuration files `_workflowr.yml` and
`_site.yml` have been edited

* Disable inline code chunks by default in R Markdown files created by
workflowr. Document how to use inline code chunks in the [FAQ][faq] (discussed
with @rgayler and @Robinlovelace , #140)

[faq]: https://jdblischak.github.io/workflowr/articles/wflow-05-faq.html

# workflowr 1.2.0

This release overhauls the layout of the reproducibility report, adds support
for GitLab, introduces some safety checks and warnings related to caching,
provides more documentation, and more.

## Full support for GitLab

While it has always been possible to host workflowr projects using platforms
other than GitHub, it was less convenient and not all the features were
supported. With this release, a workflowr project hosted on GitLab has all the
same features as a workflowr project hosted on GitHub, including links to past
versions of the R Markdown, HTML, and figure files. It's also possible to use
workflowr with GitHub Enterprise or a custom GitLab instance, but you'll have to
coordinate with your administrators to make sure it's possible to host the
website.

* Create vignette "Hosting workflowr websites using GitLab" (written with
@lazappi, #112)
* Add argument `domain` to `wflow_git_remote()` to allow specifying any remote
Git host, e.g. `domain = "gitlab.com"`
* Create function `wflow_use_gitlab()` to automate GitLab setup

## Reproducibility report and other layout changes

The layout of the reproducibility report and other content that workflowr
inserts in the HTML output has been overhauled to be both highly informative but
also collapsed by default. This way the information is there if you need it, but
otherwise is minimally distracting.

* Collapse the reproducibility report (suggested by @timtrice, #110)
* Collapse the "Session information" chunk (suggested by @xiangzhu, #120)
* Convert table of past figure versions to a collapsable button (suggested by
@xiangzhu, #120)
* Improve formatting of tables of past version of files using [Bootstrap table
classes][bootstrap-tables]
* Remove the footer
* Report the knit directory (where the code was executed) in the workflowr
report
* Link to workflowr GitHub repository no longer automatically inserted into
navigation bar. Use either `wflow_use_github()` or `wflow_use_gitlab()` to
insert a link to your workflowr project into the navigation bar

## Improved support for caching

A popular knitr/rmarkdown feature is caching slow-running chunks. This can be
problematic for workflowr because it assumes that the results are newly created
when `wflow_publish()` publishes the results with a given version of the code.
In this release, workflowr now provides warnings, safety checks, and some
convenience arguments for safely using caching.

* Include a check in the reproducibility report that reports any existing cached
chunks
* A warning is inserted directly in the HTML file after any code chunk that is
cached (`cache=TRUE`) but is not set to re-run if its upstream chunks are
changed (`autodep=FALSE`)
* Add argument `clean_fig_files` to `wflow_build()`. The default for
`wflow_build()` is `FALSE`, so that old figures are not removed. This is useful
for iterative development when plots from cached chunks may not be regenerated
during a build. However, `clean_fig_files` is fixed to `TRUE` for
`wflow_publish()` to ensure that the final results are produced during the
build (suggested by @lazappi, #113)
* Add argument `delete_cache` to `wflow_build()`/`wflow_publish()`. The default
is `FALSE`, but if set to `TRUE` it will delete the cache directory prior to
building each R Markdown file. This helps ensure reproducibility of the
published results
* Have `wflow_build()` send message about status of cache directory

## Documentation

In addition to the new vignette on GitLab, this release has multiple other new
vignettes plus updates to existing ones.

* Add vignette "Sharing common code across analyses"
(written with @timtrice, #111, #142)
* Add vignette "Alternative strategies for deploying workflowr websites"
    * Password-protected site with Amazon S3 (written by @edavidaja, #124)
    * Secure sharing with Beaker Browser (written by @johnsonlab, #59, #65)
* Update getting started vignette to use new function `wflow_use_github()`
* Add section "Style with custom CSS" to customization vignette
* Add FAQ entry "How can I include external images in my website?"
* Add code of conduct

## Miscellaneous

* Improve support for hosting on Shiny Server. Setting the option
`fig_path_ext: false` in `_workflowr.yml` removes the file extension from the
figure subdirectories, allowing them to be viewed on Shiny Server (implemented
by @Tutuchan, #119, #122)
* Insert warning into HTML if user configures the knitr chunk option
`fig.path`, which workflowr ignores (idea from @lazappi, #114)
* Add argument `disable_remote` to `wflow_start()`. It creates a Git [pre-push
hook][pre-push-hook] that disables the ability to push to a remote repository.
Useful for confidential projects. Currently only available for Linux and macOS
(suggested by @rgayler, #141)
* Refactor and export the individual functions of `wflow_html()` to facilitate
integrating workflowr features into other R Markdown output formats such as
[blogdown][] (suggested by @docmanny, #126)
* New function `wflow_rename()` to rename files and directories, including
committing the change with Git
* Use [fs][] internally to improve cross-platform handling of file paths
* Have `wflow_publish()`/`wflow_git_commit()` fail early if any of the files
have merge conflicts
* Switch from [rawgit][] to [raw.githack][] to serve past versions of HTML files
(#134)
* Have `wflow_git_push()` set the upstream tracking branch by default (see
[git2r Issue 375][git2r-375])
* Have `wflow_build()` report the current working directory. If the knit
directory (where the code is executed) is different than the working directory,
have `wflow_build()` report where the code in each file is being executed
* As usual, some minor bug fixes, improved error handling, and more tests

[blogdown]: https://github.com/rstudio/blogdown
[bootstrap-tables]: https://www.w3schools.com/bootstrap/bootstrap_tables.asp
[fs]: https://github.com/r-lib/fs
[git2r-375]: https://github.com/ropensci/git2r/issues/375
[pre-push-hook]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks
[rawgit]: https://rawgit.com/
[raw.githack]: https://raw.githack.com/

# workflowr 1.1.1

Fix unit test on CRAN Windows servers.

# workflowr 1.1.0

workflowr 1.1.0 is a maintenance release. It includes some minor new features,
improved error handling, and bug fixes. Critically, it makes workflowr
compatible with the latest release of [git2r][].

[git2r]: https://cran.r-project.org/package=git2r

The most noticeable changes are 1) `wflow_publish()`/`wflow_status()` are now
much faster, especially if your project has many R Markdown files and/or many
Git commits, 2) the rmarkdown package is no longer loaded automatically when you
load workflowr, and 3) the new function `wflow_open()` (based on a previous
version in [workflowrBeta][]) to open new or existing R Markdown files.

[workflowrBeta]: https://github.com/jdblischak/workflowrBeta

## New features

* Resurrect `wflow_open()`. Since there is no longer a workflowr template in
workflowr 1.0+, it creates a file with a minimal YAML header (@xiangzhu, #107)
* Add bibliography before session information (reported by @docmanny, #102)
* Add argument `verbose` to `wflow_build()`/`wflow_publish()` to display the
build log in the R console as each file is built. Useful for monitoring
long-running code chunks (idea from @pcarbo)
* Argument `dry_run` added to `wflow_start()`
* S3 print method for `wflow_start()`
* Updates to `wflow_view()`:
    * Renamed argument `recent` to `latest` to display the most recently
    modified HTML file
    * The argument `files` now requires correct paths to existing R Markdown or
    HTML files. Previously, `wflow_view()` would guess the correct path. While
    convenient, it was also potentially confusing since it was inconsistent with
    the other workflowr functions
    * The argument `latest` is no longer mutually-exclusive with `files`. If the
    most recently modified HTML is different than those specified by `files`,
    they will all be opened for viewing
    * S3 print method
    * Do not attempt to open HTML files with `browseURL()` if
    `getOption("browser")` does not provide a default option

## Internal changes

* Dramatically increase speed of `wflow_status()`/`wflow_publish()` by using
`git2r::odb_blobs()` to obtain past commit times of files (if these functions
are still slow for you, try running `git gc` in the Terminal)
* Make workflowr compatible with latest release of [git2r][] as well as previous
versions
* Move rmarkdown from Depends to Imports. Unlike earlier versions, it's no
longer necessary to call rmarkdown functions directly when using workflowr
(which was the original intention of attaching rmarkdown). Also there is the
potential for problems to arise from the order in which packages are loaded in a
user's R session (described [here][leeper-depends])

[leeper-depends]: https://github.com/leeper/Depends

* Remove the fields `include`/`exclude` from the template `_site.yml`. Also
remove the file `analysis/.nojekyll` (related to point above). Unlike
`rmarkdown:::default_site`, `wflow_site()` does not copy the entire directory to
the output directory, so these fields have no effect. See the R Markdown
documentation on [Included files][included-files] for more information

[included-files]: https://rmarkdown.rstudio.com/rmarkdown_websites.html#included-files

*  Add GitHub Issue Templates (created by @pcarbo)

## Bug fixes and improved error handling

* Fix Windows-specific bug in `wflow_publish()`, `wflow_remove()`, and
`wflow_git_commit()`. The bug prevented Windows users from running these
functions from a subdirectory of a workflowr project

* Restore all files in website directory if `wflow_publish()` fails to build any
of the files (reported by @pcarbo)

* If a user has not set the Git configuration variables `user.name` and
`user.email`, any workflowr function that creates a Git commit will throw an
informative error message. Previously this was only done for `wflow_start()`,
but has been expanded to `wflow_git_commit()`, `wflow_publish()`, and
`wflow_remove()` (idea from @pcarbo)

* Ensure that `wflow_build()` creates a new `.nojekyll` file if necessary and
that `wflow_publish()` commits it. This is most useful when changing the name of
the output directory (#72)

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

* Introduce `wflow_html()` and `wflow_site()` to overhaul the reproducibility
features of workflowr
* Improve API consistency:
    * `wflow_commit()` -> `wflow_git_commit()`
    * `wflow_remotes()` -> `wflow_git_remote()`
* Remove some less commonly used infrastructure files
* Remove template infrastructure: `wflow_open()` and `wflow_convert()`
* Reimplement `wflow_update()` to update a pre-1.0 workflowr project to a
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
facilitate troubleshooting

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
permission issues when multiple workflowr users try to use the same machine
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

* Added FAQ on sharing workflowr sites securely using Beaker Browser
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
If instead files are specifically stated, those files will be built.

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

* All workflowr functions should now accept the file extension `.rmd` in addition
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

* `wflow_commit` can optionally add and commit provided files (argument is `commit_files`) before re-building website and committing HTML files

* `wflow_open` accepts multiple filenames

* Both `wflow_build` and `wflow_commit` have the argument `all` to optionally build all the HTML files

# workflowr 0.2.0

* First alpha release. Contains enough functions to create workflowr project from scratch.

* Main functions: `start_project`, `open_rmd`, `build_site`, `commit_site`, `create_results`

# workflowr 0.1.0

* Initial demonstration of idea.
