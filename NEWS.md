# dev

* Fix missing figure version table on Windows (implemented by @warmdev in #275)

# workflowr 1.7.0

This minor release includes some new features, improved documentation, and bug
fixes.

## Minor improvements

* New argument `combine` for `wflow_build()` and `wflow_publish()`. When Rmd
files are specified with the argument `files`, they are built in addition to any
Rmd files that are automatically built when setting arguments like `make = TRUE`
and `republish = TRUE`. If you would instead like to only build Rmd files that
are included in all the filters, you can set `combine = "and"` to take the
intersection. For example, if you ran `wflow_build("analysis/example*.Rmd", make
= TRUE, combine = "and")`, then this would only build those Rmd files matched by
the file glob `analysis/example*.Rmd` and had been modified more recently than
their corresponding HTML file. With the default, `combine = "or"`, this would
have built all the files that matched the file glob in addition to any files
that had been modified more recently than their corresponding HTML file, even if
they didn't match the file glob pattern (idea from @bfairkun in #221,
implementation by @zaynaib in #227, #228)

* If `wflow_publish()` is called, but was not instructed which files to publish,
it now throws an error. In other words, you must specify the files you wish to
publish or use one of the convenience arguments like `republish = TRUE` or
`update = TRUE`. It's previous behavior was to complete without having done
anything, which was misleading (idea from @stephens999)

* It is now easier to enter commit messages with a separate title and body. If
you pass a character vector to the argument `message` to any of the functions
that perform a commit, e.g. `wflow_publish()`, the first element will be used as
the title, and any subsequent elements will be separate paragraphs in the commit
body. Using a separate title and body will improve the display of your commit
messages on GitHub/GitLab and `git log --oneline` since these only show the
title (suggestion from @LearnUseZone in #222, implementation by @zaynaib in #225)

* New argument `only_published` for `wflow_toc()`. If set to `FALSE`, then the
table of contents will also include unpublished files (implemented by @giocomai
in #234)

## Updated documentation

* Improved organization of [reproducible research workshop
vignette][vig-workshop] (thanks to @stephens999)

[vig-workshop]: https://workflowr.github.io/workflowr/articles/wflow-09-workshop.html

* Added more documentation to `wflow_build()` to explain when it does and
doesn't load code defined in a `.Rprofile` file (idea from @pcarbo)

## Bug fixes

* Bug fix: Now workflowr will detect any problems with its dependencies when it
is attached. All dependencies must be installed, loadable, and meet the minimum
required version. Broken packages were causing cryptic, misleading errors
(reported by in @markellekelly in #216 and @LearnUseZone in #217)

* Bug fix: `wflow_quickstart()` can now handle relative paths to the Rmd files
when the working directory is changed (`change_wd = TRUE`, which is the default)

* Remove Rd warnings when installing package on Windows by explicitly specifying
the topic page when cross-referencing an exported function from another package.
Note that the links worked previously, so this change is just being proactive in
case this warning starts getting strongly enforced. If the authors of the other
package rearrange how they group functions into documentation topics, this will
break the cross-references and require an update. See this
[thread][rs-community-rd-warning] for more details

[rs-community-rd-warning]: https://community.rstudio.com/t/file-link-quasiquotation-in-package-rlang-does-not-exist-and-so-has-been-treated-as-a-topic/55774

* Bug fix: `wflow_use_github()` and `wflow_use_gitlab()` now use Font Awesome 5
syntax to insert icons into the navigation bar when a recent version of
rmarkdown is installed (>= 2.6) (bug report from @christianholland, #231)

* Bug fix: `wflow_open()` no longer sends a warning if you are using
`bookdown::html_document2` as your primary output format in `_site.yml` with
`base_format: workflowr::wflow_html` (bug report from @rgayler, #233)

## Miscellaneous

* Removed function `wflow_update()`. Its only purpose was to migrate projects
created with [workflowrBeta][], which is now over 3 years old

* Bump minimum required version of R from 3.2.5 to 3.3.0. While workflowr itself
should be able to continue to work fine with R 3.2.5, it was becoming too much
of a burden to regularly test workflowr with R 3.2.5 as the RStudio engineers
have started updating their packages to require a minimum of R 3.3.0

* Require minimum versions of callr 3.7.0, knitr 1.29, rmarkdown 1.18

* Switched to the workflowr repository itself to use the default branch "main"
and changed the owner to the workflowr organization. This has no effect on
workflowr projects (future or existing). It mainly affects contributors to
workflowr development. However, please update any links you might have
bookmarked (e.g. to documentation)

# workflowr 1.6.2

This patch release of workflowr includes minor improvements, updated
documentation, and bug fixes. Users are encouraged to upgrade workflowr so that
it is compatible with the next release of [fs][] (>1.4.1).

## Minor improvements

* Make the HTTP calls by `wflow_use_github()` to the GitHub API more resilient
to transient network issues by automatically retrying HTTP calls more than once
(@jameslamb, #199, #201)
* Enhance the RStudio Project Template to include more arguments from
`wflow_start()` (@trannhatanh89, #193, #200)
* Throw error early if user sets `overwrite = TRUE` to `wflow_start()` but
`existing = FALSE`, since it isn't possible to overwrite non-existent files
(@skpurdue, #194, #202, idea from @pcarbo)

## Updated documentation

* Update FAQ entry on external images to note that the website directory for
GitLab Pages is `public/`, not `docs/` (idea from @ryurko)

## Bug fixes

* Send warning if Python plots created using outdated version of reticulate (<
1.15) (bug report from @lazappi, #181)
* Make workflowr compatible with fs version 1.4.1.9000+ (@jimhester, #204)
* Set minimum version requirement for dependency rprojroot to 1.2
* Skip test of error handling of deleted working directory on Solaris

# workflowr 1.6.1

This patch release of workflowr includes minor improvements, updated
documentation, bug fixes, and lots of internal refactoring.

## Minor improvements

* Workflowr functions check if the working directory exists
(idea from @pcarbo, #185)

* `wflow_use_github()` can now create repositories for GitHub organizations
using the new argument `organization`. However, GitLab Groups should still be
specified with the argument `username` for `wflow_use_gitlab()`
(bug report from @stephens999, #186)

* Any workflowr function that prompts for user input will continue to re-prompt
until valid input has been entered (or canceled by hitting the key `Esc`)
(idea from @pcarbo)

* All functions that commit to the Git repository first check for the
availability of the Git variables user.name and user.email (either global or
local), which are required for creating a commit
(#85)

* The workflowr icon is displayed in browser tabs of workflowr websites
(idea from @pcarbo)

## Updated documentation

* Document that GitLab.com provides private repositories with access control to
the source code repository and the website
(#187)

* Update URL to RStudio-specific instructions for installing pandoc
(bug report from @PietrH, #190)

* Document in the FAQ vignette how to include an external image using an
absolute URL to the image hosted on another website
(idea from @JiaxiangBU, #159)

* Document how to embed Shiny apps with `knitr::include_app()` and
[shinyapps.io](https://www.shinyapps.io/)
(idea from @rsimon64, #130)

* Update the FAQ on including external images to account for the breaking change
in `knitr::include_graphics()` introduced in knitr version 1.28
(bug report from @ditordccaa, #103)

* Add FAQ entry on how to use a Git hosting site that uses the HTTP protocol
(idea from @antass, #163)

## Bug fixes

* Fix Windows-specific bug that caused the table of past versions to be missing
from the workflowr report (bug introduced in version 1.5.0)

## Internal refactoring

* Internal refactoring for increased speed and improved error handling of input
arguments

* Check for class with `inherits()`
(#189)

* Switch to use pandoc option `--include-in-header` to insert workflowr-specific
CSS and other metadata (surprisingly `--include-before-body` works fine)

# workflowr 1.6.0

## New features

* New function `wflow_run()`. It executes the code chunks of an R Markdown file
in the current R session without affecting any of the website files. This is
meant to be used while interactively developing an analysis. It does not change
the working directory or isolate the computation from the current R session.
This is analogous to the RStudio option "Run all" to run all the code chunks
(idea from @pcarbo)

* New autosave feature. The workflowr functions `wflow_build()`,
`wflow_publish()`, and `wflow_status()` will autosave any unsaved files open in
the RStudio editor pane. This is similar to the behavior of the Knit HTML
button. This feature can be disabled by setting the package option
`workflowr.autosave` to `FALSE` (idea from @xiangzhu in #179)

* New [vignette][vig-data] on using large data files in a workflowr project
(motivated by @xiangzhu, #183)

[vig-data]: https://workflowr.github.io/workflowr/articles/wflow-10-data.html

* If there are merge conflicts after running `wflow_git_pull()`, and the merge
was allowed to proceed (`fail = FALSE`), then the conflicted files are listed
and optionally opened in RStudio at the first line of the conflict that needs
to be resolved

* `wflow_git_config()` has a new argument `overwrite`. Previously
`wflow_git_config()` would by default overwrite any previous settings. Now this
will throw an error. To overwrite a previous setting, set `overwrite = TRUE`
([idea][f1000-config-overwrite] from @petebaker)

[f1000-config-overwrite]: https://f1000research.com/articles/8-1749/v1#referee-response-55117

## Minor improvements and bug fixes

* Warn user if only HTML file has been committed (and avoid throwing an error).
Previously this threw an error because workflowr expects the R Markdown file to
be committed to the Git repo if its corresponding HTML file is
(bug report from @kevinlkx)

* Warn user if a dependency does not meet the minimum required version. There
are multiple ways this could happen. First, it is possible to install an old
version after having installed workflowr. Second, when running
`install.packages()`, if the minimum required version is available in any of the
package libraries, it is not installed. However, if the version of the package
in the first directory listed in `.libPaths()` does not meet the minimum
required version, it is still the one that is loaded when workflowr is loaded
(idea from @stephens999)

* Fix off-by-one date bug by specifying the local timezone (see [git2r Issue
407][git2r407])

[git2r407]: https://github.com/ropensci/git2r/issues/407

* Fix bug when path to project includes a space. The bug was introduced in
version 1.5.0 with the feature to use the system Git executable to run `git
ls-files`. To fix the issue in version 1.5.0, set `options(workflowr.sysgit =
"")` in the file `.Rprofile` (bug report from @wolfemd, #180)

* Fix bug caused by an unset timezone. If the machine has no timezone set,
workflowr will default to Etc/UTC

* Handle missing title/pagetitle warning from pandoc 2+ and rmarkdown 1.18+
(see [rmarkdown Issue 1355][rmarkdown1355])

[rmarkdown1355]: https://github.com/rstudio/rmarkdown/pull/1355#issuecomment-558817744

* Improve speed of `wflow_publish()`/`wflow_status()` by using the system Git
executable (if available) to obtain the last commit time of the analysis files
(this is used to determine which published Rmd files are outdated and need to be
republished)

* Report exact command to run `git push` or `git pull` in the terminal if either
`wflow_git_push()` or `wflow_git_pull()` fail (reported by @jennysjaarda, #182)

* Update FAQ to include how to create a PDF using the RStudio Knit button
(reported by @han16)

* Update citation to [workflowr publication](https://doi.org/10.12688/f1000research.20843.1)

* Properly quote the Git executable with `shQuote()` whenever Git is called
from R

# workflowr 1.5.0

This minor release of workflowr includes a new function, the introduction of
options to control package-wide function behavior, the ability to suppress the
workflowr report from the HTML file, a new vignette for teaching workflowr, and
lots of error handling improvements.

## New function `wflow_quickstart()`

The new function `wflow_quickstart()` provides a simple interface to
effortlessly create a workflowr project from an existing data analysis. Pass it
your existing R Markdown file(s), and it will start a new workflowr project,
publish the analysis files, and configure GitHub (or GitLab).

```
wflow_quickstart(files = "existing-analysis.Rmd", username = "your-github-username")
```

## Package options

This is the first release to include options for controlling the behavior of all
workflowr functions. This makes it more convenient for you to create a
consistent workflowr experience. You can set the options in your project's
`.Rprofile` (using the function `options()`) instead of having to always
remember to change a default argument every time you call a function.

Currently there are two workflowr package options. See `?workflowr` for more
details.

* `workflowr.git`: Set the path to the system Git executable, which is
occasionally used to increase the speed of Git operations performed by workflowr
functions.

* `workflowr.view`: Should workflowr functions open webpages for viewing in the
browser? The default is set to `interactive()` (i.e. it is `TRUE` only if it is
an interactive R session). This option is currently used by `wflow_build()`,
`wflow_git_push()`, and `wflow_publish()`.

## Workshop tutorial for teaching workflowr

The new vignette "Reproducible research with workflowr" is designed to be taught
as a tutorial in a workshop setting. It includes setup instructions, an example
analysis to highlight the benefits of workflowr, and troubleshooting advice.

## Suppress the HTML workflowr report

If you'd like to suppress the workflowr report at the top of an HTML page, you
can set the option `suppress_report` to `TRUE`. To suppress the report in every
HTML file, set the option in `_workflowr.yml`:

```
suppress_report: TRUE
```

To suppress the report in a specific HTML file, add the following to the YAML
header of the corresponding Rmd file:

```
workflowr:
  suppress_report: TRUE
```

Many thanks to @kaneplusplus for implementing this feature! (#168)

## Minor improvements and bug fixes

* Require git2r >= 0.26.0 to support internal changes that increase speed and
robustness of workflowr Git functionality

* `wflow_start()` adds a `.gitattributes` file that classifies R Markdown files
as R code for the official GitHub language statistics calculated via
[linguist][]. The default setting is to ignore R Markdown files.

[linguist]: https://github.com/github/linguist

* Address [callr 3.3.0 bug][callr-bug-3.3.0] that writes objects to the global
environment, causing the workflowr check of the global environment to fail. The
failed check now explains that the problem can be fixed by updating the callr
package.

[callr-bug-3.3.0]: https://github.com/r-lib/callr/commit/9f7665e1081da6f5134b214da694b4461d05659f

* Warn user from `wflow_build()` if `index.Rmd` is missing the
workflowr-specific site generator `wflow_site()` (idea from @pcarbo, #177)

* Fail early if missing required file `index.Rmd`

* Add argument `fail` to `wflow_git_pull()` with default value of `TRUE`. Now if
a pull generates a merge conflict, `wflow_git_pull()` will abort the pull. Thus
no changes will be made to the local files. Users can set `fail = FALSE` to allow
Git to add the merge conflicts to the local files.

* Speed improvements for `wflow_publish()`

* Improved error handling when files contain merge conflicts. Before workflowr
only detected merge conflicts in Rmd files. Now it detects them for any file
(since Git requires any merge conflicts to be resolved before it makes any new
commits).

* Warn user if `knit_root_dir` (the directory where the code in the Rmd files is
executed) defined in `_workflowr.yml` is an absolute path. An absolute path
would only work on the current computer, limiting reproducibility.

* Include Git status in output of `wflow_status()`. Note that it purposefully
excludes any files in the website directory since these generated files should
only be committed by workflowr. You can omit the Git status by setting
`include_git_status = FALSE` (idea from @pcarbo)

* Make it clearer that you have two options for creating the remote repository on
Github: 1) let `wflow_use_github()` do it automatically, or 2) create it yourself
manually at https://github.com/new (idea from @pcarbo)

* New FAQ "How can I save a figure in a vector graphics format (e.g. PDF)?"

* Added citation to [F1000Research
paper](https://doi.org/10.12688/f1000research.20843.1). Run
`citation("workflowr")` to obtain the new citation information.

* Fixed `wflow_start()` infinite recursion bug by requiring stringr >=1.3.0

* Added httpuv as imported dependency so that `wflow_use_github()` is able to
automatically create the GitHub repository via the httr package

* Set (or increased) minimum required versions for fs, git2r, httpuv,
rstudioapi, stringr, whisker, clipr, shiny, testthat, and withr

* Document possible error of a greyed out GitHub authentication button when
trying to give permission for workflowr to create a repository for your account

* Fixed bug in date displayed in table of past versions in the workflowr report.
Depending on the time of day the commit was made, the displayed day may have
been off by one.

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

[faq]: https://workflowr.github.io/workflowr/articles/wflow-05-faq.html

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

[included-files]: https://bookdown.org/yihui/rmarkdown/rmarkdown-site.html

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
devtools::install_github("workflowr/workflowr")

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
[a8ce711](https://github.com/workflowr/workflowr/commit/a8ce711de4ea2939bf76f2c2403c1d631ec130b0#diff-08716e76a3c9aeb6ad641bb23e5e5ea6R126),
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

[pkgdown]: https://pkgdown.r-lib.org/
[travis]: https://www.travis-ci.com/
[covr]: https://github.com/r-lib/covr
[Codecov]: https://about.codecov.io/

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
