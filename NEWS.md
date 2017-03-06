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

[pkgdown]: http://hadley.github.io/pkgdown/
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
