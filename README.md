# workflowr: organized + reproducible + shareable data science in R

[![Travis-CI Build Status](https://travis-ci.org/jdblischak/workflowr.svg?branch=master)](https://travis-ci.org/jdblischak/workflowr)
[![codecov](https://codecov.io/gh/jdblischak/workflowr/branch/master/graph/badge.svg)](https://codecov.io/gh/jdblischak/workflowr)

The [workflowr][] R package makes it easier for researchers to
organize their projects and share their results with colleagues. 

Install the latest release (v0.7.0.9000) by running this command in R or
RStudio:

```r
devtools::install_github("jdblischak/workflowr", build_vignettes = TRUE)
```

If you are already writing R code to analyze data, and know the basics
of Git and GitHub, you can start taking advantage of workflowr
immediately. In a matter of minutes, you can create a research website
like [this][demo01]. (See also the [Divvy data exploration project][divvy]
for a more elaborate example of a workflowr project.)

If you find any problems, or would like to suggest new features,
please open an [Issue][issues].

* [Why use workflowr?](#why-use-workflowr)
* [Quick start](#quick-start)
* [Upgrading](#upgrading)
* [More about this repository](#more-about-this-repository)
* [Background and related work](#background-and-related-work)
* [Credits](#credits)
* [License](#license)
* [Citation](#citation)
* [Pronunciation](#pronunciation)

## Why use workflowr?

First, hopefully you don't need much convincing to write your analyses
in R Markdown. It allows you to combine your R code, text, and figures
in the same document! See the [website][rmarkdown] to learn about all
the cool features.  Second, building a website with the rmarkdown
package (as opposed to using knitr to produce Markdown files and
passing these to a static site generator) enables you to use all the
latest R packages (e.g. [htmlwidgets][]) directly in your
analyses. Third, the workflowr package provides functions to make it
easier for a researcher to maintain a version-controlled R Markdown
website:

* A function to start a project with all the necessary files (see `?wflow_start`)
* Includes an R Markdown template that will automatically insert the date and most recent Git commit ID (i.e. SHA1) at the top of the file to aid reproducibility (see `?wflow_open`)
* Saves generated figures into an organized directory structure
* A function that handles all the version control operations to track code development and also ensures all the R Markdown files are built in a reproducible manner (see `?wflow_publish`)

## Quick start

workflowr builds on several software tools including
[Git][git], [pandoc][] and
[knitr][], **but you do not need to have experience using any of these
tools to get started with workflowr.** You only need to know how to
code in [R][r] and be generally familiar with the
[R Markdown][rmarkdown] format. A [basic
understanding of git][git-for-science]
as well as the [UNIX command line][swc-shell]
is not essential, but helpful.

Here is a minimal set of steps to get you started with workflowr. If you are
already using R and git, you may be able to skip some of these steps.

1. Install [R][r] ([instructions][swc-r] from [Software Carpentry][swc]).

2. (Optional) Install [RStudio][rstudio] (workflowr takes advantages of some
RStudio features, but RStudio is not required to use workflowr)

3. Install [Git][git] ([instructions][swc-git] from [Software Carpentry][swc])

4. Create an account on [GitHub][gh].

5. Configure [Git][git] ([instructions][swc-git-config] from
[Software Carpentry][swc]). Run the following commands in the shell
command-line, inserting your account information:

    ```bash
    git config --global user.name "Your Name"
    git config --global user.email "youremail@domain"
    ```

6. Install the latest stable release of [workflowr][] from
[GitHub][gh] using [devtools][]:

    ```r
    install.packages("devtools")
    devtools::install_github("jdblischak/workflowr", build_vignettes = TRUE)
    ```

7. Work through the vignette,
["Getting started with workflowr"][vig-start], to learn how to set up
a workflowr project. (You can view all the available vignettes locally
with `browseVignettes("workflowr")`.)

8. Alternatively, if you have already started your project, read the
vignette
["Migrating an existing project to use workflowr"][vig-migrating] to
learn how to convert your project to a workflowr project.

9. Learn more about how to [Customize your research website][vig-custom].

10. If you find any unexpected behavior or think of an additional
feature that would be nice to have, please open an Issue
[here][issues]. When writing your bug report or feature request,
please note the version of workflowr you are using (which you can
obtain by running `packageVersion("workflowr")`).

## Upgrading

To upgrade workflowr to the most recent stable release, follow these steps:

* Re-install from [GitHub][gh] with [devtools][]:

```r
devtools::install_github("jdblischak/workflowr", build_vignettes = TRUE)
```

* Preview potential changes to your project files with `wflow_update()`:

```r
library("workflowr")
wflow_update()
```

* To implement these changes, set `dry_run = FALSE`:

```r
wflow_update(dry_run = FALSE)
```

## More about this repository

This repository contains the workflowr R package. If your goal is to create a 
workflowr project, you do **not** need to fork this repository. Instead, 
following the [Quick start](#quick-start) instructions above.

For the most part, I try to follow the guidelines from [R packages][r-pkg] by 
[Hadley Wickham][hadley]. The unit tests are performed with [testthat][], the 
documentation is built with [roxygen2][], the online package documentation is 
created with [pkgdown][], continuous integration testing is performed by [Travis
CI][travis], and code coverage is calculated with [covr][] and [Codecov][].

The template files used by `wflow_start()` to populate a new project are located
in `inst/infrastructure/`. The R Markdown templates used by `wflow_open()` are 
located in `inst/rmarkdown/templates/`. The repository contains the files 
`LICENSE` and `LICENSE.md` to both adhere to [R package conventions for defining
the license][r-exts-licensing] and also to make the license clear in a more 
conventional manner (suggestions for improvement welcome). `document.R` is a 
convenience script for regenerating the documentation. The remaining directories
are standard for R packages as described in the manual [Writing R 
Extensions][r-exts].

If you are interested in contributing to this project, please see these
[instructions][contributing].

## Background and related work

There is lots of interest and development around reproducible research with R. 
Projects like workflowr are possible due to two key developments. First, the R 
packages [knitr][] and [rmarkdown][] have made it easy for any R programmer to 
generate reports that combine text, code, output, and figures. Second, the 
version control software [Git][], the Git hosting site [GitHub][gh], and the 
static website hosting service [GitHub Pages][] have made it easy to share not 
only source code but also static HTML files (i.e. no need to purchase a domain
name, setup a server, etc).

My first attempt at sharing a reproducible project online was [singleCellSeq][].
Basically, I started by copying the documentation website of [rmarkdown][] and 
added some customizations to organize the generated figures and to insert the 
status of the Git repository directly into the HTML pages. The workflowr R 
package is my attempt to simplify my previous workflow and provide helper 
functions so that any researcher can take advantage of this workflow.

workflowr encompasses multiple functions: 1) provides a project template, 2) 
version controls the R Markdown and HTML files, and 3) builds a website. 
Furthermore, it provides R functions to perform each of these steps. There are 
many other related works that provide similar functionality. Some are templates 
to be copied, some are R packages, and some involve more complex software (e.g. 
static blog software). Depending on your use case, one of the related works
listed below may better suit your needs. Please check them out!

* **Project template hosted on GitHub:**
    * [analysis_framework][]
    * [cboettig/template][]
    * [knitr-jekyll][]
    * [Pakillo/template][]
    * [researchcompendium][]
    * [rmd-notebook][]
    * [rr-init][]

* **Project template created via R package:**
    * [makeProject][]
    * [manuscriptPackage][]
    * [prodigenr][]
    * [pRojects][]
    * [ProjectTemplate][]

* **Create websites from R Markdown files:**
    * [blogdown][]
    * [blogR][]
    * [bookdown][]
    * [knowledge-repo][]
    * [lnraw][]
    * [pkgdown][]
    * [poirot][]
    * [rmarkdown][]
    * [rsmith][]
    * [samantha][]

* **Guides for reproducible research with R:**
    * [CRAN Task View: Reproducible Research][cran-rr]
    * [rmflight][]: [Analyses as Packages][rmflight-post]
    * [rOpenSci Guide to Reproducible Research][rOpenSci]

* **Other**:
    * [exreport][]

If you know of other related works I should include, please send a pull request
to the "dev" branch.

## Credits

workflowr was developed, and is maintained, by John Blischak, a
postdoctoral researcher in the laboratory of
[Matthew Stephens][stephens] at
[The University of Chicago][uchicago]. He is funded by a grant from
the [Gordon and Betty Moore Foundation][moore] to MS.  Many others
have contributed to workflowr by testing the package and providing
feedback---thanks especially to Lei Sun, Xiang Zhou and other members 
(past and present) of the Stephens lab.

The workflowr package uses many great open source packages. Especially
critical for this project are the R packages [git2r][], [knitr][], and
[rmarkdown][]. Please see the vignette
[How the workflowr package works][vig-details] to learn about the
software that makes workflowr possible.

## License

workflowr is available under the [MIT license][MIT].

## Citation

To cite workflowr in publications use:

  John D. Blischak, Peter Carbonetto and Matthew Stephens (2017). workflowr: A workflow template for
  creating a research website. R package version 0.7.0.9000. https://github.com/jdblischak/workflowr

A BibTeX entry for LaTeX users is

```
  @Manual{,
    title = {workflowr: A workflow template for creating a research website},
    author = {John D. Blischak and Peter Carbonetto and Matthew Stephens},
    note = {R package version 0.7.0.9000},
    year = {2017},
    url = {https://github.com/jdblischak/workflowr},
  }
```

## Pronunciation

It is common for R packages to end with an "r", and I tend to
pronounce this as if it was "er" because I personally find this the
easiest. Thus I pronounce the package "workflow + er". Other equally
good options are "workflow + R" or "work + flower".

[analysis_framework]: https://github.com/jimhester/analysis_framework
[blogdown]: https://github.com/rstudio/blogdown
[blogR]: https://github.com/rmflight/blogR
[bookdown]: https://github.com/rstudio/bookdown
[cboettig/template]: https://github.com/cboettig/template
[Codecov]: https://codecov.io/
[contributing]: https://github.com/jdblischak/workflowr/blob/master/CONTRIBUTING.md
[covr]: https://github.com/jimhester/covr
[cran-rr]: https://cran.r-project.org/web/views/ReproducibleResearch.html
[demo01]: https://jdblischak.github.io/workflowr-demo01/
[divvy]: https://stephenslab.github.io/wflow-divvy/
[devtools]: https://github.com/hadley/devtools
[exreport]: https://github.com/jacintoArias/exreport
[gh]: https://github.com
[git]: https://git-scm.com/
[git-for-science]: https://doi.org/10.1371/journal.pcbi.1004668
[git2r]: https://cran.r-project.org/web/packages/git2r/index.html
[GitHub Pages]: https://pages.github.com/
[hadley]: http://hadley.nz/
[htmlwidgets]: http://www.htmlwidgets.org/
[issues]: https://github.com/jdblischak/workflowr/issues
[knitr]: https://github.com/yihui/knitr
[knitr-jekyll]: https://github.com/yihui/knitr-jekyll
[knowledge-repo]: https://github.com/airbnb/knowledge-repo
[lnraw]: https://github.com/mmadsen/lnraw
[makeProject]: https://cran.r-project.org/web/packages/makeProject/index.html
[manuscriptPackage]: https://github.com/jhollist/manuscriptPackage
[MIT]: https://opensource.org/licenses/mit-license.php
[moore]: https://www.moore.org/
[Pakillo/template]: https://github.com/Pakillo/template
[pandoc]: http://pandoc.org
[pkgdown]: http://hadley.github.io/pkgdown/
[poirot]: https://github.com/ramnathv/poirot
[prodigenr]: https://github.com/lwjohnst86/prodigenr
[pRojects]: https://github.com/lockedata/pRojects
[ProjectTemplate]: https://github.com/johnmyleswhite/ProjectTemplate
[r]: http://cran.r-project.org
[r-exts]: https://cran.r-project.org/doc/manuals/R-exts.html
[r-exts-licensing]: https://cran.r-project.org/doc/manuals/R-exts.html#Licensing
[r-pkg]: http://r-pkgs.had.co.nz/
[researchcompendium]: https://github.com/benmarwick/researchcompendium
[rmarkdown]: http://rmarkdown.rstudio.com/
[rmd-notebook]: https://github.com/lmullen/rmd-notebook
[rmflight]: https://github.com/rmflight
[rmflight-post]: https://rmflight.github.io/posts/2014/07/analyses_as_packages.html
[rOpenSci]: https://ropensci.github.io/reproducibility-guide/
[roxygen2]: https://github.com/klutometis/roxygen
[rr-init]: https://github.com/Reproducible-Science-Curriculum/rr-init
[rsmith]: https://github.com/hadley/rsmith
[rstudio]: https://www.rstudio.com/products/rstudio/download/
[samantha]: https://github.com/DASpringate/samatha
[singleCellSeq]: https://jdblischak.github.io/singleCellSeq/analysis/
[stephens]: http://stephenslab.uchicago.edu/
[swc]: https://software-carpentry.org
[swc-git]: https://swcarpentry.github.io/workshop-template/#git
[swc-git-config]: http://swcarpentry.github.io/git-novice/02-setup/
[swc-r]: https://swcarpentry.github.io/workshop-template/#r
[swc-shell]: https://swcarpentry.github.io/shell-novice
[testthat]: https://github.com/hadley/testthat
[travis]: https://travis-ci.org/
[uchicago]: http://www.uchicago.edu/
[vig-custom]: https://jdblischak.github.io/workflowr/articles/wflow-02-customization.html
[vig-details]: https://jdblischak.github.io/workflowr/articles/wflow-04-how-it-works.html
[vig-migrating]: https://jdblischak.github.io/workflowr/articles/wflow-03-migrating.html
[vig-start]: https://jdblischak.github.io/workflowr/articles/wflow-01-getting-started.html
[workflowr]: https://jdblischak.github.io/workflowr/
