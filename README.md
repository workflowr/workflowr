# workflowr: organized + reproducible + shareable data science in R

[![Travis-CI Build Status](https://travis-ci.org/jdblischak/workflowr.svg?branch=master)](https://travis-ci.org/jdblischak/workflowr)
[![codecov](https://codecov.io/gh/jdblischak/workflowr/branch/master/graph/badge.svg)](https://codecov.io/gh/jdblischak/workflowr)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/jdblischak/workflowr?branch=master&svg=true)](https://ci.appveyor.com/project/jdblischak/workflowr)
[![CRAN status](https://www.r-pkg.org/badges/version/workflowr)](https://cran.r-project.org/package=workflowr)

The workflowr R package helps scientists organize their research in a way that
promotes effective project management, reproducibility, collaboration, and
sharing of results. Workflowr combines literate programming (knitr and
rmarkdown) and version control (Git, via git2r) to generate a website containing
time-stamped, versioned, and documented results. Any R user can quickly and
easily adopt workflowr.

For an example, see the [Divvy data exploration project][divvy].

To keep up-to-date with the latest workflowr developments, please join the
[workflowr-announce][] mailing list (low-volume, read-only).

[workflowr-announce]: https://groups.google.com/forum/#!forum/workflowr-announce

* [Features](#features)
* [Quick start](#quick-start)
* [Installation](#installation)
* [Attribution](#attribution)

## Features

* Organized
    * Provides a project template with organized subdirectories
    * Mixes code and results with R Markdown
    * Uses Git to version both source code and results
* Reproducible
    * Displays the code version used to create each result
    * Runs each analysis in an isolated R session
    * Records the session information of each analysis
    * Sets the same seed for random number generation for each analysis
* Shareable
    * Creates a website to present your research results
    * Documents how to host your website for free via [GitHub Pages][]
    * Creates links to past versions of results

## Quick start

```
library("workflowr")

# Configure Git (only need to do once per computer)
wflow_git_config(user.name = "Full Name", user.email = "email@domain")

# Start a new workflowr project
wflow_start("myproject")

# Build the site
wflow_build()

# Customize your site!
#   1. Edit the R Markdown files in analysis/
#   2. Edit the theme and layout in analysis/_site.yml
#   3. Add new or copy existing R Markdown files to analysis/

# Preview your changes
wflow_build()

# Publish the site, i.e. version the source code and HTML results
wflow_publish("analysis/*", "Start my new project")
```

**Next steps:**

1. Read the full [Getting started vignette][vig-start] to learn how to share
your results online via GitHub

1. Read the [customization vignette][vig-custom] for ideas on how to customize
your research website

## Installation

1. Install [R][r]

    * (Recommended) Install [RStudio][rstudio]

    * (Optional) Install [pandoc][pandoc-install]
    
    * (Optional) Install [Git][git]

1. Create an account on [GitHub][gh]

1. Install [workflowr][] from [GitHub][gh] using [devtools][]:

    ```r
    #install.packages("devtools")
    devtools::install_github("jdblischak/workflowr")
    ```

## Attribution

Workflowr was developed, and is maintained, by John Blischak, a postdoctoral
researcher in the laboratory of [Matthew Stephens][stephens] at [The University
of Chicago][uchicago]. He is funded by a grant from the [Gordon and Betty Moore
Foundation][moore] to MS. [Peter Carbonetto][pcarbo] and [Matthew
Stephens][stephens] are co-authors.

We are very thankful to workflowr [contributors][] for helping improve the
package. We are also grateful for workflowr users for testing the package and
providing feedback---thanks especially to [Lei Sun][lsun], [Xiang
Zhu][xiangzhu], [Wei Wang][nkweiwang], and other members (past and present) of
the Stephens lab.

The workflowr package uses many great open source packages. Especially critical
for this project are the R packages [git2r][], [knitr][], and [rmarkdown][].
Please see the vignette [How the workflowr package works][vig-details] to learn
about the software that makes workflowr possible.

Workflowr is available under the [MIT license][MIT]. Please see the file
`CITATION` for proper attribution.

[adapr]: https://github.com/gelfondjal/adapr
[analysis_framework]: https://github.com/jimhester/analysis_framework
[appveyor]: https://ci.appveyor.com
[blogdown]: https://github.com/rstudio/blogdown
[blogR]: https://github.com/rmflight/blogR
[bookdown]: https://github.com/rstudio/bookdown
[cboettig/template]: https://github.com/cboettig/template
[Codecov]: https://codecov.io/
[contributing]: https://github.com/jdblischak/workflowr/blob/master/CONTRIBUTING.md
[contributors]: https://github.com/jdblischak/workflowr/graphs/contributors
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
[lsun]: https://github.com/LSun
[makeProject]: https://cran.r-project.org/web/packages/makeProject/index.html
[manuscriptPackage]: https://github.com/jhollist/manuscriptPackage
[MIT]: https://opensource.org/licenses/mit-license.php
[moore]: https://www.moore.org/
[nkweiwang]: https://github.com/NKweiwang
[Pakillo/template]: https://github.com/Pakillo/template
[pandoc]: http://pandoc.org
[pandoc-install]: https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md
[pcarbo]: https://pcarbo.github.io/
[pkgdown]: http://hadley.github.io/pkgdown/
[poirot]: https://github.com/ramnathv/poirot
[prodigenr]: https://github.com/lwjohnst86/prodigenr
[pRojects]: https://github.com/lockedata/pRojects
[ProjectTemplate]: https://github.com/johnmyleswhite/ProjectTemplate
[pt]: https://rstudio.github.io/rstudio-extensions/rstudio_project_templates.html
[r]: http://cran.r-project.org
[r-exts]: https://cran.r-project.org/doc/manuals/R-exts.html
[r-exts-licensing]: https://cran.r-project.org/doc/manuals/R-exts.html#Licensing
[r-pkg]: http://r-pkgs.had.co.nz/
[rddj-template]: https://github.com/grssnbchr/rddj-template
[researchcompendium]: https://github.com/benmarwick/researchcompendium
[rmarkdown]: http://rmarkdown.rstudio.com/
[rmd-notebook]: https://github.com/lmullen/rmd-notebook
[rmflight]: https://github.com/rmflight
[rmflight-post]: https://rmflight.github.io/posts/2014/07/analyses_as_packages.html
[rOpenSci]: https://ropensci.github.io/reproducibility-guide/
[roxygen2]: https://github.com/klutometis/roxygen
[rr-init]: https://github.com/Reproducible-Science-Curriculum/rr-init
[rrtools]: https://github.com/benmarwick/rrtools
[rsmith]: https://github.com/hadley/rsmith
[rstudio]: https://www.rstudio.com/products/rstudio/download/
[samantha]: https://github.com/DASpringate/samatha
[singleCellSeq]: https://jdblischak.github.io/singleCellSeq/analysis/
[stephens]: http://stephenslab.uchicago.edu/
[swc]: https://software-carpentry.org
[swc-git]: https://swcarpentry.github.io/workshop-template/#git
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
[xiangzhu]: https://github.com/xiangzhu
[zoon]: https://github.com/zoonproject/zoon
