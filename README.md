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

For more details, see the [online documentation][workflowr-docs]. For an
example, see the [Divvy data exploration project][divvy]. To keep up-to-date
with the latest workflowr developments, please join the [workflowr-announce][]
mailing list (low-volume, read-only). For bugs reports, feature requests, and
questions, please open an [Issue][issues].

For those with existing workflowr projects (pre-1.0.0), see `?wflow_update` if
you're interested in updating your project to use the latest features. If you
like your current project the way it is, you can continue to use workflowr as
you have been by getting the latest bug fixes from [workflowrBeta][].

[workflowr-announce]: https://groups.google.com/forum/#!forum/workflowr-announce
[workflowrBeta]: https://github.com/jdblischak/workflowrBeta

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

To see a workflowr website in action, see this [video demonstration][video].

For related tools, see [r-project-workflows][].

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

1. Read the [migrating vignette][vig-migrating] for how to integrate workflowr
into your existing project

## Installation

1. Install [R][r]

    * (Recommended) Install [RStudio][rstudio]

    * (Optional) Install [pandoc][pandoc-install]
    
    * (Optional) Install [Git][git]

1. Install workflowr from [CRAN][cran]:

    ```r
    install.packages("workflowr")
    ```

1. Create an account on [GitHub][gh]

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

Workflowr is available under the [MIT license][MIT]. Please run
`citation("workflowr")` for proper attribution.

[contributors]: https://github.com/jdblischak/workflowr/graphs/contributors
[cran]: https://cran.r-project.org/package=workflowr
[divvy]: https://stephenslab.github.io/wflow-divvy/
[gh]: https://github.com
[git]: https://git-scm.com/
[git2r]: https://cran.r-project.org/web/packages/git2r/index.html
[GitHub Pages]: https://pages.github.com/
[issues]: https://github.com/jdblischak/workflowr/issues
[knitr]: https://github.com/yihui/knitr
[lsun]: https://github.com/LSun
[MIT]: https://opensource.org/licenses/mit-license.php
[moore]: https://www.moore.org/
[nkweiwang]: https://github.com/NKweiwang
[pandoc]: http://pandoc.org
[pandoc-install]: https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md
[pcarbo]: https://pcarbo.github.io/
[r]: http://cran.r-project.org
[rmarkdown]: http://rmarkdown.rstudio.com/
[r-project-workflows]: https://github.com/jdblischak/r-project-workflows#readme
[rstudio]: https://www.rstudio.com/products/rstudio/download/
[stephens]: http://stephenslab.uchicago.edu/
[uchicago]: http://www.uchicago.edu/
[video]: https://www.youtube.com/watch?v=O1wv94sZfvE
[vig-custom]: https://jdblischak.github.io/workflowr/articles/wflow-02-customization.html
[vig-details]: https://jdblischak.github.io/workflowr/articles/wflow-04-how-it-works.html
[vig-migrating]: https://jdblischak.github.io/workflowr/articles/wflow-03-migrating.html
[vig-start]: https://jdblischak.github.io/workflowr/articles/wflow-01-getting-started.html
[workflowr-docs]: https://jdblischak.github.io/workflowr/
[xiangzhu]: https://github.com/xiangzhu
