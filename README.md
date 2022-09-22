# workflowr: organized + reproducible + shareable data science in R

[![CRAN status](https://www.r-pkg.org/badges/version/workflowr)](https://cran.r-project.org/package=workflowr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/workflowr)](https://cran.r-project.org/package=workflowr)
[![DOI](https://zenodo.org/badge/75893305.svg)](https://zenodo.org/badge/latestdoi/75893305)
[![build](https://github.com/workflowr/workflowr/workflows/build/badge.svg)](https://github.com/workflowr/workflowr/actions/workflows/build.yaml)
[![codecov](https://codecov.io/gh/workflowr/workflowr/branch/main/graphs/badge.svg?branch=main)](https://app.codecov.io/gh/workflowr/workflowr)

<img src="https://raw.githubusercontent.com/workflowr/workflowr-assets/main/img/hex-workflowr.png" alt="hex sticker for workflowr R package" align="right" width="100px">

The workflowr R package helps researchers organize their analyses in a way that
promotes effective project management, reproducibility, collaboration, and
sharing of results. Workflowr combines literate programming (knitr and
rmarkdown) and version control (Git, via git2r) to generate a website containing
time-stamped, versioned, and documented results. Any R user can quickly and
easily adopt workflowr.

For more details, see the [online documentation][workflowr-docs]. For an
example, see the [Divvy data exploration project][divvy]. To explore
reproducible research projects facilitated by workflowr, browse the projects
registered at [workflowr.io][]. To keep up-to-date with the latest workflowr
developments, please join the [workflowr-announce][] mailing list (low-volume,
read-only). For bugs reports, feature requests, and questions, please open an
[Issue][issues].

[workflowr.io]: https://workflowr.io/
[workflowr-announce]: https://groups.google.com/forum/#!forum/workflowr-announce

* [Features](#features)
* [Installation](#installation)
* [Quick start](#quick-start)
* [Attribution](#attribution)
* [Contributing](#contributing)

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
    * Documents how to host your website for free via [GitHub Pages][] or
    [GitLab Pages][]
    * Creates links to past versions of results

To see a workflowr website in action, see this [video demonstration][video].

For related tools, see [r-project-workflows][].

## Installation

1. Install [R][r]

    * (Recommended) Install [RStudio][rstudio]

    * (Optional) Install [pandoc][] ([Instructions][pandoc-install])

    * (Optional) Install [Git][git]

1. Install workflowr from [CRAN][cran]:

    ```r
    install.packages("workflowr")
    ```

1. Create an account on [GitHub][gh] or [GitLab][gl]

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
your results online. Alternatively, you could read the [Reproducible research
workshop vignette][vig-workshop]. It covers the same steps, but includes example
code and data to demonstrate some of workflowr's reproducibility features

1. Read the [customization vignette][vig-custom] for ideas on how to customize
your research website

1. Read the [migrating vignette][vig-migrating] for how to integrate workflowr
into your existing project

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

Workflowr is available under the [MIT license][MIT]. For proper attribution,
please cite our [manuscript][workflowr-paper] that describes the software:

> Blischak JD, Carbonetto P, and Stephens M. Creating and sharing reproducible
> research code the workflowr way [version 1; peer review: 3 approved].
> F1000Research 2019, 8:1749 (https://doi.org/10.12688/f1000research.20843.1)

To obtain a BibTeX entry, please run `citation("workflowr")`. Note that
F1000Research publishes not only the original version but also any revisions. To
check for the latest version, please go to the paper's [URL][workflowr-paper].

## Contributing

We welcome community contributions, especially improvements to documentation. To
get started, please read the [contributing guidelines](CONTRIBUTING.md).
Also, please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.

[contributors]: https://github.com/workflowr/workflowr/graphs/contributors
[cran]: https://cran.r-project.org/package=workflowr
[divvy]: https://stephenslab.github.io/wflow-divvy/
[gh]: https://github.com
[gl]: https://about.gitlab.com/
[git]: https://git-scm.com/
[git2r]: https://cran.r-project.org/package=git2r
[GitHub Pages]: https://pages.github.com/
[GitLab Pages]: https://about.gitlab.com/stages-devops-lifecycle/pages/
[issues]: https://github.com/workflowr/workflowr/issues
[knitr]: https://github.com/yihui/knitr
[lsun]: https://github.com/LSun
[MIT]: https://opensource.org/licenses/mit-license.php
[moore]: https://www.moore.org/
[nkweiwang]: https://github.com/NKweiwang
[pandoc]: https://pandoc.org/
[pandoc-install]: https://pandoc.org/installing.html
[pcarbo]: https://pcarbo.github.io/
[r]: https://cran.r-project.org
[rmarkdown]: https://rmarkdown.rstudio.com/
[r-project-workflows]: https://github.com/jdblischak/r-project-workflows#readme
[rstudio]: https://www.rstudio.com/products/rstudio/download/
[stephens]: https://stephenslab.uchicago.edu/
[uchicago]: https://www.uchicago.edu/
[video]: https://www.youtube.com/watch?v=O1wv94sZfvE
[vig-custom]: https://workflowr.github.io/workflowr/articles/wflow-02-customization.html
[vig-details]: https://workflowr.github.io/workflowr/articles/wflow-04-how-it-works.html
[vig-migrating]: https://workflowr.github.io/workflowr/articles/wflow-03-migrating.html
[vig-start]: https://workflowr.github.io/workflowr/articles/wflow-01-getting-started.html
[vig-workshop]: https://workflowr.github.io/workflowr/articles/wflow-09-workshop.html
[workflowr-docs]: https://workflowr.github.io/workflowr/
[workflowr-paper]: https://doi.org/10.12688/f1000research.20843.1
[xiangzhu]: https://github.com/xiangzhu
