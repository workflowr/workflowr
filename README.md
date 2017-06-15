# workflowr

[![Travis-CI Build Status](https://travis-ci.org/jdblischak/workflowr.svg?branch=master)](https://travis-ci.org/jdblischak/workflowr)
[![codecov](https://codecov.io/gh/jdblischak/workflowr/branch/master/graph/badge.svg)](https://codecov.io/gh/jdblischak/workflowr)

* [Quick start](#quick-start)
* [Upgrading workflowr](#upgrading-workflowr)
* [License](#license)
* [Pronunciation](#pronunciation)

The goal of the [workflowr][] package is to make it easier for researchers to 
organize their projects and share their results with colleagues. If you are 
already writing R code to analyze data, and know the basics of Git and GitHub, 
you can start taking advantage of workflowr immediately. In a matter of minutes,
you can create a research website like [this][demo01].

**WARNING:** workflowr is still in the early stages of development, so there 
will potentially be major changes between versions. Thus make sure to upgrade if
necessary and look over the documentation. The good news is that any site you 
create will not be affected by these changes and will continue to function with 
new versions of the workflowr functions. If you find any problems, or would like
to suggest new features, please open an [Issue][issues].

## Why use workflowr?

First, hopefully you don't need much convincing to write your analyses in R 
Markdown. It allows you to combine your R code, text, and figures in the same 
document! See the [website][rmarkdown] to learn about all the cool features. 
Second, building a website with the rmarkdown package (as opposed to using knitr
to produce Markdown files and passing these to a static site generator) enables
you to use all the latest R packages (e.g. [htmlwidgets][]) directly in your
analyses. Third, the workflowr package provides functions to make it easier for
a researcher to maintain a version-controlled R Markdown website:

* A function to start a project with all the necessary files (see `?wflow_start`)
* Includes an R Markdown template that will automatically insert the date and most recent Git commit ID (i.e. SHA1) at the top of the file to aid reproducibility (see `?wflow_open`)
* Saves generated figures into an organized directory structure
* A function to only build new and updated files (analogous to a Makefile) so that long-running files do not have to be constantly re-run every time the site is built (or rely on caching, which can easily cause reproducibility headaches especially as a project grows) (see `?wflow_build`)
* A function to commit the website files to Git, ensuring that they are in sync with the latest committed version of the corresponding R Markdown files (see `?wflow_commit`)

## Quick start

* Install [R][r] ([instructions][swc-r] from [Software Carpentry][swc])

* (Optional) Install [RStudio][rstudio] (workflowr takes advantages of some
RStudio features, but RStudio is not required to use workflowr)

* Install [Git][git] ([instructions][swc-git] from [Software Carpentry][swc])

* Create an account on [GitHub][gh]

* Configure [Git][git] ([instructions][swc-git-config] from [Software
Carpentry][swc]). Run the following in the Terminal, inserting your information:

```bash
git config --global user.name "Your Name"
git config --global user.email "youremail@domain"
```

* Install the latest stable release of [workflowr][] from [GitHub][gh] using
[devtools][]:

```r
# install.packages("devtools")
devtools::install_github("jdblischak/workflowr", build_vignettes = TRUE)
```

* Work through the vignette [Getting started with workflowr][vig-start] to learn
the basic design and functionality of a workflowr project (you can view all the
available vignettes locally with `browseVignettes("workflowr")`)

* Start your project with `wflow_start()` (if you have already started your
project, please read the vignette [Migrating an existing project to use
workflowr][vig-migrating] for advice on how to proceed)

* Read through the vignette [Customize your research website][vig-custom]

* If you find any unexpected behavior or think of an additional feature that
would be nice to have, please open an Issue [here][issues]. When writing your
bug report or feature request, please note the version of workflowr you are
using (which you can obtain by running `packageVersion("workflowr")`).

## Upgrading workflowr

To upgrade workflowr to the most recent stable release, follow these steps:

* Install from [GitHub][gh] with [devtools][]:

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

## Credits

workflowr was developed, and is maintained, by John Blischak, a postdoctoral 
researcher in the laboratory of [Matthew Stephens][stephens] at [The University 
of Chicago][uchicago]. He is funded by a grant from the [Gordon and Betty Moore 
Foundation][moore] to MS.

The workflowr package uses many great open source packages. Most importantly it 
depends on the R packages [rmarkdown][], which is responsible for providing the 
infrastructure to convert a collection of R Markdown files into a static 
website, and [git2r][], which enables running Git commands from R. Furthermore 
it relies on the [GitHub Pages][] service for hosting the websites for free. See
the file DESCRIPTION for the full list of R packages used and the 
[vignette][vig-details] that describes the details of how workflowr works.

The package documentation is created with [pkgdown][], continuous integration 
testing is performed by [Travis CI][travis], and code coverage is calculated
with [covr][] and [Codecov][].

## License

workflowr is available under the [MIT][] license.

## Pronunciation

It is common for R packages to end with an "r", and I tend to pronounce this as 
if it was "er" because I personally find this the easiest. Thus I pronounce the 
package "workflow + er". Other equally as good options are "workflow + R" or
"work + flower".

[Codecov]: https://codecov.io/
[covr]: https://github.com/jimhester/covr
[demo01]: https://jdblischak.github.io/workflowr-demo01/
[devtools]: https://github.com/hadley/devtools
[gh]: https://github.com
[git]: https://git-scm.com/
[git2r]: https://cran.r-project.org/web/packages/git2r/index.html
[GitHub Pages]: https://pages.github.com/
[htmlwidgets]: http://www.htmlwidgets.org/
[issues]: https://github.com/jdblischak/workflowr/issues
[MIT]: https://opensource.org/licenses/mit-license.php
[moore]: https://www.moore.org/
[pkgdown]: http://hadley.github.io/pkgdown/
[r]: http://cran.r-project.org
[rmarkdown]: http://rmarkdown.rstudio.com/
[rstudio]: https://www.rstudio.com/products/rstudio/download/
[stephens]: http://stephenslab.uchicago.edu/
[swc]: https://software-carpentry.org
[swc-git]: https://swcarpentry.github.io/workshop-template/#git
[swc-git-config]: http://swcarpentry.github.io/git-novice/02-setup/
[swc-r]: https://swcarpentry.github.io/workshop-template/#r
[travis]: https://travis-ci.org/
[uchicago]: http://www.uchicago.edu/
[vig-custom]: https://jdblischak.github.io/workflowr/docs/articles/wflow-02-customization.html
[vig-details]: https://jdblischak.github.io/workflowr/docs/articles/wflow-04-how-it-works.html
[vig-migrating]: https://jdblischak.github.io/workflowr/docs/articles/wflow-03-migrating.html
[vig-start]: https://jdblischak.github.io/workflowr/docs/articles/wflow-01-getting-started.html
[workflowr]: https://jdblischak.github.io/workflowr/
