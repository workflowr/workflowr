# workflowr

The goal of the workflowr package is to make it easier for researchers to 
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

[demo01]: https://jdblischak.github.io/workflowr-demo01/
[issues]: https://github.com/jdblischak/workflowr/issues

## Installation

workflowr is hosted in a [GitHub repository][repo]. To install or upgrade 
workflowr, run the following in R:

```r
# install.packages("devtools") 
devtools::install_github("jdblischak/workflowr", build_vignettes = TRUE)
```

[repo]: https://github.com/jdblischak/workflowr

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

[htmlwidgets]: http://www.htmlwidgets.org/

## Documentation

The following vignettes are available. You can open these locally from R after 
you've installed the package, or view the vignette online by clicking on the
title in the table below.

Vignette title | To open from R
-------------- | -------------
[Getting started with workflowr][vig-start] | `vignette("getting-started", "workflowr")` 
[Customize your research website][vig-custom] | `vignette("customization", "workflowr")` 
[How the workflowr package works][vig-details] | `vignette("how-it-works", "workflowr")`

[vig-start]: https://jdblischak.github.io/workflowr/inst/doc/getting-started.html 
[vig-custom]: https://jdblischak.github.io/workflowr/inst/doc/customization.html
[vig-details]: https://jdblischak.github.io/workflowr/inst/doc/how-it-works.html

## News

See the file [NEWS.md](./NEWS.md) to learn about recent updates.

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
[DESCRIPTION](./DESCRIPTION) for the full list of R packages used and the
[vignette][vig-details] that describes the details of how workflowr works.

[stephens]: http://stephenslab.uchicago.edu/
[uchicago]: http://www.uchicago.edu/
[moore]: https://www.moore.org/
[rmarkdown]: http://rmarkdown.rstudio.com/
[git2r]: https://cran.r-project.org/web/packages/git2r/index.html
[GitHub Pages]: https://pages.github.com/

## License

workflowr is available under the MIT license. See the files
[DESCRIPTION](./DESCRIPTION) and [LICENSE](./LICENSE) for more details.

## Pronunciation

It is common for R packages to end with an "r", and I tend to pronounce this as 
if it was "er" because I personally find this the easiest. Thus I pronounce the 
package "workflow + er". Other equally as good options are "workflow + R" or
"work + flower".
