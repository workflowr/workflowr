# Infrastructure for workflowr projects.

# This file defines templates for use with various workflowr functions. They are
# inserted with `cat(glue::glue(x), file = fname)`, which removes the starting
# blank line and leaves a final blank line in the output file.

# wflow_start() ----------------------------------------------------------------

templates <- list(
  .gitattributes = '
# Classify R Markdown files as R code for GitHub language statistics
# https://github.com/github/linguist#overrides
*.[Rr]md linguist-language=R

',
  .gitignore = '
.Rproj.user
.Rhistory
.RData
.Ruserdata
.Rapp.history
.DS_Store
analysis/figure
analysis/*png
analysis/*html
analysis/*_cache
analysis/site_libs

',
  .Rprofile = '
## This makes sure that R loads the workflowr package
## automatically, everytime the project is loaded
if (requireNamespace("workflowr", quietly = TRUE)) {{
  message("Loading .Rprofile for the current workflowr project")
  library("workflowr")
}} else {{
  message("workflowr package not installed, please run install.packages(\\"workflowr\\") to use the workflowr functions")
}}

',
  `_workflowr.yml` = '
# workflowr options
# Version {wflow_version}

# The seed to use for random number generation. See ?set.seed for details.
seed: {the_seed_to_set}
# The working directory to build the R Markdown files. The path is relative to
# _workflowr.yml. See ?rmarkdown::render for details.
knit_root_dir: "."

',
    `analysis/_site.yml` = '
name: "{name}"
output_dir: ../docs
navbar:
  title: "{name}"
  left:
  - text: Home
    href: index.html
  - text: About
    href: about.html
  - text: License
    href: license.html
output:
  workflowr::wflow_html:
    toc: yes
    toc_float: yes
    theme: cosmo
    highlight: textmate

',
  `analysis/index.Rmd` = '
---
title: "Home"
site: workflowr::wflow_site
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

Welcome to my research website.

',
  `analysis/about.Rmd` = '
---
title: "About"
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

Describe your project.

',
  `analysis/license.Rmd` = '
---
title: "License"
output:
  workflowr::wflow_html:
    toc: false
editor_options:
  chunk_output_type: console
---

What license are you using for your code? See [choosealicense.com][choose] for
help deciding. It\'s a convention to save a file `LICENSE` in the root of your
Git repo that contains the license text.

What license are you using for the written content on your site? It is
traditional to choose a [Creative Commons][cc] license for this type of content.

[choose]: http://choosealicense.com/
[cc]: https://creativecommons.org/choose/

How should others cite your work? It\'s a convention to save a file `CITATION`
in the root of your Git repo that contains the citation information.

',
  `code/README.md` = '
# Code

Save command-line scripts and shared R code here.

',
  `data/README.md` = '
# Data

Save raw data files here.

',
    `output/README.md` = '
# Output

Save processed data files here.

',
  README.md = '
# {name}

A [workflowr][] project.

[workflowr]: https://github.com/jdblischak/workflowr

',
  "Rproj" = '
Version: 1.0

RestoreWorkspace: No
SaveWorkspace: No
AlwaysSaveHistory: Yes

EnableCodeIndexing: Yes
UseSpacesForTab: Yes
NumSpacesForTab: 2
Encoding: UTF-8

RnwWeave: Sweave
LaTeX: pdfLaTeX

AutoAppendNewline: Yes
StripTrailingWhitespace: Yes

'
)

# wflow_html() -----------------------------------------------------------------

# These templates are used by wflow_html() to insert HTML before and after the
# document body.

includes <- list(
  header = '
<!-- Add a small amount of space between sections. -->
<style type="text/css">
div.section {
  padding-top: 12px;
}
</style>

',
footer = '
<!-- Adjust MathJax settings so that all math formulae are shown using
TeX fonts only; see
http://docs.mathjax.org/en/latest/configuration.html.  This will make
the presentation more consistent at the cost of the webpage sometimes
taking slightly longer to load. Note that this only works because the
footer is added to webpages before the MathJax javascript. -->
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({
    "HTML-CSS": { availableFonts: ["TeX"] }
  });
</script>

'
)

extras <- list(
  disable_remote = '
#!/bin/bash

# This hook prevents pushing to a remote repository, which is ideal for a
# project containing confidential data. It was created by wflow_start() with the
# argument disable_remote = TRUE. If you decide you want to be able to push this
# repository, delete this file.

echo "This is a confidential project. Do not push the files to a remote server"
exit 1

'
)

# wflow_use_gitlab() -----------------------------------------------------------

gitlab <- list(`.gitlab-ci.yml` = '
pages:
  stage: deploy
  script:
    - echo \'Nothing to do...\'
  artifacts:
    paths:
      - public
  only:
    - master

')
