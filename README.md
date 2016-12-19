# workflowr - A workflow template for creating a research website

Some demos:

*  [workflowr-demo01][demo01] - A small project with a few pages
*  [workflowr-demo02][demo02] - A re-implementation of [fiveMinuteStats][]

[demo01]: https://github.com/jdblischak/workflowr-demo01
[demo02]: https://github.com/jdblischak/workflowr-demo02
[fiveMinuteStats]: http://stephens999.github.io/fiveMinuteStats/analysis/

To install:

```
# install.packages("devtools")
devtools::install_github("jdblischak/workflowr")
```

## Quick start

This quick start guide is intended for those who are actively using R, Git, and
GitHub. More extensive documentation forthcoming.

Start R and load workflowr:

```
> library("workflowr")
```

Start a new project:

```
> start_project("A new project", "new")
```

Open the project using the RStudio Project file. Build the site with the Build tab in RStudio or by running the following from the root of the directory:

```
> rmarkdown::render_site("analysis")
```

Create a new analysis file:

```
> rmarkdown::draft(file = "analysis/first.Rmd", template = "analysis",
                   package = "workflowr")
```

Press `Knit HTML` to render the file. To interactively edit the file, set the
working directory to the `analysis` directory. To render from the R console:

```
> setwd("analysis")
> rmarkdown::render_site("first.Rmd")
```

If you are using RStudio, the file should be displayed in the Viewer tab. The
website files are located in the directory `docs`. Now commit everything with
Git.

```
$ git add .
$ git commit -m "Build site."
```

On GitHub, create a new empty repo. GitHub provides the commands to run to "push
an existing repository from the command line", which you can conveniently 
copy-paste. The first line registers your remote repository with your local Git
repo using the conventional name "origin". The second pushes your Git repo to
GitHub.

```
$ git remote add origin <insert GitHub URL>
$ git push -u origin master
```

On GitHub, go to Settings, then scroll down to the section "GitHub Pages". For
the source, select "master branch /docs folder" and then click save. This tells
GitHub to serve the files in `docs`. If you scroll back down to the same 
section, it shows the URL of your new site. Check it out! And for convenience,
you can add this URL to the top of your GitHub repo. Go to the main page for the
repo and click Edit which appears next to the phrase "No description or website 
provided."

As you add new Rmd files, you'll want to adopt the following workflow:

1. Once you are satisfied with the analysis, add and commit the Rmd file.
1. Knit the Rmd file using the `Knit HTML` button or `rmarkdown::render_site`. This ensures that the unique Git commit at the top of the HTML page corresponds to the version of the R Markdown file that was used to create it.
1. Add and commit the website files (`git add docs`).
