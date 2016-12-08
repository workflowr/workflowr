# workflowr - A workflow template for creating a research website

To install:

```
# install.packages("devtools")
devtools::install_github("jdblischak/workflowr")
```

## Quick start

Start R and load workflowr and rmarkdown:

```
> library("workflowr")
> library("rmarkdown")
```

Create a new project:

```
> create_site("A new project", "new")
```

Open the project using the RStudio Project file. Build the site with the Build tab in RStudio or by running the following from the root of the directory:

```
> render_site("analysis")
```

Create a new analysis file:

```
> draft(file = "analysis/first.Rmd", template = "analysis", package = "workflowr")
```

Press `Knit HTML` to render the file. To interactively edit the file, set the
working directory to the `analysis` directory. To render from the R console:

```
> render_site("first.Rmd")
```
