# Analysis

Save your R Markdown analyses here. To create a new one, run

```
rmarkdown::draft("filename.Rmd", template = "analysis", package = "workflowr")
```

and save it in this directory. Alternatively in RStudio, you can use the
dropdown menu: `File` -> `New File` -> `R Markdown...` -> `From Template` ->
`Analysis Template`.

To render your file, run

```
rmarkdown::render_site("filename.Rmd")
```

This will create the HTML file in the website directory `docs`. The figures will
be created in both `analysis/figures` and `docs/figures`. Alternatively in
RStudio, you can click on the `Knit HTML` button. This has the added benefit of
displaying your file in the Viewer pane.
