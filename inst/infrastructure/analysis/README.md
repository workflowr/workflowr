# Analysis

Save your R Markdown analyses here. To create a new one, run

```
wflow_open("filename.Rmd")
```

Alternatively in RStudio, you can use the dropdown menu: `File` -> `New File` ->
`R Markdown...` -> `From Template` -> `Analysis Template`.

To build your file, run

```
wflow_build()
```

This will create the HTML file in the website directory `docs/`. The figures
will be created in both `analysis/figures` and `docs/figures`. Alternatively in 
RStudio, you can click on the `Knit HTML` button. This has the added benefit of 
displaying your file in the Viewer pane.

When you are ready to add the analysis to your research website, publish the
analysis, which commit the source R Markdown file, the corresponding HTML file,
and any figures to the Git repository.

```
wflow_publish("filename.Rmd")
```

Run `vignette("wflow-01-getting-started")` for more details.
