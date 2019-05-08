#!/usr/bin/env r

"Pandoc version"
rmarkdown::pandoc_version()

"Session information"
sessionInfo()

"Installed package versions"
installed.packages()[, c("Package", "Version")]
