if (identical(Sys.getenv("NOT_CRAN"), "true")) file.create("system-info.Rout.save")

# System information
Sys.info()
.Platform
sessionInfo()
# Dates and times
Sys.Date()
Sys.time()
Sys.timezone()
# Paths
getwd()
tempdir()
.libPaths()
path.expand("~")
normalizePath("/")
# Pandoc
Sys.which("pandoc")
rmarkdown::pandoc_available()
rmarkdown::pandoc_version()
# Git
Sys.which("git")
if (file.exists(Sys.which("git"))) system("git --version")
