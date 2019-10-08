file.create("system-info.Rout.save")
# System information
Sys.info()
.Platform
sessionInfo()
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
