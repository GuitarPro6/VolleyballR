required_packages = c(
  "shiny",
  "ggplot2",
  "hexbin",
  "dplyr",
  "httr",
  "jsonlite",
  "readr"
)

packages_to_install = required_packages[!(required_packages %in% installed.packages()[, 1])]

if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, repos = "https://cran.rstudio.com")
}
remove.packages('readr', c("/Users/Jon/Library/R/3.3/library", "/Library/Frameworks/R.framework/Versions/3.3/Resources/library"))
.libPaths()
