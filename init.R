my_packages = c("shiny", "shinythemes", "RCurl", "dplyr", "jsonlite", "glue", "ggplot2", "purrr", "plotly", "GGally", "network")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))
