
my_packages = c("jsonlite", "httr","dplyr","tidyr",
                "readr","DT","shinythemes","googledrive",
                "googlesheets4","shinycssloaders","stringr","purrr","ggplot2","Cairo","collapse")#,"gargle")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))

