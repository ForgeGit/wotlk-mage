library(shiny)
library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)

source("./helpers.R")

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)

