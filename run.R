library(shiny)
library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(shinythemes)
library(googlesheets4)
library(googledrive)
library(shinycssloaders)
library(stringr)
library(purrr)
#library(gargle)

#source("./helpers.R")

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)

