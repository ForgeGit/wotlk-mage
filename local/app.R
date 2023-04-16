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
#library(gargle)

source("./ui.R")
source("./server.R")


shinyApp(
  
  ui,
  server 
)

#shinyApp(ui = ui, server = server)

#shiny::runApp(display.mode="showcase")
#