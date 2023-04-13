library(shiny)
library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(shinytheme)

source("./ui.R")
source("./server.R")


shinyApp(
  
  ui,
  server 
)

#shinyApp(ui = ui, server = server)

#shiny::runApp(display.mode="showcase")
