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
library(ggplot2)
library(Cairo)
library(pryr)
library(collapse)#test
#library(gargle)

source("./ui.R")
source("./server.R")
source("./LEADERBOARD_build.R")
1
shinyApp(
  
  ui,
  server
)

#shinyApp(ui = ui, server = server)

#shiny::runApp()
#
#
#
#
library(profvis)

profvis({ runApp('./') }  
        , prof_output = './profiling')

profvis(prof_input = './profiling') 
