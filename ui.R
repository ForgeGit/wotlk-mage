ui <- fluidPage(
  
  titlePanel(paste0(("Ignite Analyzer"))),
  
  sidebarLayout(
    sidebarPanel(
      # input for log ID
      textInput("log_id", "Enter Log ID:"),
      
      actionButton("submit_log_id","Enter log and load data",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      
      actionButton("submit_char_id","Estimate Ignite Metrics"),
      actionButton("debug_id","DEBUG TABLE"),
      br(),
      br(),
      "_- Forge#0001 [Vivax-Pagle(US)]_",
  #    "Consider a donation to upkeep this service:"
      "See my other work: https://github.com/ForgeGit?tab=repositories"
    ),
    mainPanel(
      # output for data table
      #  dataTableOutput("table"),
      htmlOutput("summary"),
      dataTableOutput("table2")
    )
  )
)