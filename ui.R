ui <- fluidPage(
  
  titlePanel(paste0(Sys.getenv("ABCDE"))),
  
  sidebarLayout(
    sidebarPanel(
      # input for log ID
      textInput("log_id", "Enter Log ID:"),
      
      actionButton("submit_log_id",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      
      actionButton("submit_char_id","Estimate Ignite Metrics")
    ),
    mainPanel(
      # output for data table
      #  dataTableOutput("table"),
      dataTableOutput("table2"),
      htmlOutput("summary")
    )
  )
)