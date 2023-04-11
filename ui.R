ui <- fluidPage(
  
  titlePanel(paste0(("Ignite Analyzer"))),
  
  sidebarLayout(
    sidebarPanel(
      "Instructions:",
      br(),
      "Enter log, press ENTER button, select mage and fight, press ESTIMATE button",
      br(),
      "Currently limited boss support (Algalon, Thorim, Vezax and Razorscale)",
      br(),
      
      # input for log ID
      textInput("log_id", "Enter Log ID:"),
      
      actionButton("submit_log_id","ENTER log and load data",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      
      actionButton("submit_char_id","ESTIMATE Ignite Metrics"),
      actionButton("debug_id","debug table"),
      br(),
      br(), 
      span("Forge#0001 [Vivax-Pagle(US)]", style = "font-style:italic"),
      br(),
      #    "Consider a donation to upkeep this service:"
      "See my other work: ",
      br(),
      "https://github.com/ForgeGit?tab=repositories"
    ),
    mainPanel(
      # output for data table
      #  dataTableOutput("table"),
      htmlOutput("summary"),
      dataTableOutput("table2")
    )
  )
)