ui <- fluidPage(
  
  titlePanel(paste0(("Ignite Analyzer"))),
  
  sidebarLayout(
    sidebarPanel(
      "Instructions:",
      br(),
      "- Enter your log, press ENTER, select a mage and boss fight, press ESTIMATE",
      br(),
      "- If you do not interact with the app for an extended period, it may temporarily 'grey out.' If this occurs, please refresh the page. If the app crashes after pressing a button, please refresh the page and try again or contact me.",
      br(),
      "- The app currently supports only a limited number of bosses (Algalon, Thorim, Vezax and Razorscale)",
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
      
      tags$a(href= "https://github.com/ForgeGit?tab=repositories", "Github")
      
     
    ),
    mainPanel(
      # output for data table
      #  dataTableOutput("table"),
      htmlOutput("summary"),
      dataTableOutput("table2")
    )
  )
)