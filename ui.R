ui <- fluidPage(
  
  titlePanel(paste0(("Ignite Analyzer - BETA"))),
  
  sidebarLayout(
    sidebarPanel(
      "Instructions:",
      br(),
      "- Enter your log, press ENTER, select a mage and boss fight, press ESTIMATE",
      br(),
      "- If you do not interact with the app for an extended period, it may temporarily 'grey out.' If this occurs, please refresh the page. If the app crashes after pressing a button, please refresh the page and try again or contact me.",
      br(),
      "- The app currently supports only a limited number of bosses",
      br(),
      "(Algalon, Thorim, Vezax, Ignis, Kolo, Auriaya, Hodir, and Razorscale)",
      br(),
      
      # input for log ID
      textInput("log_id", "Enter Log ID:"),
      
      actionButton("submit_log_id","ENTER log and load data",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      
      actionButton("submit_char_id","ESTIMATE Ignite Metrics"),
      br(),
      actionButton("debug_id","debug table"),
      br(),
      br(), 
      span("Forge#0001 [Vivax-Pagle(US)]", style = "font-style:italic"),
      br(),
      tags$a(href= "https://discord.gg/eszwRckRmA", "Mage Discord"),
      br(),
      br(),
      "See my other work: ",
      br(),
      tags$a(href= "https://github.com/ForgeGit?tab=repositories", "Github"),
      br(),
      br(),
      "Consider a donation:",
      br(),
      tags$a(href= "https://ko-fi.com/forge", "Buy me a coffe! (and help me pay for the app hosting)"),
      br(),
      br(),
      tags$a(href= "https://github.com/ForgeGit/ignite_wotlk", "FAQ: Munching? Vomit? What?")
     
    ),
    mainPanel(
      # output for data table
      #  dataTableOutput("table"),
      htmlOutput("summary"),
      dataTableOutput("table2")
    )
  )
)