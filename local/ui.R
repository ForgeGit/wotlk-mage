ui <- fluidPage(
  # tags$head(tags$link(rel="shortcut icon", href="https://cdn.discordapp.com/attachments/1091572489009758309/1096172475550810183/ourignite2.ico")),
  tags$head(tags$link(rel="shortcut icon", href="https://cdn.discordapp.com/attachments/244901735515029504/1104018878339756042/5361083.ico")),
  
  tags$style(HTML("
    h4, .h4 {
      margin-top: 0;
      margin-bottom: 0;
    }
  ")),
  
  
  tags$head(
    tags$script(
      type = "text/javascript",
      src = "https://www.googletagmanager.com/gtag/js?id=G-WPSQCXFWF5"
    ),
    tags$script(
      type = "text/javascript",
      "
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-WPSQCXFWF5');
      "
    ),
    tags$meta(property = "og:title", content = "Wrath Mage Analyzer - Log analysis tools for mages"),
    tags$meta(property = "og:description", content = "Analyze your mage logs for performance and gameplay on each boss."),
    tags$meta(property = "og:image", content = "https://cdn.discordapp.com/emojis/1037039849833767053.png")
  ),
  
  # https://bootswatch.com/slate/
  theme = shinytheme("slate"),
  tags$head(tags$style(HTML(
    "
                  .dataTables_length label,
                  .dataTables_filter label,
                  .dataTables_info {
                      color: white!important;
                      }

                  .paginate_button {
                      background: white!important;
                  }

                  thead {
                      color: white;
                  }
                  
                  a {
                      color: #ffcc00!important;
                  }    
                      
                  input[type=text] {
                      background-color: #f5f5f5;
                      color: #333333;
                  }
                  
      table.dataTable tbody tr:nth-child(even) {
        background-color: #f2f2f2;
      }
      
      table.dataTable tbody tr:nth-child(odd) {
        background-color: #f7f7f7;
            }
      
      table.dataTable thead th {
        background-color: white;
         color: black;
      }
                  
.btn-default.action-button.shiny-bound-input {
  background-color: #4169E1;
  border-color: #000000;
  color: #F5F5F5;
  transition: background-color 0.2s ease-in-out;
}

.btn-default.action-button.shiny-bound-input:active {
  background-color: #354C7E;
  border-color: #363636;
  color: #7a7376;
}



                  "))),
  tags$head(tags$script(HTML(
    '
    $(document).on("click", ".btn-default.action-button.shiny-bound-input", function() {
      var $btn = $(this);
      $btn.addClass("active");
      setTimeout(function() {
        $btn.removeClass("active");
      }, 100);
    });
    '
  ))),
  
  titlePanel(title=div(img(src="https://wow.zamimg.com/images/wow/icons/large/classicon_mage.jpg"), "WOTLK Mage Analyzer - BETA"),
             windowTitle="WOTLK Mage Analyzer"),
  
  sidebarLayout(
    sidebarPanel(
      "Instructions:",
      br(),
      "1.- Type in your log, then hit the READ button",
      br(),
      "2.- Select a mage and boss fight, press ESTIMATE",
      br(),
      "3.- Repeat step 2 for each fight/mage",
      br(),
      "Notes:",
      br(),
      "- If you do not interact with the app for an extended period, it will 'grey out.' If this occurs, please refresh the page. If the app crashes after pressing a button, please refresh the page and try again or contact me.",
      br(),
      "- The app currently supports only a limited number of bosses",
      br(),
      "(Algalon, Thorim, Vezax, Ignis, Kolo, Auriaya, Hodir, and Razorscale. Partially Freya. All of Naxx except 4H)",
      br(),
      tags$span(HTML("<span style='color:#D78613;'>If you happen to encounter any issues with an encounter or log, feel free to reach out to me on Discord.</span>")),
      br(),
      # input for log ID
      textInput("log_id", "Enter Log ID:",placeholder="e.g. gLOFenDCV103kNa2 or https://classic.warcraftlogs.com/reports/yJRZjtPBbxd9LC74"),
      
      actionButton("submit_log_id","READ log",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      actionButton("submit_char_id","ESTIMATE Fight Metrics"),
    #  br(),
     # actionButton("submit_demonic","Demonic Pact Metrics"),
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
      tags$a(href= "https://ko-fi.com/forge", "Buy me a covfefe! (and help me maintain the app and hosting)"),
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Metrics",
                           
                           
                           fluidRow(
                             column(width = 12, uiOutput("summary_header"))
                           ),
                           fluidRow(
                             column(width = 12, uiOutput("alert_header"))
                           ),
                           br(),
                           
                           fluidRow(
                             column(width = 5, withSpinner(uiOutput("summary_ignite_1"),color="red")),
                             column(width = 4, uiOutput("summary_ignite_2"))
                           ),
                           br(),
                           fluidRow(
                             column(width = 5, uiOutput("cast_metrics_1")),
                             column(width = 4, uiOutput("cast_metrics_2"))
                           ),
                           
                           fluidRow(
                             column(width = 5, uiOutput("cast_delays_1")),
                             column(width = 4, uiOutput("cast_delays_2"))
                           ),
                           fluidRow(
                             column(width = 7, uiOutput("extra_algalon"))
                           ),
                           fluidRow(
                             column(width = 12,uiOutput("caption_1"))
                           ),
                           fluidRow(
                             column(width = 12, dataTableOutput("table2"))
                           ),
                           fluidRow(
                             column(width = 12, uiOutput("extra_else"))
                           )
                  ),
                  tabPanel("Changelog",
                           uiOutput("Changelog")
                  ),
                  tabPanel("F.A.Q",
                           fluidRow(
                             column(width = 10, uiOutput("FAQ"))
                           )
                  ),
                  tabPanel("Demonic Pact (BETA)",
                           fluidRow(
                             column(width = 9, uiOutput("DP_info"))
                             ),
                           fluidRow(
                             column(width = 9, withSpinner(plotOutput("plot_DP"),color="red"))
                           )
                  ),
                  tabPanel("Scoreboard",
                           fluidRow(      br(),
                                          
                                          h4("Last update: ", Sys.Date()),
                                          br(),
                                          column(width = 5, withSpinner(DTOutput("table_A_UI"),color="red")
                                          )) ,
                           fluidRow(      br(),
                                          
                                         # h4("Highest Pyro to Hotstreak ratio"),
                                          
                                          column(width = 5, DTOutput("table_B_UI"))
                                          
                           ),
                           fluidRow(      br(),
                                          
                                         # h4("Biggest munch recorded"),
                                          
                                          column(width = 5, DTOutput("table_C_UI"))
                                          
                           ),
                           fluidRow(      br(),
                                          
                                         # h4("Biggest vomit recorded"),
                                          
                                          column(width = 5, DTOutput("table_D_UI"))
                                          
                           ),
                           fluidRow(      br(),
                                          
                                        #  h4("Biggest ignite tick recorded"),
                                          
                                          column(width = 5, DTOutput("table_E_UI"))
                                          
                           )
                  )
      )
    )
  )
)
  