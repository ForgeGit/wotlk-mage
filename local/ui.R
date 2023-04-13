ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/ForgeGit/wotlk-mage/main/local/ourignite.ico")),
  
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
    )
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
      "1.- Enter your log, press ENTER",
      br(),
      "2.- Select a mage and boss fight, press ESTIMATE",
      br(),
      "3.- Repeat step 2 for each fight/mage",
      br(),
      "- If you do not interact with the app for an extended period, it may temporarily 'grey out.' If this occurs, please refresh the page. If the app crashes after pressing a button, please refresh the page and try again or contact me.",
      br(),
      "- The app currently supports only a limited number of bosses",
      br(),
      "(Algalon, Thorim, Vezax, Ignis, Kolo, Auriaya, Hodir, and Razorscale)",
      br(),
      br(),
      # input for log ID
      textInput("log_id", "Enter Log ID:"),
      
      actionButton("submit_log_id","ENTER log and load data",icon("refresh")),
      # input for character selection
      selectInput("character", "Select Character:", choices = NULL),
      # input for fight selection
      selectInput("fight", "Select Fight:", choices = NULL),
      
      actionButton("submit_char_id","ESTIMATE Metrics"),
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
      tags$a(href= "https://ko-fi.com/forge", "Buy me a covfefe! (and help me pay for the app hosting)"),
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