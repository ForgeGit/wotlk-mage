server <- function(input, output,session) {
  actors <- NULL
  fights <- NULL
  
  # retrieve list of mages
  observeEvent(input$submit_log_id, {
    
    request <- sprintf(request_mage, as.character(input$log_id))
    request <- WCL_API2_request(request)
    actors <- request$data$reportData$report$masterData$actors 
    
    if(length(actors)!=0){
      
      actors <- actors %>% 
        filter(subType=="Mage") %>% 
        
        mutate(name = paste0(name, " (ID:",id,")"))
      
      
      updateSelectInput(session, "character", choices = actors$name)
      
      #        output$table <- renderDataTable({
      #         
      #       datatable(actors)
      #        
      #    })
      
      
      # retrieve list of fights
      request <- sprintf(request_fights, as.character(input$log_id))
      request <- WCL_API2_request(request)
      fights <- request$data$reportData$report$fights 
      fights <- fights %>% 
        filter(encounterID%in% boss_list) %>% 
        
        mutate(encounterID = as.character(encounterID),
               
               encounterID = case_when(encounterID == '757' ~ 'Algalon',
                                       encounterID == '752' ~ 'Thorim',
                                       encounterID == '755' ~ 'Vezax',
                                       encounterID == '746'  ~ 'Razosrcale',
                                       TRUE ~ encounterID),
               
               encounterID = ifelse(kill==0, 
                                    paste0(encounterID, " (Fight:",id,") - Wipe"), 
                                    paste0(encounterID, " (Fight:",id,") - Kill")
               )
        )
      
      updateSelectInput(session, "fight", choices = fights$encounterID)
      
      
    } else {   
      
      output$table <- renderDataTable({
        
        data.frame(A=c("ERROR"),B=c("ERROR"), C=c("Hi! Yes, This is incomplete"))
        
      }) 
      
      showModal(modalDialog(
        title = "AAAAAAAAAAAAAAA",
        "Hi! Yes, This is incomplete and the user experience is missing, for now.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
      
      updateSelectInput(session, "character", choices = NULL)
    }
    
    
    
  })
  
  # retrieve list of mages
  observeEvent(input$submit_char_id, {
    
    actor_temp <- parse_number(input$character)
    fight_temp <- parse_number(input$fight)
    
    
    request <- sprintf(request_damage, as.character(input$log_id), as.numeric(actor_temp), as.numeric(fight_temp))
    request <- WCL_API2_request(request)
    request <- request$data$reportData$report$events$data
    
    
    ignite_table <- request %>% 
      ignite_cleaning() %>% ungroup() %>%
      add_count(targetID) %>%
      filter(n==max(n)) %>%
      ignite_summary()
    
    
    output$table2 <- renderDataTable({
      
      ignite_table
      
      
    }) 
    
    output$summary <- renderText({
      paste0("Ignite lost to (target) death: ", max(ignite_table$Ignite_tick_lost_dead2), "\n",
             "Estimated Ignite Damage: ", max(ignite_table$Munch_NET_2))
    })
    
    
    output$summary <- renderUI({
      str1 <- paste0("Ignite lost to (target) death: ", max(ignite_table$Ignite_tick_lost_dead2))
      str2 <- paste0( "Estimated Ignite Damage difference: ", round(ignite_table$Munch_NET_2))
      HTML(paste(str1, str2, sep = '<br/>'))
      
    })
    
  })
}
