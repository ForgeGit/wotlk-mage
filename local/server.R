
#########################################################################################
options(scipen = 100)

boss_list <- c(757, #Alga
               752, #Thorim
               755, #Vezax
               746,  #Razorscale
               750, #Auriaya
               749, #Kologarn
               745, #Ignis
               751, #Hodir
               753
) 

npc_exclusions <- c("Hodir's Fury",
                    "Champion of Hodir",
                    "Razorscale Spawner",
                    "Razorscale Harpoon Fire State",
                    "Razorscale Controller",
                    "Algalon Stalker Asteroid Target 02",
                    "Rubble Stalker Kologarn",
                    "Auriaya Feral Defender Stalker",
                    "Thorim's Hammer",
                    "Thorim Event Bunny",
                    "Freya's Ward")
#########################################################################################
url <- "https://classic.warcraftlogs.com/api/v2"

WCL_API2_request <- function(request) {
  
  request <- jsonlite::toJSON(list(query=request),
                              auto_unbox=TRUE, 
                              bigint_as_char=F) 
  
  response <- POST(url,
                   add_headers("Content-Type" = "application/json",
                               "Authorization"= paste0("Bearer ",
                                                       Sys.getenv("TOKEN"))),
                   body = request,
                   content_type_json(),
                   encode = "json")
  
  response_formated <- fromJSON(content(response, 
                                        as = "text",
                                        encoding="UTF-8"), 
                                bigint_as_char=TRUE)
  
  return(response_formated)
}

######### Errors ##########

error1 <-"It looks like the log you just linked does not exist, has no mages, or there is something wrong with the app. Contact Forge#0001 on discord or try refreshing"
error2 <-"It looks like the log you just linked does not have valid fights for analysis, or there is something wrong with the app. Contact Forge#0001 on discord or try refreshing."
error3 <- "It looks like that character has no data for that fight. If you think this is an error, contact Forge#0001 on discord or try refreshing"
error_diag <-function(x,y) {
  
  x <- modalDialog(
    title = paste0("Error #",y),
    x,
    easyClose = TRUE,
    footer = tagList(
      modalButton("OK")
    )
  )
  
  return(x)
  
}



######### Requests ##########

request_actors <-'{
    reportData {
        report(code: "%s") {
            masterData(translate: true) {
                actors{
          
                gameID
                id
                name
                server
                subType
                
                }
        }
    }
}}'


request_fights <-'{
    reportData {
        report(code: "%s") {
            fights(killType: All){
            encounterID
            difficulty
            hardModeLevel
            averageItemLevel
            size
            kill
            lastPhase
            startTime
            endTime
            fightPercentage
            bossPercentage
            completeRaid
            id
            }
        }
        }
    }'


request_damage <-'{
    reportData {
        report(code: "%s") {
            events(dataType:DamageDone
            hostilityType:Friendlies
            fightIDs:%i
            sourceID:%i
            targetID:%i
            startTime: 0
            endTime: 999999999999){
              data
              nextPageTimestamp
              }
            }
        }
        }'

request_spec <-'{
    reportData {
        report(code: "%s") {
            events(
                dataType: CombatantInfo
                startTime: 0
                endTime: 999999999999
                fightIDs: %i
                sourceID: %i
                hostilityType: Friendlies
                includeResources: true
                
            ) {
                data
                nextPageTimestamp
            }
        }
    }
}'


request_debuff<-'{
    reportData {
        report(code: "%s") {
            events(
                dataType: Debuffs
                startTime: 0
                endTime: 999999999999
                fightIDs: %i
                sourceID: %i
                targetID: %i
                hostilityType: Enemies
                includeResources: true
                
            ) {
                data
                nextPageTimestamp
            }
        }
    }
}'

request_cast<-'{
    reportData {
        report(code: "%s") {
            events(
                dataType: All
                startTime: 0
                endTime: 999999999999
                fightIDs: %i
                sourceID: %i
                hostilityType: Friendlies
                includeResources:true
                
            ) {
                data
                nextPageTimestamp
            }
        }
    }
}'

spell_filter <- c(1,60488,5019,# necromatic power and misc shoot 
                  55039,# Gnomish Lightning Generator
                  49909, #icy touch
                  49921, # plague strike
                  55021,#impcs
                  59638, 59637, 42917, # MI spells and frost nova
                  42842, 42841, 38697, 27072, 27071, 25304, 10181, 10180, 10179, 8408, 8407, 8406, 7322, 837, 205, 116,#frostbolt
                  72898, 31707,# water ele 
                  44572, 71757,# deep freeze
                  122,865,6131,10230,27088, 42917, #frost nova
                  30455 , 42913  ,42914 ,# All Ice Lances
                  44425,44780, 44781 ,# All abarrs
                  42844 , 42845 ,42844, 38700, 27076, 25346, 10274, 10273, 8418, 8419,7270, 7269,7268,  # Most missiles
                  34913,43043,43044 ,#molten armor
                  32747, #interrupt
                  12355, # impact
                  28730 ,# arcane torrent
                  54735, #electropulse
                  42931, 42930, 10160, 27087, 10159, 8492, 120,10161, #CoC
                  54043, #retri aura
                  42897, 42896 , 30451 , 42894, #Arcane Blasts (all) 
                  42937,42198, 42211, 42210, 42213, 42209, 42212,42208, # blizzards
                  28715 #Flame Cap
)


#################################Cleaning########################################################


ignite_cleaning <- function(x) {
  
  x <- x %>%
    
    arrange(timestamp,fight,sourceID,targetID)  %>%
    
    # filter(sourceID == as.numeric(actor_temp)) %>% 
    
    select(timestamp,fight, sourceID,targetID,
           abilityGameID, 
           amount,unmitigatedAmount, hitType) %>%
    
    filter( (hitType!=1 &  hitType!=16) | abilityGameID == 12654) %>%
    
    filter( !(abilityGameID %in% spell_filter) & 
              abilityGameID != 42845 & abilityGameID != 38703 & #AM x2
              abilityGameID !=42921 &   abilityGameID !=42920 &   abilityGameID !=27082 &   abilityGameID !=27080  & #  AE
              abilityGameID !=2139 &  #counterspell
              abilityGameID !=60483 &  #Pendulum of Telluric Currents
              abilityGameID != 56488 &   # Global Thermal Sapper Charge
              abilityGameID !=60203 &  # Darkmoon Card: Death
              abilityGameID !=42938 & abilityGameID != 12494 &#  blizz frostbite
              abilityGameID != 56350  #Saronite bomb
    ) %>%
    mutate(
      abilityGameID = as.character(abilityGameID),
      abilityGameID = case_when(abilityGameID == '55360' ~ 'Living Bomb',
                                abilityGameID == '44457' ~ 'Living Bomb R1',
                                abilityGameID == '44461' ~ 'Living Bomb R1 tick',
                                abilityGameID == '55361' ~ 'Living Bomb R2',
                                abilityGameID == '12654' ~ 'Ignite',
                                abilityGameID == '42833' ~ 'Fireball',
                                abilityGameID == '42832' ~ 'Fireball R15',
                                abilityGameID == '38692' ~ 'Fireball R14',
                                abilityGameID == '42890' ~ 'Pyroblast R11',
                                abilityGameID == '33938' ~ 'Pyroblast R10',
                                abilityGameID == '12522' ~ 'Pyroblast R3',
                                abilityGameID == '42891' ~ 'Pyroblast',
                                abilityGameID=="55362"~"Living Bomb (explosion)",
                                abilityGameID=="55359"~"Living Bomb R2? (explosion)",
                                
                                abilityGameID=="42859"~"Scorch",
                                abilityGameID=="42858"~"Scorch R10",
                                abilityGameID=="27074"~"Scorch R9",
                                abilityGameID=="42873"~"Fire Blast",
                                abilityGameID=="42872"~"Fire Blast R10",
                                abilityGameID=="27079"~"Fire Blast R9",
                                abilityGameID=="42926"~"Flamestrike",
                                abilityGameID=="42925"~"Flamestrike R8",
                                abilityGameID=="27086"~"Flamestrike R7",
                                abilityGameID=="47610"~"Frostfire Bolt",
                                abilityGameID=="44614"~"Frostfire Bolt R1",
                                abilityGameID=="42950"~"Dragon's Breath",
                                abilityGameID=="42949"~"Dragon's Breath R5",
                                abilityGameID=="42944"~"Blast Wave R8",
                                abilityGameID=="42945"~"Blast Wave",
                                
                                TRUE ~ abilityGameID)
    ) %>%  
    group_by(fight,sourceID,targetID) %>% 
    
    ## Ignite chunks https://github.com/ForgeGit/ignite_wotlk#part-2---ignite-chunks
    mutate(IGNITE_END =  ifelse(lag(abilityGameID)=="Ignite" &  
                                   lag(abilityGameID,2)=="Ignite",
                                 "START",
                                 "NA"),
           IGNITE_END = cumsum(replace_na(IGNITE_END,"NA")=="START")) %>%
    
    group_by(fight,
             sourceID,targetID,IGNITE_END) %>% 
    
    
    ## Ignite values for each chunk
    mutate(igniteADD = ifelse(abilityGameID!="Ignite", 
                              amount*0.4,0), # Rounding
           igniteSUB = ifelse(abilityGameID=="Ignite", 
                              unmitigatedAmount,0), 
           igniteSUB_resist = ifelse(abilityGameID=="Ignite", 
                                     amount,0),
           
           igniteSUB= ifelse(is.na(igniteSUB),0,igniteSUB), 
           
           igniteSUB_resist = ifelse(is.na(igniteSUB_resist),0,igniteSUB_resist),
           
           igniteCUM =  cumsum(igniteADD),
           
           igniteDIM = cumsum(igniteSUB)
           
    ) %>%
    ungroup()%>%
    
    group_by(fight,
             sourceID,targetID) %>% 
    
    ## Is it the right ignite amount?
    
    dplyr::mutate(
      igniteREM = igniteCUM-igniteDIM,
      
      trueIgnite = as.character(
        ifelse(abilityGameID=="Ignite" &  
                 (as.integer(igniteSUB) ==  as.integer(round(lag(igniteREM)) / 2) |
                    as.integer(igniteSUB) + 1 ==  as.integer(round(lag(igniteREM)) / 2) |
                    as.integer(igniteSUB) - 1 ==  as.integer(round(lag(igniteREM)) / 2)|
                    as.integer(igniteSUB) + 2 ==  as.integer(round(lag(igniteREM)) / 2) |
                    as.integer(igniteSUB) - 2 ==  as.integer(round(lag(igniteREM)) / 2)) &  
                 lag(abilityGameID)!="Ignite",
               "YES", 
               ifelse(abilityGameID=="Ignite" &   
                        (as.integer(igniteSUB) !=  as.integer(round(lag(igniteREM)) / 2) |
                           as.integer(igniteSUB) + 1 !=  as.integer(round(lag(igniteREM)) / 2) |
                           as.integer(igniteSUB) - 1 !=  as.integer(round(lag(igniteREM)) / 2)|
                           as.integer(igniteSUB) + 2 !=  as.integer(round(lag(igniteREM)) / 2) |
                           as.integer(igniteSUB) - 2 !=  as.integer(round(lag(igniteREM)) / 2)) &  
                        lag(abilityGameID)!="Ignite",
                      "NO",
                      ifelse(abilityGameID=="Ignite" &  
                               lag(abilityGameID)=="Ignite",
                             "END",NA)))
      ),
      
      ## How much it was munched
      munched = as.integer(ceiling(
        ifelse(trueIgnite=="NO" & ((lag(igniteREM)/2)-igniteSUB)>0, 
               (lag(igniteREM)/2)-igniteSUB,
               0)) - 0.5 ), #forced ceiling to avoid "to even"
      
      ## How much ignite was it in practice
      igniteREAL = igniteREM - munched,
      
      ## Total munched so far
      munchedCUM = cumsum(replace_na(munched,0))
    )%>%
    
    group_by(fight,
             sourceID,targetID,IGNITE_END) %>%
    
    mutate(
      
      munched_temp = cumsum(replace_na(munched,0)),
      timestamp_2 = timestamp-lag(timestamp)
      
    ) %>%
    
    ungroup()%>%
    
    group_by(fight,
             sourceID,targetID) %>%
    mutate(
      igniteREM_IRL =  ifelse(abilityGameID=="Ignite",
                              igniteSUB,
                              ifelse(lag(replace_na(trueIgnite,"NA"))=="END",
                                     0,
                                     igniteREM - munched_temp
                              ))
    )                               
  
  
  return(x)
  
}


#################################Summary########################################################


ignite_summary <- function(x) {
  
  x <- x %>%
    summarise(#Total_Ignites_with_Munch = sum(trueIgnite,na.rm=T),
      Munch_dmg_lost = max(munchedCUM),
      
      Total_Ignite_Dmg_Potential = sum(igniteADD),
      
      Total_Ignite_Dmg_Dealt= sum(igniteSUB),
      
      Total_Ignite_Dmg_Dealt_resist = sum(igniteSUB_resist),
      
      #Ignite_tick_lost_dead1 = last(igniteREM_IRL), #Does not consider 1st tick calc as valid in the new set of ignite, see example 
      Ignite_tick_lost_dead2 = last(igniteREM),
      
      # Basically formula here: https://github.com/ForgeGit/ignite_wotlk#part-3---ignite-munching-and-vomit-basic-interactions
      
      # Munch_NET_1 = Total_Ignite_Dmg_Potential - (Total_Ignite_Dmg_Dealt+Ignite_tick_lost_dead1),
      
      Munch_NET_2 = Total_Ignite_Dmg_Potential - (Total_Ignite_Dmg_Dealt+Ignite_tick_lost_dead2)
    ) 
  
  return(x)
  
}

#########################################################################################

extract_log_id <- function(log_input) {
  # Regular expression pattern to match the log ID
  pattern <- "(?<=\\/|\\#|^)([A-Za-z0-9]+)(?=\\/?\\#|\\?|\\/$|$)"
  
  # Extract the ID using the pattern
  match <- regexpr(pattern, log_input, perl = TRUE)
  if (match == -1) {
    # The pattern did not match anything
    return(NA)
  } else {
    # Return the matched substring
    return(substr(log_input, match, match + attr(match, "match.length") - 1))
  }
}



############################### SERVER ############################### 

server <- function(input, output,session) {
  
  output$summary_ignite_1 <- renderUI({ HTML(paste(paste0("")))})
  
  #### CHANGELOG####
  output$Changelog <- renderUI({
    
    HTML(paste(paste0("<h5><b>Changelog (dd/mm/yyyy):</b></h5>"),
               paste0("- 18/04/2023: Missing enchant notification for most slots added."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Resource metrics added (HP/Mana/SP)."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;App supports 'no spec' on logs as long as you had 1 ignite tick on target."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href='https://ko-fi.com/home/coffeeshop?ReturnUrl=/&txid=e01ae145-0a7a-4d49-8c3a-457b95d3de3e'>Added support for 'Frostnite' spec (Frost spec w/ ignite). Thanks to a donation.</a>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Fixed very rare bug were a very specific selection of logs would not work."),
               paste0("- 17/04/2023: Change log released."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Tabs included."),
               paste0("- 16/04/2023: Notifications for unusual values. (See FAQ)"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Algalon note: Black hole causes 'munching'."),
               paste0("- 15/04/2023: <a href='https://www.youtube.com/watch?v=baC6QJW1H4s&t=259s'>Crateria (Human Bald Mage) video on how to use the Tool + Ignite Munching news!</a>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pyroblast Metrics added."),
               paste0("- 14/04/2023: Dark Mode enabled as default."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pyroblast hard-cast detection."),
               paste0("- 13/04/2023: Freya support"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href='https://ko-fi.com/home/coffeeshop?ReturnUrl=/&txid=656b9483-24e7-49be-ad38-9465c1b42699'>Dr. Boom logs supported by the app. Thanks to a donation.</a>"),
               paste0("- 12/04/2023: Fireball and Pyroblast delay estimations."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Living Bomb clipping detection."),
               paste0("- 11/04/2023: First release."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Munchalyzer."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href='https://ko-fi.com/home/coffeeshop?ReturnUrl=/&txid=da1b79d1-84cb-4df2-ac38-2fe2d5068acf'>Thank you Villms for your donation and for making this possible!</a>"),
               sep = '<br/>'))
    
    
})
  #### FAQ ####
  output$FAQ <- renderUI({
    
    HTML(paste(paste0("<h5><b>Frequently Asked Questions<sup>Is the app dumb? Or is it me?</sup>:</b></h5>"),
               
               paste0("<b>- Color scheme:</b>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<font color=\"#54A5BE\"> Information text. Not good. Not bad. Neutral.</font>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<font color=\"#D78613\"> There is something weird. It is probably ok, but it could be worth looking into.<sup>*</sup></font>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<font color=\"#BE5350\"> This needs your attention. Very likely, you are doing something wrong.<sup>*</sup></font>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<font color=\"#61B661\"> Is it luck? Is it skill? I can't tell, but you did something right.</font>"),
               '<br/>',
               paste0("<b>- What is Vomit/Munch?:</b>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; The most simple answer is that both are bugs associated with Ignite, they happen under certain conditions, and as a TTW mage you can mitigate the negatives effects of munching."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; <a href='https://github.com/ForgeGit/ignite_wotlk'> If you want further details you can click here.</a>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; As of 10/04/2023 (dd/mm/yyyy) <a href='https://us.forums.blizzard.com/en/wow/t/wotlk-stealth-change-to-feral/1566375/5'>Blizzard announced they would adjust ignite to 'fix' munching.</a>"),
               '<br/>',
               paste0("<b>- How do I fix munching?:</b>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; You can't fix it, but you can use some workarounds with the use of Weakauras and macros to minimize its effects."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; Consider asking at the <a href='https://github.com/ForgeGit/ignite_wotlk'>Mage Discord.</a> for a proper explanation on the available options. "),
               '<br/>',
               
               paste0("<b>- Can I use this to test at Dr. Boom/Training Dummies?:</b>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; Dr. Boom testing is enabled. If Dr. Boom is present in your log, it should allow you to select a timestamp as a fight."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; I do not have plans at the moment to include Dummies due to their buggy nature and weird inconsistent results in the past."),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; <i>Note: Weakauras, addons, or some spell interactions could work differently in instances vs open-world. Consider this when testing @ Dr. Boom</i>"),
               '<br/>',
               
               paste0("<b>- What about Frost/Arcane Mages?:</b>"),
               paste0("&nbsp;&nbsp;&nbsp;&nbsp; This app began as an ignite measuring tool. I plan to support all mage specs in the near future."),
               '<br/>',
               paste0("*<i>Do you think this is a mistake? Contact me on Discord at Forge#0001</i>."),
               sep = '<br/>'))
    
    
  })
  
  
  #### Style settings #####
  
  tags$style(HTML("
    h4, .h4 {
      margin-top: 0;
      margin-bottom: 0;
    }
  "))
  
  #### STEP 1: Submit log ID (Prep) ####
  
  ##### + Actors list ######
  
  actors <-eventReactive(input$submit_log_id, {
    
    ###### Actor Download ######
    actors<-WCL_API2_request(
      sprintf(
        request_actors, # Request 
        as.character(extract_log_id(as.character(input$log_id))) # log ID
      )
    )$data$reportData$report$masterData$actors
    
    if(!is.null(actors)){
      
      actors %>% 
        filter(subType %in% c("NPC","Boss","Mage","Unknown")) # Exclude unnecesary classes/pets
      
    }else{
      
      "NO DATA"
      
    }
    
  })
  
  ##### DR Boom main trigger ####
  
  
  doctor_pressence <-eventReactive(input$submit_log_id, { #IS DR BOOM PRESENT?
  
  if(
    any(actors()$name %in% c("Dr. Boom")) ==T){"TRUE"} else{"FALSE"}

    
  })
  
  
  ##### + Fights list ######
  
  fights <-eventReactive(input$submit_log_id, {
    
    fights<-WCL_API2_request(
      sprintf(
        request_fights, # Request 
        as.character(extract_log_id(as.character(input$log_id))) # log ID
      )
    )$data$reportData$report$fights
    
    if(!is.null(fights)){
      
      ##### Dr Boom Identification #####
      if(doctor_pressence()=="TRUE"){
        fights
      } else if(max(fights$encounterID)>0) {
        
        fights %>% 
          filter(encounterID!=0) %>% 
          
          filter(encounterID %in% boss_list) %>% 
          
          mutate(encounterID_2 = as.character(encounterID),
                 
                 encounterID_2 = case_when(encounterID_2 == '757' ~ 'Algalon',
                                           encounterID_2 == '752' ~ 'Thorim',
                                           encounterID_2 == '755' ~ 'Vezax',
                                           encounterID_2 == '746'  ~ 'Razorscale',
                                           encounterID_2 == '750'  ~ 'Auriaya',
                                           encounterID_2 == '749'  ~ 'Kologarn',
                                           encounterID_2 == '745'  ~ 'Ignis',
                                           encounterID_2 == '751'  ~ 'Hodir',
                                           encounterID_2 == '753'  ~ 'Freya',
                                           
                                           TRUE ~ encounterID_2),
                 
                 encounterID_2 = ifelse(kill==0, 
                                        paste0(encounterID_2, " (Fight:",id," - Wipe)"), 
                                        paste0(encounterID_2, " (Fight:",id," - Kill)")
                 )
          )
        
      }else{
        "NO DATA"
      }
      
    }else{
      "NO DATA"
    }
    
  })
  
  ### STEP 1.5: Submit log ID (display) ####
  
  observeEvent(input$submit_log_id, {
    
    #####+ Actor display ####
    
    if(is.data.frame(actors())==TRUE){
      
      mages <- actors() %>% 
        filter(subType=="Mage") %>% 
        mutate(name = paste0(name, " (ID:",id,")"))
      
      if(nrow(mages)>0){
        
        updateSelectInput(session, "character", choices = mages$name)
        
        ###### Dr. Boom logic ####
        
      } else if(doctor_pressence()=="TRUE") {
        
        updateSelectInput(session, "character", choices = actors()$name)
        
      } else {
        showModal(error_diag(error1,1))
        updateSelectInput(session, "character", choices = "NO MAGES")
      }
      
    } else if(is.data.frame(actors())==FALSE) {
      ###### Modal Error 1 ####
      showModal(error_diag(error1,1))
      updateSelectInput(session, "character", choices = actors())
    }
    
    
    ##### + Fights display #####
    
    if(is.data.frame(fights())==TRUE){
      
      if(max(fights()$encounterID)>0){
        
        updateSelectInput(session, "fight", choices = fights()$encounterID_2)
        
        ###### Dr. Boom logic ####
        
      } else if(doctor_pressence()=="TRUE"){  
        
        updateSelectInput(session, "fight", choices = fights()$startTime)
        
      } else if(actors()!="NO DATA"){
        
        showModal(error_diag(error2,2))
        updateSelectInput(session, "fight", choices = "NO BOSS FIGHTS")
        
      }else {
        
        updateSelectInput(session, "fight", choices = "NO BOSS FIGHTS")
      }
      
    } else if(is.data.frame(fights())==FALSE & actors()!="NO DATA") {
      ###### Modal Error 2 ######
      showModal(error_diag(error2,2))
      updateSelectInput(session, "fight", choices = fights())
    }
    
  })
  
  ### STEP 2: Retrieve data and estimations ####
  
  observeEvent(input$submit_char_id, {
    
    output$summary_ignite_1 <- renderUI({ HTML(paste(paste0("")))})
    output$summary_ignite_2 <- renderUI({ HTML(paste(paste0("")))})
    output$cast_delays_1 <- renderUI({ HTML(paste(paste0("")))})
    output$cast_delays_2 <- renderUI({ HTML(paste(paste0("")))})
    output$cast_metrics_1 <- renderUI({ HTML(paste(paste0("")))})
    output$cast_metrics_2 <- renderUI({ HTML(paste(paste0("")))})
    output$extra_algalon <- renderUI({ HTML(paste(paste0("")))})
    output$summary_header <- renderUI({ HTML(paste(paste0("")))})
    output$alert_header <- renderUI({ HTML(paste(paste0("")))})
    
    ##### + Parse fight ID ####
     
    fight_name <- input$fight
    
    if(doctor_pressence()=="TRUE"){
      
      fight_temp<- fights() %>% 
        filter(startTime==as.numeric(fight_name)) %>% 
        select(id)
      
      fight_temp <- fight_temp$id[1]
      fight_name="Dr. Boom"
    }else {
      fight_temp <- parse_number(fight_name)
    }
    
    ##### + Parse actor ID ####
    
    actor_name <- input$character
    
    if(doctor_pressence()=="TRUE"){
      
      actor_temp<- actors() %>% 
        filter(name==actor_name  | id ==parse_number(input$character)) %>% 
        select(id)
      
      actor_temp <- actor_temp$id[1]
      
    }else {
      actor_temp <- parse_number(actor_name)
    }
   
    #### + All casts extraction ####
    casts <- WCL_API2_request(sprintf(request_cast, 
                                      as.character(extract_log_id(as.character(input$log_id))), 
                                      as.numeric(fight_temp), 
                                      as.numeric(actor_temp)))$data$reportData$report$events$data 
    # request <- WCL_API2_request(sprintf(request_damage,  ## Damage 
    #                               as.character(extract_log_id(as.character(input$log_id))),
    #                               as.numeric(fight_temp), 
    #                               as.numeric(actor_temp), 
    #                               as.numeric(targetID_code$id[1])
    #                               )
    #                             )$data$reportData$report$events$data
    
    pre_combatant_trigger <- any(grepl("combatantinfo", casts$type))
    ignite_pressence <- any(grepl(12654, casts$abilityGameID))
    
    if(pre_combatant_trigger==F & doctor_pressence()=="FALSE"){
 
      ####+ Spec detection ####
       
      spec <- data.frame(arcane_tree=c(0),fire_tree=c(0),frost_tree=c(0))
      
      combatinfo <- WCL_API2_request(
        sprintf(request_spec, 
                as.character(extract_log_id(as.character(input$log_id))), # Log ID
                as.numeric(fight_temp),  # Fight ID
                as.numeric(actor_temp))  # Actor ID
      )$data$reportData$report$events$data
 
      
    } else if (pre_combatant_trigger==T){
      
      spec <- data.frame(arcane_tree=c(0),fire_tree=c(0),frost_tree=c(0))
      combatinfo <- casts[grepl("combatantinfo", casts$type),]
      
    } else { 
      combatinfo <- vector(length = 0)
      }

     
    if(length(combatinfo)!=0){ 
    
      spec_temp <-  combatinfo$talents[[1]][[1]]
      
      spec$arcane_tree[1] = spec_temp[1]
      spec$fire_tree[1] = spec_temp[2]
      spec$frost_tree[1] = spec_temp[3]
      
      spec_main <- spec
      
      spec <- spec %>% mutate(   
        spec = ifelse(arcane_tree>fire_tree & arcane_tree>frost_tree,"Arcane",
                      ifelse(fire_tree>arcane_tree & fire_tree>frost_tree,"Fire","Frost")))
       
      
      spec <- as.character(spec$spec[1])
     # rm(spec_temp)
      
    } else {
 
      spec <- "No Spec"
      
    }
    
    #### Fire mages ####
    
    # Fire mages only from here onward
    # Have to change for frost ignite spec (Pending Hodir log)

    if(spec=="Fire" | ignite_pressence == T | doctor_pressence()=="TRUE" ){
      
      ###### Enchants & Hit metrics ######
      
      if(doctor_pressence()=="FALSE" & spec=="Fire"){
         
        combatinfo <- combatinfo %>% 
          filter(type=="combatantinfo") 
          
        enchants <- combatinfo$gear[[1]] %>%
          mutate(icon = str_extract(icon,"(?<=inv_)[A-Za-z ]+"),
                 permanentEnchant = as.character(permanentEnchant),
                 permanentEnchant = case_when(
                   icon == "helmet" & is.na(permanentEnchant) ~ "helmet",
                   icon == "shoulder" & is.na(permanentEnchant) ~ "shoulder",
                   icon == "chest" & itemLevel >= 150 & is.na(permanentEnchant) ~ "chest",
                   icon == "pants" & is.na(permanentEnchant) ~ "pants",
                   icon == "boots" & is.na(permanentEnchant) ~ "boots",
                   icon == "gauntlets" & is.na(permanentEnchant) ~ "gloves",
                   icon == "bracer" & is.na(permanentEnchant) ~ "bracer",
                   icon == "weapon" & is.na(permanentEnchant) ~ "weapon",
                   icon == "staff" & is.na(permanentEnchant) ~ "staff",
                   lead(icon) %in% c("staff", "weapon") & is.na(permanentEnchant) ~ "cloak",
                   TRUE ~ permanentEnchant
                 )) %>%
          filter(!is.na(permanentEnchant) & !grepl("[0-9]", permanentEnchant)) %>%
          pull(permanentEnchant) %>%
          paste(collapse = ",") %>%
          str_to_sentence() 
          
        
          hitSpell <- combatinfo %>% 
            pull(hitSpell) 
           
          Draenei_buff <- combatinfo %>% 
            pull(auras) %>% 
            `[[`(1) %>% 
            filter(ability == 28878) %>% 
            mutate(Draenei_buff = ifelse(nrow(.) >= 1, paste0("<sup>[-26]</sup>", "<img src='https://wow.zamimg.com/images/wow/icons/large/inv_helmet_21.jpg' height='15' width='15'/>"), "")) %>% 
            slice(1) %>%
            pull(Draenei_buff) %>% 
            toString()
          
          
      } else { 
        enchants = "NO DATA"
        Draenei_buff = ""
        }
      
    
      ###### Targets detection ####
      
      targetID_code <- actors() %>%
        filter(
          grepl(
            paste0("\\b(", 
                   paste(
                     sub("(\\w+).*", "\\1", 
                         fight_name),
                     collapse = "|"), ")\\b"),
            name,
            ignore.case = TRUE)
        ) %>% 
        filter(subType!="Unknown" & 
                 !(name %in% ## Names are searched based on "Single Names" - Using "Algalon" will keep "Algalon the Observer".
                     npc_exclusions)) %>% ## Sometimes, "Hodir" and "Hodir's Wrath" exist. We want the wrath away.
        select(id,name) 
       
      #### Sub-spec detection ####
      if(spec!="No Spec"){
        
        sub_spec <- ifelse(spec_main$arcane_tree[1]>spec_main$frost_tree[1],
                           "TTW",
                           ifelse(spec_main$frost_tree[1]>spec_main$arcane_tree[1],"FFB",
                                  "Error - Contact Forge#0001"))
        
        spec_image <- ifelse(sub_spec=="TTW", # Image for spec
                             "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_flamebolt.jpg' height='25' width='25'/>",
                             ifelse(sub_spec=="FFB",
                                    "<img src='https://wow.zamimg.com/images/wow/icons/large/ability_mage_frostfirebolt.jpg' height='25' width='25'/>", 
                                    "<img src='https://wow.zamimg.com/images/wow/icons/large/trade_engineering.jpg' height='25' width='25'/>"))
      } else {
        
        sub_spec <- spec   
        spec_image <- "<img src='https://wow.zamimg.com/images/wow/icons/large/trade_engineering.jpg' height='25' width='25'/>"
      }
  
      if(length(combatinfo)!=0){ 
        
        if(spec_temp[3]>spec_temp[2] & ignite_pressence==T){
          
          sub_spec <- "Frostnite" 
          spec_image <-   "<img src='https://wow.zamimg.com/images/wow/icons/large/ability_mount_frostyflyingcarpet.jpg' height='25' width='25'/>"
          
        } 
      }
      
      rm(combatinfo)
      
      #### Damage (Main Target only) ####
      damage <- casts %>% 
        filter(type=="damage" & 
                 targetID==as.numeric(targetID_code$id[1]))
      
      
      if(nrow(damage)!=0){
        
        ignite_table_debug <- damage %>% 
          filter(targetID==as.numeric(targetID_code$id[1])& sourceID==as.numeric(actor_temp)) %>% # Source ID for pets like pumpkin
          ignite_cleaning() # See Algalon C9t67a4LWNqpvmcj 
        
        ignite_table <- ignite_table_debug %>%
          ignite_summary()
        
        
        ignite_table_debug$MARKED <- ifelse(str_detect(ignite_table_debug$abilityGameID, "^\\d+$"), "MARKED", "NOT MARKED")
        
        Marked_Data <- subset(ignite_table_debug, MARKED == "MARKED")
        
        ###### Dead ######
        if("overkill" %in% colnames(casts)) {
          dead <- casts %>% 
            filter(targetID == as.numeric(actor_temp)) %>%
            summarize(total_overkill = sum(overkill, na.rm = TRUE)) %>%
            mutate(dead = if_else(total_overkill > 0 & !is.na(total_overkill), "Dead", "")) %>%
            pull(dead)
        } else {dead<-""}
        
        #### Force Spec if needed ####
        
       sum_ffb_cast <-  sum(
          grepl("Frostfire Bolt", 
                ignite_table_debug$abilityGameID))
        
        sum_fb_cast <- sum(
          grepl("Fireball", 
                ignite_table_debug$abilityGameID))
        
        if (spec=="No Spec" & (sum_ffb_cast >  sum_fb_cast)){
          
          sub_spec <- "FFB"
          hitSpell <- "NO DATA"
        } else if (spec=="No Spec" & (sum_ffb_cast < sum_fb_cast)){ 
          
          sub_spec <- "TTW"
          hitSpell <- "NO DATA"
        } 
        rm(sum_fb_cast,sum_ffb_cast)
        
        #### Debuff LB extraction ####
        debuff_table <- WCL_API2_request(
          sprintf(request_debuff, ## Debuffs
                  as.character(extract_log_id(as.character(input$log_id))),  ## Log
                  as.numeric(fight_temp), ## Fight ID
                  as.numeric(targetID_code$id[1]), ## Target as sourceID
                  as.numeric(actor_temp))## Actor as targetID
        )$data$reportData$report$events$data %>% 
          
          filter(abilityGameID ==55360 & 
                   type == "refreshdebuff")

        ### Cast gaps extraction
        
        # casts <- WCL_API2_request(sprintf(request_cast, 
        #                                     as.character(extract_log_id(as.character(input$log_id))), 
        #                                     as.numeric(fight_temp), 
        #                                     as.numeric(actor_temp)))$data$reportData$report$events$data 
        # 
        casts_fb_pyro <- casts %>% 
          filter((abilityGameID==42833 | abilityGameID==42891) & 
                   type=="cast"
          ) %>%
          select(timestamp,abilityGameID) %>%
          mutate(delay = timestamp-lag(timestamp)) %>%
          filter(delay<750)
        
        ######## Pyroblasts cancelled/interrupted #######
        main_pyro <- casts %>% 
          filter(abilityGameID==42891 & 
                   !(type%in%
                       c("damage","refreshdebuff",
                         "applydebuff","removedebuff")) 
          )
        
        pyro_interrupt <- main_pyro %>%
          mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                           type=="begincast",
                                         "Interrupted","OK")) %>% 
          filter(flag_interrupt=="Interrupted")
        
        ######## Pyroblasts hard-casted #######
        
        pyro_hard_cast <- main_pyro %>%
          mutate(cast_time = ifelse(type=="cast" & 
                                      lag(type)=="begincast", 
                                    timestamp-lag(timestamp),
                                    NA)
          ) %>% 
          filter(cast_time>500)
        
        ######## Pyroblasts count #######
        
        pyro_n <- main_pyro %>% 
          filter(abilityGameID==42891 & 
                   type=="cast") 
        
        
        rm(main_pyro)
        ##### + FFB MAGE SECTION #####
        
        if(sub_spec=="FFB"){
          
          ######## FFB cancelled/interrupted  #######
          
          frostfirebolt_interrupt <- casts %>% 
            filter(abilityGameID==47610 & 
                     !(type%in%
                         c("damage","refreshbuff",
                           "applydebuff","removebuff")) 
            )%>%
            mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                             type=="begincast",
                                           "Interrupted","OK")) %>% filter(flag_interrupt=="Interrupted")
    
          ### FFB Hit Cap
          Draenei_buff<- ifelse(exists("Draenei_buff") | !is.na(Draenei_buff),Draenei_buff,"")
          
          hitCap <- paste0("210 to 288",Draenei_buff)
          hitCap <- ifelse(hitSpell>=10,hitCap,"")
          
          realhitCap <- ifelse(nchar(Draenei_buff)>1, 262, 288)
          
          alert_hit_1 <- ifelse(hitSpell>realhitCap+90 | hitSpell<realhitCap-128,
                                "<font color=\"#BE5350\">","")
          
          alert_hit_2 <- ifelse(hitSpell>realhitCap+90 | hitSpell<realhitCap-128,
                                "</font>","") 
          
          ### Main Spell
          str_mainspell <- paste0("- Frostfire Bolt cancelled: ",nrow(frostfirebolt_interrupt))
          
        }
        
        ##### + Fireball MAGE SECTION #####
        
        if(sub_spec=="TTW"){
          
          ######## Fireball cancelled/interrupted  #######
          
          fireball_interrupt <- casts %>% 
            filter(abilityGameID==42833 & 
                     !(type%in%
                         c("damage","refreshbuff",
                           "applydebuff","removebuff")) 
            )%>%
            mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                             type=="begincast",
                                           "Interrupted","OK")) %>% 
            filter(flag_interrupt=="Interrupted")
      
          ### TTW Hit Cap
          Draenei_buff<- ifelse(exists("Draenei_buff") | !is.na(Draenei_buff),Draenei_buff,"")
          hitCap <- paste0("367",Draenei_buff)
          hitCap <- ifelse(hitSpell>=10,hitCap,"")
          realhitCap <- ifelse(nchar(Draenei_buff)>3, 341, 367)
          
          alert_hit_1 <- ifelse(hitSpell>realhitCap+90 | hitSpell<realhitCap-50,
                              "<font color=\"#BE5350\">","")
          alert_hit_2 <- ifelse(hitSpell>realhitCap+90 | hitSpell<realhitCap-50,
                                "</font>","")
          ### Main Spell
          str_mainspell <- paste0("- Fireball cancelled: ",nrow(fireball_interrupt)) 
          
          
          
        }
        
        ##### + NO SPEC#####

        if(sub_spec=="No Spec"){
          
          hitCap <- "NO DATA"
          str_mainspell <- "NO DATA"
         # hitSpell <- "NO DATA"
        }
        
        ## Hot streaks and Pyros Hotstreaks
        
        insta_pyros_db <- casts %>% 
          
          filter(abilityGameID==48108 | 
                   (abilityGameID==42891 & type=="cast")) %>%
          
          select(timestamp,abilityGameID,type,sourceID,targetID)%>% 
          
          filter(type != "refreshbuff" &
                   !(abilityGameID==48108 & type == "cast" )) %>% 
          mutate(set = cumsum(ifelse(type == "applybuff", 1, 0)),
                 
                 skip = ifelse(lag(type, default = "") == "cast" & 
                                 lead(type, default = "") == "applybuff", 
                               1, 0)) 
        
        df_casts_per_set <- insta_pyros_db %>% 
          filter(type %in% c("applybuff", "cast", "removebuff")) %>% 
          group_by(set) %>% 
          #mutate(skip = lag(skip, default = 0)) %>% 
          filter(skip == 0) %>% 
          summarise(casts_per_set = sum(type == "cast"))
        
        insta_pyros_db <- left_join(insta_pyros_db, 
                                    df_casts_per_set, by = "set")
        
         
        ### Hot Streaks
        
        hot_streak_n <- casts %>% 
          
          filter(abilityGameID==48108 & type=="applybuff") %>%
          
          select(timestamp,abilityGameID,type,sourceID,targetID)
        
        ### Hot Streaks refresh
        
        hot_streak_ref <- casts %>% 
          
          filter(abilityGameID==48108 & type=="refreshbuff") %>%
          
          select(timestamp,abilityGameID,type,sourceID,targetID)
        
        ##### Resources ####
        
        resources <- casts %>% 
          filter(sourceID == as.numeric(actor_temp) & targetID==as.numeric(actor_temp)#& sourceID==4
          )
        
        
        mana <- casts %>% 
          mutate(row = map(classResources, ~ bind_rows(.x))) %>%
          select(row,timestamp) %>% 
          unnest(row) %>%
          mutate(mana_per = amount/max)%>%
          filter(timestamp==max(timestamp))
        
        
        ### Render output ######
        ignite_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_incinerate.jpg' height='20' width='20'/>"
        ignite_total_dealt <- round(ignite_table$Total_Ignite_Dmg_Dealt)
        ignite_lost_sadge <- round(ignite_table$Ignite_tick_lost_dead2)
        
        n_total_pyros<- round(as.numeric(nrow(pyro_n)),2)
        n_total_pyro_hard_cast <- round(as.numeric(nrow(pyro_hard_cast)),2)
        n_total_hot_streak <- round(as.numeric(nrow(hot_streak_n)),2)
        
        n_insta_pyros <- n_total_pyros-n_total_pyro_hard_cast
        
        
        boss_name_singular <- sapply(strsplit(targetID_code$name[1], " "), `[`, 1) 
        
        
        Munch_NET_result <- (round(ignite_table$Munch_NET_2)*-1)
        
        lowest_hp <- min(resources$hitPoints,na.rm=T)
        max_sp <- max(resources$spellPower,na.rm=T)
        
        mana_end_n <- round(min(mana$mana_per,na.rm=T),2)*100
        
        mana_end <-  ifelse(nchar(dead)>2, paste0("<font color=\"#BE5350\">Dead (",mana_end_n,"%)</font>"),
                            paste0(mana_end_n,"%"))
      
        lb_clipped <- nrow(debuff_table)
        
        lb_clipped_alert_1 <- ifelse(lb_clipped==0,"","<font color=\"#BE5350\">")
        lb_clipped_alert_2 <- ifelse(lb_clipped==0,"","</font>")
        
        
        ignite_lost_alert_1 <- ifelse(ignite_lost_sadge>=30000 & ignite_lost_sadge<=50000 & targetID_code$name[1]!="Hodir","<font color=\"#D78613\">",
                                      ifelse(ignite_lost_sadge>50000 & targetID_code$name[1]!="Hodir","<font color=\"#BE5350\">",""))  ## I need a better way to show this 
        ignite_lost_alert_2 <- ifelse(ignite_lost_sadge>=30000 & targetID_code$name[1]!="Hodir","</font>","") ## I need a better way to show this 
        ####  Header   ####
        output$summary_header <- renderUI({
  

          HTML(paste(paste0("<h3> Metrics for ",actor_name,
                            " on ",fight_name," - ",
                            sub_spec," ",spec_image,"</h3>"),
                     paste0("<b>Resources through the fight:</b>"),
                     paste0(alert_hit_1,"Hit: ",hitSpell," / ",hitCap,alert_hit_2," <b>|</b> ",
                            "Lowest HP%: ",lowest_hp,"% <b>|</b> ",
                            "Highest Spellpower: ",prettyNum(max_sp,big.mark=",",scientific=FALSE)," <b>|</b> ",
                            "End-of-fight mana: ",mana_end), 
                     sep = '<br/>'))
        })
        
        
         
        if(nchar(enchants)>0){
          
          output$alert_header <- renderUI({
            
            HTML(paste(paste0("<font color=\"#BE5350\">Missing Enchants: ",enchants,".</font>"), 
                       sep = '<br/>'))
          })
          
          
        }
        
          
        
        ####  Ignite Metrics left  ####  
        
        output$summary_ignite_1 <- renderUI({
          

          str1 <- paste0( "- Expected ignite damage<sup>*</sup>: ",  prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential)),big.mark=",",scientific=FALSE))
          str2 <- paste0( "- Actual ignite damage dealt<sup>*</sup>: ",  prettyNum((ignite_total_dealt),big.mark=",",scientific=FALSE))
          str4 <- paste0(prettyNum((ignite_total_dealt),big.mark=",",scientific=FALSE), " - ",
                         prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential))- (ignite_lost_sadge),big.mark=",",scientific=FALSE),
                         " = ",
                         prettyNum(Munch_NET_result,big.mark=",",scientific=FALSE))
          
          if(Munch_NET_result > 0 & Munch_NET_result >= 10) { 
            ## Vomit trigger
            str5 <- paste0("<font color=\"#61B661\"><b> The total ignite damage dealt was ",prettyNum(Munch_NET_result,big.mark=",",scientific=FALSE),
                           " more than expected. <br> This means VOMIT was present at some point.</b></font>")
            ## Munch Trigger
          } else if(Munch_NET_result < 0 & Munch_NET_result<= -10){ 
            str5 <- paste0("<font color=\"#BE5350\"><b> The total ignite damage dealt was ",prettyNum(Munch_NET_result,big.mark=",",scientific=FALSE),
                           " less than expected. <br> This means MUNCH was present at some point.</b></font>")
            ## Expected ignite
          } else { 
            str5 <- paste0("<font color=\"#54A5BE\"><b>You dealt the expected ignite damage. No munch or vomit.</b></font>")  
          }
          
          ### Ignite ###
          
          ######### TBD
          #str_min <- paste0( "- Lowest ignite tick: ",  prettyNum((min(ignite_table_debug$igniteSUB_resist)),big.mark=",",scientific=FALSE))
          str_summ1 <- paste0("- Expected ignite damage before target death<sup>*</sup>: ",
                              prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential)),big.mark=",",scientific=FALSE),
                              " - ",  
                              prettyNum(ignite_lost_sadge,big.mark=",",scientific=FALSE),
                              " = ",prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential))- (ignite_lost_sadge),big.mark=",",scientific=FALSE) )
          
          HTML(paste(
            paste0("<h4> <b>",ignite_img," Munching Metrics (Main Target only) </b> </h4>"),
            str5,
            str1, #Expected ignite damage 
            str_summ1, 
            str2, 
            paste0("<b>Result:</b> ",str4),
            "<br/",
            sep = '<br/>'))
          
        })
         
        ####  Ignite Metrics right  ####  
        
        output$summary_ignite_2 <- renderUI({
          
          str3 <- paste0(ignite_lost_alert_1,"- Ignite lost to (target) death<sup>1</sup>: ",  prettyNum(ignite_lost_sadge,big.mark=",",scientific=FALSE),ignite_lost_alert_2)
          str2_res <- paste0( "- Ignite damage dealt (after resists): ",  prettyNum((round(ignite_table$Total_Ignite_Dmg_Dealt_resist)),big.mark=",",scientific=FALSE))
          str_max <- paste0( "- Highest ignite tick: ",  prettyNum((max(ignite_table_debug$igniteSUB_resist)),big.mark=",",scientific=FALSE))
          ## Final format
          HTML(paste(
            paste0("<h4><b>",ignite_img," Ignite Metrics (Main Target only)</b></h4>"),
            str3,
            str2_res,
            str_max,
            sep = '<br/>'))
          
        })

        ####  Cast Metrics delay left  ####  
        
        output$cast_delays_1 <- renderUI({
          
          ### Delay gaps ###
          
          # Logic for grief TTW?
          if(nrow(casts_fb_pyro)>0){
            
            ### Alert for 0ms casts
            if( (nrow(casts_fb_pyro %>% 
                      filter(delay == 0)) / nrow(casts_fb_pyro) ) > 0 & 
                (nrow(casts_fb_pyro %>% 
                      filter(delay == 0)) / nrow(casts_fb_pyro) ) < 0.30 & 
                sub_spec=="TTW"){
              
              str_delay_5 <- paste0("<font color=\"#D78613\"> - Delays at 0ms: ", 
                                    nrow(casts_fb_pyro %>% 
                                           filter(delay == 0)),"</font>"
              )
              str_alert <- paste0("<font color=\"#D78613\">Your munching prevention method might be failing ocasionally.<sup>4</sup></font>")
              
            } else if((nrow(casts_fb_pyro %>% 
                            filter(delay == 0)) / nrow(casts_fb_pyro) ) >= 0.30  & 
                      sub_spec=="TTW"){
              
              str_delay_5 <- paste0("<font color=\"#BE5350\"> - Delays at 0ms: ", 
                                    nrow(casts_fb_pyro %>% 
                                           filter(delay == 0)),"</font>")
              str_alert <- paste0("<font color=\"#BE5350\">Are you using a WA for munching?<sup>4</sup></font>")
              
            } else if(sub_spec=="TTW") {       
              str_delay_5 <- paste0("<font color=\"#54A5BE\"> - Delays at 0ms: ", 
                                    nrow(casts_fb_pyro %>% 
                                           filter(delay == 0)),"</font>")
              str_alert <- paste0("<font color=\"#54A5BE\">Your WA or anti-munching method seems to be working.<sup>4</sup></font>")
              
            } else {
              str_delay_5 <- paste0("- Delays at 0ms: ", 
                                    nrow(casts_fb_pyro %>% 
                                           filter(delay == 0))) 
              str_alert <- ""
              
            }
          }else if(sub_spec=="FFB") {
            str_alert <- paste0("<font color=\"#54A5BE\">Delays between fireballs and pyroblast are only relevant for TTW mages. If you're playing FFB put everything you think you know about ignite munching in a box and close the lid and slide it under your bed.<sup>amyy#7377</sup></font>")
            #str_alert <- paste0("<font color=\"#54A5BE\">Delays between fireballs and pyroblast are only relevant for TTW mages. </font>")
            str_delay_5 <- paste0("- Delays at 0ms: ", 
                                  nrow(casts_fb_pyro %>% 
                                         filter(delay == 0))) 
          } else {
            str_delay_5="" 
            str_alert=""}
          
          str_delay_6 <- paste0("- Delays at >0ms and <100ms: ",
                                nrow(casts_fb_pyro %>% filter(delay > 0 & delay < 100)))
          
          str_delay_7 <- paste0("- Delays at >=100ms and <300ms: ",
                                nrow(casts_fb_pyro %>% filter(delay >= 100 & delay < 300)))
          
          str_delay_500 <- paste0("- Delays at >=300ms and <500ms: ",
                                nrow(casts_fb_pyro %>% filter(delay >= 300 & delay < 500)))
          
          str_lb_clip <- paste0(lb_clipped_alert_1,"- Living Bombs clipped<sup>2</sup>: ", nrow(debuff_table),lb_clipped_alert_2)
          
          casts_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/ability_hunter_pet_turtle.jpg' height='20' width='20'/>"
          
          
          HTML(paste(
            paste0("<h4> <b>",casts_img," Cast metrics (Main Target only)</b> </h4>"),
            paste0("<b>Living Bomb metrics:</b>"),
            str_lb_clip,
            "<br/",
            paste0("<b>Milliseconds between Fireball and Pyroblast casts (<750ms):</b>"),
            str_delay_5,
            str_delay_6,
            str_delay_7,
            str_delay_500,
            str_alert,
            "<br/",
            sep = '<br/>'))
          
        }) 
        
        ####  Cast Metrics delay right summary ####  
        
        output$cast_delays_2 <- renderUI({
          
        max_delay_alert<- max(casts_fb_pyro$delay,na.rm=T)  
        
        if(max_delay_alert>=500){
          
          str_delay_2 <- paste0("<font color=\"#D78613\">- Max. Delay:", max_delay_alert, " ms<sup>High</sup></font>")
          
        } else{ 
          str_delay_2 <- paste0("- Max. Delay: ", max_delay_alert, " ms")
          
          }
        
        
          
          
          
          str_delay_1 <- paste0("- Avg. Delay: ", as.integer(mean(casts_fb_pyro$delay,na.rm=T))," ms")

          str_delay_3 <- paste0("- Min Delay: ", min(casts_fb_pyro$delay,na.rm=T) , " ms")
          str_delay_4 <- paste0("- Total Insta-Pyros (after fireball): ", nrow(casts_fb_pyro))
          str_delay_5 <- paste0("- Median Delay: ", as.integer(median(casts_fb_pyro$delay,na.rm=T))," ms")
          
          
          HTML(paste(paste0(""),
                     "<br/",
                     "<br/",
                     "<br/",
                     "<br/",
                     "<br/",
                     str_delay_1,
                     str_delay_2,
                     str_delay_3,
                     str_delay_5, 
                     str_delay_4,
                     sep = '<br/>'))
          
          
        }) 
        
        ####  Cast Metrics Pyroblast #### 
        
        output$cast_metrics_1 <- renderUI({
          
          
          str_total_pyro <- paste0("- Total Pyroblasts: ", n_total_pyros)
          str_insta_pyro <- paste0("- Total Insta-Pyros: ", n_insta_pyros)
          
          fblast_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_fireball.jpg' height='20' width='20'/>"
          
          str_pyro_canned <- paste0("- Pyroblasts cancelled/interrupted: ",nrow(pyro_interrupt)) 
          str_pyro_hard <- paste0("- Pyroblasts hard-cast: ",n_total_pyro_hard_cast)

          
          HTML(paste(
            paste0("<h4> <b>",fblast_img," Cast Metrics (Encounter-wide, All Targets)</h4> </b>"),
            paste0("<b>Pyroblast metrics:</b>"),
            str_total_pyro,
            str_insta_pyro,
            str_pyro_hard,
            str_pyro_canned, 
            
            sep = '<br/>'))
          
        })
        ####  Main Spell metrics (FB/FFB) & Hotstreaks #### 
        
        output$cast_metrics_2 <- renderUI({
          
          pyros_per_hotstreak <- round(n_insta_pyros/n_total_hot_streak, digits = 2)
          
          ################### REVIEW ##################
          if(pyros_per_hotstreak<1 & boss_name_singular %in% c("General","Hodir","Freya")){
            
          str_hotstreak_pyro <- paste0("<font color=\"#D78613\">- # Pyros per Hot Streak: ",pyros_per_hotstreak,"</font>" )
        
          } else if(pyros_per_hotstreak<1){
            
            str_hotstreak_pyro <- paste0("<font color=\"#BE5350\">- # Pyros per Hot Streak: ",pyros_per_hotstreak,"</font>")
            
          } else if(pyros_per_hotstreak>2){
            
            str_hotstreak_pyro <- paste0("<font color=\"#61B661\">- # Pyros per Hot Streak: ",pyros_per_hotstreak,"<sup>High</sup></font>")
  
          } else{
            
            str_hotstreak_pyro <- paste0("- # Pyros per Hot Streak: ",pyros_per_hotstreak)
          }
            
          
          str_hotstreak_n <- paste0("- # Hot Streaks (Buff): ",n_total_hot_streak)
          str_4pct8 <- paste0("- # 4pcT8 Pyros used: ", round(n_insta_pyros-n_total_hot_streak, digits = 2))
          str_refreshpyro <- paste0("- Hot Streaks 'refreshed'<sup>5</sup>: ",nrow(hot_streak_ref))
          HTML(paste(
            "<br/",
            paste0("<b>Main spell metrics:</b>"),
            str_mainspell,
            "<br/",
            paste0("<b>Hot Streak:</b>"),
            str_hotstreak_n,
            str_hotstreak_pyro,
            str_4pct8,
            str_refreshpyro,
            sep = '<br/>'))
          
        }) 
        
        ####  Caption #### 
        
        output$caption_1 <- renderUI({
          HTML(paste(
            "<br/",
            paste0("<i><sup>1</sup> If a target dies before the 'stored' Ignite Damage has time to tick, any damage 'stored' in the Ignite is lost. This is NOT munching.</i>"), 
            paste0("<i><sup>2</sup> # of Living Bombs refreshed BEFORE they had time to explode.</i>"), 
            paste0("<i><sup>4</sup> Unsure of what this means? Ask in Mage Discord (Link to your left)</i>"), 
            paste0("<i><sup>5</sup> Hot Streaks not fully used. Not fully consumed before it got 'refreshed'. i.e. You got hot streak while you had hot streak.</i>"), 
            paste0("<i><sup>*</sup> Before partial resists.</i>"), 
            sep = '<br/>'))
          
        })
        
        ####  Alert Algalon #### 
        
        
        output$extra_algalon <- renderUI({
          
          
          if(targetID_code$name[1]=="Algalon the Observer" & nrow(Marked_Data)>0 & (Munch_NET_result < 0 & Munch_NET_result<= -10)){
            
            HTML(paste("<br/",paste0("Notes:"),
                       paste0("<font color=\"#D78613\">- Crits on Algalon while you are inside the black hole do NOT generate or add towards ignite - It will be considered as 'munching' in the calculation above</font>"),
                       paste0("<font color=\"#D78613\">- There might be issues with your munching result - Try looking at them in 1-2 days and see if this note appears again. </font>"),
                       "<br/",
                       sep = '<br/>'))
          } else if(targetID_code$name[1]=="Algalon the Observer" & (Munch_NET_result < 0 & Munch_NET_result<= -10)){
            
            HTML(paste("<br/",paste0("Notes:"),
                       paste0("<font color=\"#D78613\">- Crits on Algalon while you are inside the black hole do NOT generate or add towards ignite - It will be considered as 'munching' in the calculation above"),
                       "<br/",
                       sep = '<br/>')) 
            
          } else  if(nrow(Marked_Data)>0){
            HTML(paste("<br/",paste0("Notes:"),
                       paste0("<font color=\"#D78613\">- There might be issues with your munching result - Try looking at them in 1-2 days and see if this note appears again. </font>"),
                       "<br/",
                       sep = '<br/>'))
            
          } else { 
            HTML(paste(paste0("",
                              sep = '<br/>'))) }
        })
        
        ###### Leaderboard ####
        if(fight_name != "Dr. Boom"){
        #https://medium.com/@marinebanddeluxe/create-your-serverless-database-with-google-sheets-and-shiny-part-i-26e69b8253db,
        #   water <- URLencode(as.character(water))
        #         # #   leaderboard[nrow(leaderboard)+1,1] <- as.character(extract_log_id(as.character(input$log_id)))  
        #  #  leaderboard[nrow(leaderboard),2] <- as.character(actor_name) 
        #  #  leaderboard[nrow(leaderboard),3] <- round(ignite_table$Munch_NET_2)*-1 
        #   # leaderboard[nrow(leaderboard),4] <-  round((as.integer(nrow(pyro_n))-as.integer(nrow(pyro_hard_cast)))/as.integer(nrow(hot_streak_n)), digits = 2)
        #   #leaderboard[nrow(leaderboard),5] <- targetID_code$name[1] 
        url <- paste0(as.character(Sys.getenv("LEADERBOARD_ID")),
                      as.character(extract_log_id(as.character(input$log_id))),
                      "&entry.96171645=",
                      sapply(strsplit(actor_name, " "), `[`, 1),
                      "&entry.1179038397=",
                      round(ignite_table$Munch_NET_2)*-1 ,
                      "&entry.1734686763=",
                      round(n_insta_pyros/n_total_hot_streak, digits = 2),
                      "&entry.1228481340=",
                      boss_name_singular,
                      "&entry.1629132558=",
                      median(casts_fb_pyro$delay,na.rm=T),
                      "&entry.1806850293=",
                      mean(casts_fb_pyro$delay,na.rm=T),
                      "&entry.1724070192=",
                      nrow(debuff_table),
                      "&entry.1899085980=",
                      max(ignite_table_debug$igniteSUB_resist),
                      "&entry.303429649=",
                      ignite_total_dealt,
                      "&entry.1911698108=",
                      sub_spec,
                      "&entry.1948745936=",
                      as.numeric(nrow(Marked_Data)),
                      "&entry.1124325413=",
                      max_sp,
                      "&entry.2031855061=",
                      nchar(enchants),
                      "&entry.1370589148=",
                      ignite_lost_sadge
                      )
        
        
        #nrow(Marked_Data)
        
        
        res <- POST(url = url)
        
        }
        #writesheet("user1", 700)  
        
        #   file_content <- readBin(".secrets.rar", "raw", file.info(".secrets.rar")$size)
        #   encoded_content <- base64encode(file_content)
        #   
        #   # Decode the contents of the secret archive from base64
        #   decoded_secret <- base64(encoded_content)
        #   
        #   # Write the decoded contents of the secret archive to a temporary file
        #   tmpfile <- tempfile(fileext=".rar")
        #   writeBin(charToRaw(decoded_secret),tmpfile,"output-file.ext")
        #   
        #   # Extract the contents of the secret archive
        #   unzip(tmpfile, exdir = ".")
        #   untar(tmpfile, exdir = ".")
        #   
        #   # Authenticate with Google Sheets using the extracted credentials
        #   creds <- gs4_auth()
        #   
        #   # Set the credentials as the default authentication method
        #   gs4_auth(creds = creds)
        #   
        #   # Read a sheet
        #   sheet <- gs4_get("https://docs.google.com/spreadsheets/d/SPREADSHEET_ID/edit")
        #   
        #   # Write to a sheet
        #   gs4_write(sheet, data = mtcars, sheet = "MySheet", range = "A1")
        #   
        #   a<- gargle_oauth_client(id=Sys.getenv("DRIVE_ID"),
        #                           secret=Sys.getenv("DRIVE_SECRET"),
        #                           name="mage-analytics",
        #                           type="web",
        #                          redirect_uris="https://wotlk-mage.herokuapp.com/")
        #   
        #   creds_a <- credentials_service_account(path= creds_str)
        #   
        #   gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)
        #   gs4_auth(creds_a)
        #   #gs4_auth(creds_a)
        #   drive_auth(cache = ".secrets", email = TRUE)
        #   #drive_deauth()
        #   #drive_auth(email=Sys.getenv("EMAIL_DRIVE"),
        #  #            token =a )#jsonlite::fromJSON(Sys.getenv("DRIVE_ACCOUNT")))
        #   #gs4_auth(token=a,
        #    #        email=Sys.getenv("EMAIL_DRIVE"))
        #   #gs4_auth("canalhorchatero@gmail.com")
        #   #gs4_auth(path=   Sys.getenv("DRIVE_KEY"))
        #   #a<-gar_auth_service(json = json_string2)
        #   
        #   leaderboard <- read_sheet(drive_get("leaderboard"))
        #    
        # #   leaderboard[nrow(leaderboard)+1,1] <- as.character(extract_log_id(as.character(input$log_id)))  
        #  #  leaderboard[nrow(leaderboard),2] <- as.character(actor_name) 
        #  #  leaderboard[nrow(leaderboard),3] <- round(ignite_table$Munch_NET_2)*-1 
        #   # leaderboard[nrow(leaderboard),4] <-  round((as.integer(nrow(pyro_n))-as.integer(nrow(pyro_hard_cast)))/as.integer(nrow(hot_streak_n)), digits = 2)
        #   #leaderboard[nrow(leaderboard),5] <- targetID_code$name[1] 
        #    
        # #write_sheet(leaderboard,ss=as.character(Sys.getenv("SHEET_KEY")),sheet="leaderboard")
        #   
        
      } else {
        
        showModal(error_diag(paste0(error3,as.character(extract_log_id(as.character(input$log_id))),
                                    as.numeric(fight_temp), 
                                    as.numeric(actor_temp), 
                                    as.numeric(targetID_code$id[1])),3))
        
      }
      
    } else if(spec!="No Spec"){
      
      showModal(modalDialog(
        title = "Error 4",
        paste0("It looks like that character is ",spec," Mage on that log. If you think this is an error, contact Forge#0001 on discord or try refreshing"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
      
      
    } else { 
      showModal(modalDialog(
        title = "Error 5",
        paste0("It looks like that character has no data for that fight, or it is possible no talent data is available in the log for that fight. If you think this is an error, contact Forge#0001 on discord or try refreshing - Analysis without talents will be available in a couple days."),
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
    
    ### STEP 3: Table debug ####

    observeEvent(input$debug_id, {
    
      output$table2 <- renderDataTable({
        
        ignite_table_debug %>% 
          select(-c(hitType, munched_temp)) %>%
          rename(Ignite_Chunk=IGNITE_END ,
                 time=timestamp,
                 ability = abilityGameID,
                 unm_amo = unmitigatedAmount,
                 time2=timestamp_2)
      }) 
      
      output$extra_else <- renderUI({
        
        str_pyro_hot <- paste0("- Pyroblasts w/ Hot Streak (DEBUG - For all intendand purposes, this metric could be wrong - Only interpret if you know):",sum(df_casts_per_set$casts_per_set) )
        str_pyro_hard_2 <- paste0("- Pyroblasts Hard-Cast (debug) - For all intendand purposes, this metric could be wrong - Only interpret if you know:",sum(insta_pyros_db$skip))
        
        HTML(paste(
          str_pyro_hot,str_pyro_hard_2,length(enchants),enchants,#url, # creds$type, 
          sep = '<br/>'))
        
      })  
      
    }) # End of Step 3 
    
  }) # End of Step 2
  
} # END OF SERVER
