
#########################################################################################
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
                includeResources: false
                
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


#########################################################################################


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
    mutate(IGNITE_END =  if_else(lag(abilityGameID)=="Ignite" &  
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


#########################################################################################


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
 
  output$summary_ignite <- renderUI({ HTML(paste(paste0("")))})
  
  ### Style settings ####
  
  tags$style(HTML("
    h4, .h4 {
      margin-top: 0;
      margin-bottom: 0;
    }
  "))
  
  ### Actors list ####
  
  actors <-eventReactive(input$submit_log_id, {
    
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
  
  ### Fights list ####
  
  fights <-eventReactive(input$submit_log_id, {
    
    fights<-WCL_API2_request(
      sprintf(
        request_fights, # Request 
        as.character(extract_log_id(as.character(input$log_id))) # log ID
      )
    )$data$reportData$report$fights
    
    if(!is.null(fights)){
    
    ### DR. Boom logic
    if(any(actors()$name %in% c("Dr. Boom"))==TRUE){
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
  
  ### Actors list update ####
  
  observeEvent(input$submit_log_id, {
    
    ### Actor update ###
    if(is.data.frame(actors())==TRUE){
      
      mages <- actors() %>% 
        filter(subType=="Mage") %>% 
        mutate(name = paste0(name, " (ID:",id,")"))
      
      if(nrow(mages)>0){
        
        updateSelectInput(session, "character", choices = mages$name)
      
        ### DR. Boom logic
        
      } else if(any(actors()$name %in% c("Dr. Boom"))==TRUE) {
        
        updateSelectInput(session, "character", choices = actors()$name)
        
      } else {
        showModal(error_diag(error1,1))
        updateSelectInput(session, "character", choices = "NO MAGES")
      }
      
    } else if(is.data.frame(actors())==FALSE) {
      
      showModal(error_diag(error1,1))
      updateSelectInput(session, "character", choices = actors())
    }
    
    
    ### Fight Update ###
    if(is.data.frame(fights())==TRUE){
      
      if(max(fights()$encounterID)>0){
        
        updateSelectInput(session, "fight", choices = fights()$encounterID_2)
      
      } else if(any(actors()$name %in% c("Dr. Boom"))==TRUE){  
        
        updateSelectInput(session, "fight", choices = fights()$startTime)
        
      } else if(actors()!="NO DATA"){
        
        showModal(error_diag(error2,2))
        updateSelectInput(session, "fight", choices = "NO BOSS FIGHTS")
        
      }else {
        
        updateSelectInput(session, "fight", choices = "NO BOSS FIGHTS")
      }
      
    } else if(is.data.frame(fights())==FALSE & actors()!="NO DATA") {
      
      showModal(error_diag(error2,2))
      updateSelectInput(session, "fight", choices = fights())
    }
    
  })


### STEP 2: Retrieve data and estimations ####
 
  observeEvent(input$submit_char_id, {
    
    output$summary_ignite <- renderUI({ HTML(paste(paste0("")))})
    
    ## Fight and Actor IDs
    fight_name <- input$fight
    
    if(any(actors()$name %in% c("Dr. Boom"))==TRUE){
      
      fight_temp<- fights() %>% 
        filter(startTime==as.numeric(fight_name)) %>% 
        select(id)
      
      fight_temp <- fight_temp$id[1]
      
    }else {
      fight_temp <- parse_number(input$fight)
    }

    
    actor_name <- input$character
    
    if(any(actors()$name %in% c("Dr. Boom"))==TRUE){
      
      actor_temp<- actors() %>% 
        filter(name==actor_name  | id ==parse_number(input$character)) %>% 
        select(id)
      
      actor_temp <- actor_temp$id[1]
      
    }else {
    actor_temp <- parse_number(input$character)
    }
    
    
    ### Spec detection
  
    spec <- data.frame(arcane_tree=c(0),fire_tree=c(0),frost_tree=c(0))
    
    request <- WCL_API2_request(
      sprintf(request_spec, 
              as.character(extract_log_id(as.character(input$log_id))), # Log ID
              as.numeric(fight_temp),  # Fight ID
              as.numeric(actor_temp))  # Actor ID
    )$data$reportData$report$events$data
    
    if(length(request)!=0){
      request <-  request[[32]][[1]][[1]]
      
      spec$arcane_tree[1] = request[1]
      spec$fire_tree[1] = request[2]
      spec$frost_tree[1] = request[3]
      
      spec_main <- spec
      
      spec <- spec %>% mutate(   
        spec = ifelse(arcane_tree>fire_tree & arcane_tree>frost_tree,"Arcane",
                      ifelse(fire_tree>arcane_tree & fire_tree>frost_tree,"Fire","Frost")))
      
      spec <- as.character(spec$spec[1])
      
    } else{spec <- "No Spec"}
    
    
    ## Fire mages only from here onward
    ## Have to change for frost ignite spec
  
    if(spec=="Fire" |  any(actors()$name %in% c("Dr. Boom"))==TRUE ){
      
      
      ### Targets detection
      
      if(any(actors()$name %in% c("Dr. Boom"))==TRUE){
        fight_name="Dr. Boom"
        }

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
      
      ## Sub-spec detection
      if(spec!="No Spec"){
        
      sub_spec <- ifelse(spec_main$arcane_tree[1]>spec_main$frost_tree[1],
                         "TTW",
                         ifelse(spec_main$frost_tree[1]>spec_main$arcane_tree[1],"FFB",
                                "Error - Contact Forge#0001"))
      
      ## Image for spec
      spec_image <- ifelse(sub_spec=="TTW",
                           "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_flamebolt.jpg' height='25' width='25'/>",
                           ifelse(sub_spec=="FFB",
                                  "<img src='https://wow.zamimg.com/images/wow/icons/large/ability_mage_frostfirebolt.jpg' height='25' width='25'/>", 
                                  "<img src='https://wow.zamimg.com/images/wow/icons/large/trade_engineering.jpg' height='25' width='25'/>"))
      } else {
        sub_spec <- spec   
        spec_image <- "<img src='https://wow.zamimg.com/images/wow/icons/large/trade_engineering.jpg' height='25' width='25'/>"
        }
      ### Damage and casts extraction

      
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
            # 
            
       
       damage <- casts %>% filter(type=="damage" & targetID==as.numeric(targetID_code$id[1]))
       
       
      if(length(damage)!=0){
        
        ignite_table_debug <- damage %>% 
          filter(targetID==as.numeric(targetID_code$id[1])& sourceID==as.numeric(actor_temp)) %>% # Source ID for pets like pumpkin
          ignite_cleaning() # See Algalon C9t67a4LWNqpvmcj 
        
        ignite_table <- ignite_table_debug %>%
          ignite_summary()
        
        
        ### Debuff LB extraction
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
        
        ## Pyroblasts cancelled/interrupted
        
        pyro_interrupt <- casts %>% 
          filter(abilityGameID==42891 & 
                   !(type%in%
                       c("damage","refreshdebuff",
                         "applydebuff","removedebuff")) 
                 )%>%
          mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                           type=="begincast",
                                         "Interrupted","OK")) %>% 
          filter(flag_interrupt=="Interrupted")
        
        ## Pyroblasts hard-casted
        
        pyro_hard_cast <- casts %>% 
          filter(abilityGameID==42891 & 
                   !(type%in%
                       c("damage","refreshdebuff",
                         "applydebuff","removedebuff")) 
          )%>%
          mutate(cast_time = ifelse(type=="cast" & 
                                      lag(type)=="begincast", 
                                    timestamp-lag(timestamp),
                                    NA)
          ) %>% 
          filter(cast_time>500)
      
        ## Pyroblasts count
        
        pyro_n <- casts %>% 
          filter(abilityGameID==42891 & 
                   type=="cast") 

        
        ## Fireball cancelled/interrupted
        
        fireball_interrupt <- casts %>% 
          filter(abilityGameID==42833 & 
                   !(type%in%
                       c("damage","refreshbuff",
                         "applydebuff","removebuff")) 
          )%>%
          mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                           type=="begincast",
                                         "Interrupted","OK")) %>% filter(flag_interrupt=="Interrupted")
        
        ## FFB cancelled/interrupted
        
        frostfirebolt_interrupt <- casts %>% 
          filter(abilityGameID==47610 & 
                   !(type%in%
                       c("damage","refreshbuff",
                         "applydebuff","removebuff")) 
          )%>%
          mutate(flag_interrupt = ifelse(lead(type)=="begincast" & 
                                           type=="begincast",
                                         "Interrupted","OK")) %>% filter(flag_interrupt=="Interrupted")
        
        
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
        
        
        
        ## Render output
        output$summary_header <- renderUI({
          
          HTML(paste(paste0("<h3> Metrics for ",actor_name,
                            " on ",fight_name," - ",
                            sub_spec," ",spec_image,"</h3>"), 
                     sep = '<br/>'))
        })
        
        output$summary_ignite <- renderUI({
          
          ### Munching ###
        
          Munch_NET_result <- (round(ignite_table$Munch_NET_2)*-1)
          
          str1 <- paste0( "- Expected ignite damage<sup>*</sup>: ",  prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential)),big.mark=",",scientific=FALSE))
          str2 <- paste0( "- Actual ignite damage dealt<sup>*</sup>: ",  prettyNum((round(ignite_table$Total_Ignite_Dmg_Dealt)),big.mark=",",scientific=FALSE))
          str3 <- paste0("- Ignite lost to (target) death<sup>1</sup>: ",  prettyNum(round(ignite_table$Ignite_tick_lost_dead2),big.mark=",",scientific=FALSE))
          str4 <- paste0(prettyNum((round(ignite_table$Total_Ignite_Dmg_Dealt)),big.mark=",",scientific=FALSE), " - ",
                                   prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential))- (round(ignite_table$Ignite_tick_lost_dead2)),big.mark=",",scientific=FALSE),
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
          
          str_min <- paste0( "- Lowest ignite tick: ",  prettyNum((min(ignite_table_debug$igniteSUB_resist)),big.mark=",",scientific=FALSE))
          str_summ1 <- paste0("- Expected ignite damage before target death<sup>*</sup>: ",
                              prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential)),big.mark=",",scientific=FALSE),
                               " - ",  
                               prettyNum(round(ignite_table$Ignite_tick_lost_dead2),big.mark=",",scientific=FALSE),
                               " = ",prettyNum((round(ignite_table$Total_Ignite_Dmg_Potential))- (round(ignite_table$Ignite_tick_lost_dead2)),big.mark=",",scientific=FALSE) )
        
          
          
          ignite_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_incinerate.jpg' height='20' width='20'/>"
          
          ## Final format
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
        
        output$summary_ignite2 <- renderUI({
          ignite_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_incinerate.jpg' height='20' width='20'/>"
          str3 <- paste0("- Ignite lost to (target) death<sup>1</sup>: ",  prettyNum(round(ignite_table$Ignite_tick_lost_dead2),big.mark=",",scientific=FALSE))
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
        
        
        output$summary_cast_1 <- renderUI({
          
          ### Delay gaps ###
          
          
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
            
          } else {str_delay_5 <- paste0("- Delays at 0ms: ", 
                                        nrow(casts_fb_pyro %>% 
                                               filter(delay == 0))) 
          str_alert <- ""
          
          }
          
          
          str_delay_6 <- paste0("- Delays at >0ms and <100ms: ",
                                nrow(casts_fb_pyro %>% filter(delay > 0 & delay < 100)))
          
          str_delay_7 <- paste0("- Delays at >=100ms and <300ms: ",
                                nrow(casts_fb_pyro %>% filter(delay >= 100 & delay < 300)))
          
          str_lb_clip <- paste0("- Living Bombs clipped<sup>2</sup>: ", nrow(debuff_table))
          
          casts_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/ability_hunter_pet_turtle.jpg' height='20' width='20'/>"
          
          
          HTML(paste(
            paste0("<h4> <b>",casts_img," Cast metrics (Main Target only)</b> </h4>"),
            paste0("<b>Ms<sup>3</sup> between Fireball and Pyroblast casts (<750ms):</b>"),
            str_delay_5,
            str_delay_6,
            str_delay_7,
            str_alert,
            "<br/",
            paste0("<b>Living Bomb metrics:</b>"),
            str_lb_clip,
            sep = '<br/>'))
          
        }) 
        
        
        output$summary_cast_2 <- renderUI({

          ### Delay gaps ###
          
          str_delay_1 <- paste0("- Avg. Delay: ", as.integer(mean(casts_fb_pyro$delay,na.rm=T))," ms")
          str_delay_2 <- paste0("- Max. Delay: ", max(casts_fb_pyro$delay,na.rm=T), " ms")
          str_delay_3 <- paste0("- Min Delay: ", min(casts_fb_pyro$delay,na.rm=T) , " ms")
          str_delay_4 <- paste0("- Total Insta-Pyros (after fireball): ", nrow(casts_fb_pyro))
          str_delay_5 <- paste0("- Median Delay: ", as.integer(median(casts_fb_pyro$delay,na.rm=T))," ms")
          
          
          HTML(paste("<br/",
                     "<br/",
                     "<br/",
                     str_delay_1,
                     str_delay_2,str_delay_3,str_delay_5, str_delay_4,

                     sep = '<br/>'))
          
          
        }) 
        
        output$everything_else_1 <- renderUI({
          
          
          
          str_total_pyro <- paste0("- Total Pyroblasts: ", nrow(pyro_n))
          str_insta_pyro <- paste0("- Total Insta-Pyros: ", (nrow(pyro_n)-nrow(pyro_hard_cast)))
          
          fblast_img <- "<img src='https://wow.zamimg.com/images/wow/icons/large/spell_fire_fireball.jpg' height='20' width='20'/>"
          
          str_pyro_canned <- paste0("- Pyroblasts cancelled/interrupted: ",nrow(pyro_interrupt)) 
          str_pyro_hard <- paste0("- Pyroblasts hard-cast: ",nrow(pyro_hard_cast))
          
          
          
          HTML(paste(
            "<br/",
            paste0("<h4> <b>",fblast_img," Cast Metrics (Encounter-wide, All Targets)</h4> </b>"),
            paste0("<b>Pyroblast metrics:</b>"),
            str_total_pyro,
            str_insta_pyro,
            str_pyro_hard,
            str_pyro_canned, 
            
            sep = '<br/>'))
          
        })
        
        output$everything_else_2 <- renderUI({
          
          
          str_fb <- paste0("- Fireball cancelled: ",nrow(fireball_interrupt)) 
          str_ffb <- paste0("- Frostfire Bolt cancelled: ",nrow(frostfirebolt_interrupt))
          
          if (sub_spec=="FFB"){
            str_mainspell <- str_ffb} else if(sub_spec=="TTW"){
              str_mainspell <- str_fb } else {
                str_mainspell <- ifelse(length(str_ffb)>=1, str_ffb,str_fb) 
              }
          
          
          str_hotstreak_n <- paste0("- # Hot Streaks (Buff): ",nrow(hot_streak_n))
          str_hotstreak_pyro <- paste0("- # Pyros per Hot Streak: ", round((as.integer(nrow(pyro_n))-as.integer(nrow(pyro_hard_cast)))/as.integer(nrow(hot_streak_n)), digits = 2))
          str_4pct8 <- paste0("- # 4pcT8 Pyros: ", round((as.integer(nrow(pyro_n))-as.integer(nrow(pyro_hard_cast)))-as.integer(nrow(hot_streak_n)), digits = 2))
          str_refreshpyro <- paste0("- Hot Streaks 'refreshed'<sup>5</sup>: ",nrow(hot_streak_ref))
          HTML(paste(
            "<br/",
            "<br/",
            paste0("<b>Other spells metrics:</b>"),
            str_mainspell,
            paste0("<b>Under testing:</b>"),
            str_hotstreak_n,
            str_hotstreak_pyro,
            str_4pct8,
            str_refreshpyro,
            sep = '<br/>'))
          
        }) 
        
        
        output$everything_else_3 <- renderUI({
          HTML(paste(
            "<br/",
            "<br/",
            paste0("<i><sup>1</sup> If a target dies before the 'stored' Ignite Damage has time to tick, any damage 'stored' in the Ignite is lost. This is NOT munching.</i>"), 
            paste0("<i><sup>2</sup> This is the # of Living Bombs refreshed BEFORE they had time to explode.</i>"), 
            paste0("<i><sup>3</sup> Milliseconds; 1,000 milliseconds = 1 second.</i>"), 
            paste0("<i><sup>4</sup> Unsure of what this means? Ask in Mage Discord (Link to your left)</i>"), 
            paste0("<i><sup>5</sup> Hot Streaks not fully used. Not fully consumed before it got 'refreshed'</i>"), 
            paste0("<i><sup>*</sup> This numbers are BEFORE partial resists.</i>"), 
            sep = '<br/>'))
          
        })
        
        
        output$extra_algalon <- renderUI({
          if(targetID_code$name[1]=="Algalon the Observer"){
            
            HTML(paste(paste0("<font color=\"#D78613\">Crits on Algalon while you are inside the black hole do NOT generate ignite - It will be considered as 'munching' in the calculation above"),
                       "<br/",
                       "<br/",
                       sep = '<br/>'))
          } else { HTML(paste(paste0("",
                                     sep = '<br/>'))) }
        })
        
        tryCatch({
        ### Leaderboard
        creds <- jsonlite::fromJSON(Sys.getenv("DRIVE_KEY"))
        options(googlesheets4.httr_oauth_cache = TRUE)
        gs4_auth(email=Sys.getenv("EMAIL_DRIVE"),token = creds)
        drive_auth(email=Sys.getenv("EMAIL_DRIVE"),token = creds)
        })
   
         leaderboard <- read_sheet(drive_get("leaderboard"))
         
         leaderboard[nrow(leaderboard)+1,1] <- as.character(extract_log_id(as.character(input$log_id)))  
         leaderboard[nrow(leaderboard),2] <- as.character(actor_name) 
         leaderboard[nrow(leaderboard),3] <- round(ignite_table$Munch_NET_2)*-1 
         leaderboard[nrow(leaderboard),4] <-  round((as.integer(nrow(pyro_n))-as.integer(nrow(pyro_hard_cast)))/as.integer(nrow(hot_streak_n)), digits = 2)
        leaderboard[nrow(leaderboard),5] <- targetID_code$name[1] 
         
       write_sheet(leaderboard,ss=as.character(Sys.getenv("SHEET_KEY")),sheet="leaderboard")
        

      } else {
        
        showModal(error_diag(paste0(error3,as.character(extract_log_id(as.character(input$log_id))),
                                    as.numeric(fight_temp), 
                                    as.numeric(actor_temp), 
                                    as.numeric(targetID_code$id[1])),3))
        
      }
       
    } else if(spec!="No Spec"){
      
      showModal(modalDialog(
        title = "Error 3",
        paste0("It looks like that character is ",spec," Mage on that log. If you think this is an error, contact Forge#0001 on discord or try refreshing"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
      
      
    } else { 
      showModal(modalDialog(
        title = "Error 4",
        paste0("It looks like that character has no data for that fight, or it is possible no talent data is available in the log for that fight. If you think this is an error, contact Forge#0001 on discord or try refreshing"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("OK")
        )
      ))
    }
    
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
          str_pyro_hot,str_pyro_hard_2,  creds$type, 

          sep = '<br/>'))
        
      })  
      
      
      
      
    }) 
    
  })
  
  
  
  
}
