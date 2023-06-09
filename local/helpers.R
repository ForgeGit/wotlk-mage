url <- "https://classic.warcraftlogs.com/api/v2"

WCL_API2_request <- function(request) {
  
  request <- jsonlite::toJSON(list(query=request),
                              auto_unbox=TRUE, 
                              bigint_as_char=F) 
  
  response <- POST(url,
                   add_headers("Content-Type" = "application/json",
                               "Authorization"= paste0("Bearer ",
                                                       token$access_token)),
                   body = request,
                   content_type_json(),
                   encode = "json")
  
  response_formated <- fromJSON(content(response, 
                                        as = "text",
                                        encoding="UTF-8"), 
                                bigint_as_char=TRUE)
  
  return(response_formated)
}

#########################################################################################
boss_list <- c(757, #Alga
               752, #Thorim
               755, #Vezax
               746  #Razorscale
) 


#########################################################################################


# token <- POST("https://www.warcraftlogs.com/oauth/token",
#               config = list(),
#               body = list(
#                 grant_type="client_credentials",
#                 client_id=Sys.getenv("warcralog_id_ignite"),
#                 client_secret=Sys.getenv("warcralog_secret_ignite"))) %>% 
#   content("parsed")
#########################################################################################
request_mage <-'{
    reportData {
        report(code: "%s") {
            masterData(translate: true) {
                actors(type: "player"){
          
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
            fights(killType: Encounters){
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
            killType:Encounters
            hostilityType:Friendlies
            sourceClass:"Mage"
            sourceID:%i
            fightIDs:%i
            startTime: 0
            endTime: 999999999999){
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
                  42937,42198, 42211, 42210, 42213, 42209, 42212,42208 # blizzards
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
         igniteSUB= ifelse(is.na(igniteSUB),0,igniteSUB), # Immunes fix
         
         igniteCUM =  cumsum(igniteADD),
         
         igniteDIM = cumsum(igniteSUB)
         
  ) %>%
  ungroup()%>%
  
  group_by(fight,
           sourceID,targetID) %>% 
  
  ## Is it the right ignite amount?
  
  dplyr::mutate(
    igniteREM = igniteCUM-igniteDIM,
    
    trueIgnite = ifelse(abilityGameID=="Ignite" &  
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
                                      "END",NA))),
    
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
            
            #Ignite_tick_lost_dead1 = last(igniteREM_IRL), #Does not consider 1st tick calc as valid in the new set of ignite, see example 
            Ignite_tick_lost_dead2 = last(igniteREM),
            
            # Basically formula here: https://github.com/ForgeGit/ignite_wotlk#part-3---ignite-munching-and-vomit-basic-interactions
            
           # Munch_NET_1 = Total_Ignite_Dmg_Potential - (Total_Ignite_Dmg_Dealt+Ignite_tick_lost_dead1),
            
            Munch_NET_2 = Total_Ignite_Dmg_Potential - (Total_Ignite_Dmg_Dealt+Ignite_tick_lost_dead2)
  ) 
  
  return(x)
  
}
