#Here we need to tell the program who the players are and what team they play for
library(readxl)
library(readr)
library(stringr)


fetch_players = function(season){
  
#  if(season == 16){
#    myPfile <- file.path("teams_players.xls") 
#  }
#  else if(season == 17){
#    myPfile <- file.path("teams_players_17.xls")
#  
#  }
#  else{
#    myPfile <- file.path("teams_players.xls") 
#  }
  first <- paste(season, ".xls", sep="")
  file_name <- paste("teams_players_", first, sep="")
  myPfile <- file.path(file_name) 
  
  players_data <- read_excel(myPfile, sheet = "PlayerIds", col_types = "text")


    
  
  players <- data.frame(players_data$NickName, stringsAsFactors = FALSE)
  
  
  
  
  players <- mutate(players,
                    name = as.character(players_data$NickName),
                    person_num = players_data$Num,
                    person_id = as.character(players_data$`DV-ID`),
                    team_id = as.character(players_data$`TEAMID`),
                    team_name = as.character(players_data$`Team Name`),
                    first_name = as.character(tolower(players_data$`First Name`)),
                    last_name = as.character(tolower(players_data$`Last Name`))
  )
  
  
  #Here we would need to add the column for league information
  
  #make them all lowercase
  available_players <- mutate(players, lower_name = tolower(players$name))
  
  #Order them
  available_players <- available_players[order(available_players$last_name),]
  
  
  #this is the vector that will represent the options for the player menu, give the names a unique ID
  player_options <- setNames(as.character(available_players$person_id),as.character(available_players$name))
  
  
  return(player_options)
}


fetch_teams = function(season){
#  if(season == 16){
#    myPfile <- file.path("teams_players.xls") 
#  }
#  else if(season == 17){
#    myPfile <- file.path("teams_players_17.xls")
#  }
#  else{
#  myPfile <- file.path("teams_players.xls") 
#  }
  
  first <- paste(season, ".xls", sep="")
  file_name <- paste("teams_players_", first, sep="")
  myPfile <- file.path(file_name) 
  
  team_data = read_excel(myPfile, sheet = "TeamIds")

  
  team_data_2 = cbind.data.frame("ID" = team_data$`Home Id`, "NAME" = team_data$NickName)
  # Here we need to choose the unique teamId's in the data set and match those with teamID in the excel file
  #Build file name from given season
  year <- paste(season, ".csv", sep="")
  file_name <- paste("Heatmap Data 20", year, sep="")
  myfile <- file.path(file_name) 
  vball_data <- read_csv(myfile, col_types = cols(.default = "c"))
  
  teams_to_include <- unique(vball_data$TeamId)
  
  
  opp_list_vec <- subset(team_data_2, team_data_2$ID %in% teams_to_include)
  
  opp_Vec <- setNames(as.character(opp_list_vec$ID), as.character(opp_list_vec$NAME))
  
  return(opp_Vec)
}


fetch_players_data = function(season){
  
#  if(l == "16"){
#    myPfile <- file.path("teams_players.xls") 
#  }
#  else if(l == "17"){
#    myPfile <- file.path("teams_players_17.xls")
#    
#  }
#  else{
#    myPfile <- file.path("teams_players.xls") 
#  }
  first <- paste(season, ".xls", sep="")
  file_name <- paste("teams_players_", first, sep="")
  myPfile <- file.path(file_name) 
  players_data =  read_excel(myPfile, sheet = "PlayerIds", col_types = "text")

  
  
  if(length(players_data) != 0){
    players = data.frame(players_data$NickName, stringsAsFactors = FALSE)
    
    
    
    
    players <- mutate(players,
                      name = as.character(players_data$NickName),
                      person_num = players_data$Num,
                      person_id = as.character(players_data$`DV-ID`),
                      team_id = as.character(players_data$`TEAMID`),
                      team_name = as.character(players_data$`Team Name`),
                      first_name = as.character(tolower(players_data$`First Name`)),
                      last_name = as.character(tolower(players_data$`Last Name`))
    )
    
    
    
    
    #Here we would need to add the column for league information
    
    #make them all lowercase
    available_players = mutate(players, lower_name = tolower(players$name))
    
    #Order them
    available_players <- available_players[order(available_players$last_name),]
    
  }
  else{
    available_players = vector()
  }
  
 
}

find_player_by_name = function(n,season) {

  available_players = fetch_players_data(season)
  filter(available_players, lower_name %in% tolower(n))
}


find_player_id_by_name = function(n, season) {
  
  find_player_by_name(n, season)$person_id
}

find_player_by_id = function(n, season){
  available_players = fetch_players_data(season)
  filter(available_players, person_id == n)
}



#Set the Default player fields 
default_player = find_player_by_name("B.Botkin", 18)

default_id = as.character(default_player$person_id)
default_position = as.character(default_player$person_pos)
default_team_id = as.numeric(default_player$team_id)
default_team_name = as.character(default_player$team_name)
file_vec <- list.files()
files_list <- grep("[0-9]{4}", file_vec, value = TRUE)
y_match <- gregexpr('[0-9]{4}',files_list)

years <- unique(regmatches(files_list,y_match))

year <- sort(as.numeric(years)-2000)
default_seasons = year
default_season = 18


#Method used to capitalize the first letter in a string, to be consistent with the data
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#Here we pull the photo if there is a valid address that doesn't contain too many unique items
player_photo_url = function(player_first_name, player_last_name, player_num, player_team) {
  if(length(player_team) > 1){
    paste0("UofU.png")
  }
  else if(player_team == "University of Utah"){
  paste0("http://www.utahutes.com/images/2016/8/16/",player_num,"_",simpleCap(player_first_name),"_",simpleCap(player_last_name),"_UUVLB16.jpg?width=300")
  }
  else if(player_team == "USC"){
    paste0("https://pbs.twimg.com/profile_images/775539670142758912/iN5iMkm7.jpg") 
  }
  else if(player_team == "WSU"){
    paste0("http://www.wsucougars.com/images/2016/7/1/",simpleCap(player_last_name),"",simpleCap(player_first_name),"2016C.jpg?width=300")
  }
  else if(player_team == "WASH"){
    paste0("http://www.gohuskies.com/images/2016/8/17/",simpleCap(player_first_name),"_",simpleCap(player_last_name),"_2016.jpg?width=300")
  }
  else if(player_team == "ORE"){
    paste0("http://www.goducks.com/images/2016/8/12/",simpleCap(player_last_name),"_",simpleCap(player_first_name),".jpg?width=300")
  }
  else if(player_team == "COLO"){
    paste0("http://www.cubuffs.com/images/2016/8/9/",simpleCap(player_last_name),"_",simpleCap(player_first_name),"_mug_2016.jpg?width=300")
  }
  else if(player_team == "UCLA"){
    paste0("http://www.uclabruins.com/images/2016/6/29/UCLA_Volleyball_3_color.jpg?width=300")
  }
  else if(player_team == "CAL"){
    paste0("https://pbs.twimg.com/profile_images/778281829304414208/6On0YAPr.jpg")
  }
  else if(player_team == "ARIZ"){
    paste0("http://www.arizonawildcats.com/images/2016/8/10/",simpleCap(player_last_name),"_",simpleCap(player_first_name),".jpg?width=300")
  }
  else if(player_team == "STAN"){
    paste0("https://pbs.twimg.com/profile_images/541406818795126785/E3eTh0wz.jpeg")
  }
  else if(player_team == "OSU"){
    paste0("http://www.osubeavers.com/images/2016/7/13/16_VB_",simpleCap(player_last_name),"",simpleCap(player_first_name),"_HS_58.jpg?width=300")
  }
  else if(player_team == "ASU"){
    paste0("http://www.thesundevils.com/images/2016/8/11/",player_num,"_",simpleCap(player_last_name),"_",simpleCap(player_first_name),"_copy.JPG?width=300")
  }
  else{
    paste0("UofU.png")
  }
}





