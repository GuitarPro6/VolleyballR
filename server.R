library(rdrop2)

shinyServer(function(input, output, session) {



 


  #Reactive Object that stores the current Player(s) selected by the user 
  current_player = reactive({
    req(input$Player_Filter, current_season())

    find_player_by_id(input$Player_Filter, current_season())
  })
  
  #Reactive Object that stores the current Team(s) selected by the user
  current_team = reactive({req(input$filter_select) 
    if(input$filter_select == "Team"){
    input$BTeam_Filter
    }
    else if(input$filter_select == "Against Team"){
      input$Team_Filter
    }
    
    })
  #vector used for league filter testing. Contains all pac 12 IDs
  teams = c("UOU","245", "246","247","248","249","250","251","252", 
            "254","255","257")
  #Reactive Object that stores the current Team(s) selected by the user
  current_league = reactive ({req(input$League_Filter)
    input$League_Filter


    })

  
  #Reactive Object that contains the list of available seasons to the user
  current_player_seasons = reactive({
    req(input$filter_select)
    
    #algorithm to extract two digit numbers from the file list. 
    as.numeric(c(17,16,15,14))
  })
  
  current_opponents = reactive({
    req(current_season())
    
    opponent_Vec = fetch_teams(current_season())
    
    opponent_Vec
    
  })
  
  current_player_list = reactive({
    req(current_season())
    
    available_player_list = fetch_players(current_season())
    
    available_player_list
    
    
  })
  
  #Reactive object that stores the current season selected by the user. A season must be selected at all times. 
  current_season = reactive({
    req(input$season)
    input$season
  })
  
  #Object that stores all the teams and IDs used in the team and opponenent filters
  #opponent_Vec = c( "All" = "", "UTAH"= "UOU","ASU" = 245, "OSU" = 246,"STAN" = 247, "ARIZ" = 248, "CAL" = 249, "UCLA" = 250, "COLO" = 251, "ORE" = 252, 
  #                            "WASH" = 254, "WSU" = 255, "USC" = 257, "LMU" = 366, "BYU" = 365, "POR" = 361, "GZGA" = 357, "UVU" = 354, "Seattle" = 351, "Bakersfield" = 348, "Denver" = 344, 
  #                            "Texas St" = 323, "Missouri" = 275, "Nevada" = 222, "Utah St." = 220, "Wyoming" = 219, "Fresno" = 212, "Boise State" = 211, "Illinois St" = 203, "Colorado St." = 213, "Rice" = 144, "Baylor" = 118, "UCR" = 114, "Cal Poly" = 108, "Ohio St" = 93,
  #                            "Portland St" = 75, "Idaho St" = 68, "Butler" = 57, "Pitt" = 20, "Tulsa" = 8, "Chic. St" = 356, "New Orleans" = 304, "USD" = 367, "CSULB" = 111, "Illinois" = 95, "Penn State" = 94, "Texas Tech" = 123, "Iowa" = 96, "New Mexico" = 218, "Abilene Christian" = 292, "Harvard" = 335, "Pepperdine" = 358, "Santa Clara" = 360, "UOP" = 363, "Hawaii" = 116, "Irvine" = 113, "North Iowa" = 209, 
  #                            "Creighton" = 58, "Oklahoma" = 125, "Maryland" = 105, "Northeastern" = 130, "Saint Mary's" = 359, "Southern Utah" = 70, "Minnesota" = 97, "West Virginia" = 120, "Purdue" = 103, "Grand Canyon" = 349, "James Madison" = 129, "American" = 259, "Virginia" = 26, "Kentucky" = 281, "Idaho" = 76, "Villanova" = 65, "Maryland Eastern" = 199, "George Washington" = 44, "Northern Illinois" = 167, "Coll. Of Charelston" = 127,
  #                            "Syracuse" = 25, "Nebraska" = 98, "South Dakota" = 346, "Northern Colorado" = 79, "Texas @ Austin" = 126, "Florida" = 280, "Murray State" = 239, "San Diego State" = 214, "UC Davis" = 112, "CSUN" = 110, "UNLV" = 217, "USF" = 362)
  #updates pdf rating based on insystem filter

  update_pdf_input = observe({
    req(input$filter_select)
    
    
    
    if (input$in_system_filter != "All") {
      selected_value = NULL
      updateSelectInput(session,
                        "pass_dig_free_rating_filter",
                        choices = c("All", "Perfect"="@", 
                                    "Good"="#", 
                                    "Medium"="+", 
                                    "Overpass" = "/",
                                    "Error" = "@",
                                    "Bad"="-"),
                        selected = selected_value)
    } 
    
    
    
  })
  
  
  shots_league = reactive({
    req(input$filter_select, current_league(), current_season())


    if(input$filter_select == "Against League"){
      ## Needs to have all the teams
      new_shots <- fetch_shots_ag_team_id_and_season(teams, current_season())
    }
    
    
    #}
  })
  #Reactive function that gets a data frame of shots 
  shots_team = reactive({
    req(input$filter_select, current_season(), current_team())
   # use_default_shots = input$Player_Filter == "ANA-ADO" && input$filter_select == "Player" 
    #if (use_default_shots) {
    # default_shots
    #} else {

        if(input$filter_select == "Team"){

          new_shots <- fetch_shots_by_team_id_and_season(current_team(), current_season())
          

      }
        else if(input$filter_select == "Against Team") {
          new_shots <- fetch_shots_ag_team_id_and_season(current_team(), current_season())
        }

      
    #}
  })
  
  shots = reactive({
    req(input$filter_select, current_season(), current_player())
    # use_default_shots = input$Player_Filter == "ANA-ADO" && input$filter_select == "Player" 
    #if (use_default_shots) {
    # default_shots
    #} else {
    if(input$filter_select == "Player"){
      
      new_shots <- fetch_shots_by_player_id_and_season(current_player()$person_id, current_season())
    }

    
    #}
  })
  

  #filters the shots
  #does not have InSystem filter, attack scenario filter or number of blockers filter yet
  filtered_shots <- reactive({
    req(input$filter_select)
    if(input$filter_select == "Player"){
    filter(shots(), is.null(input$attack_combo_filter) | attCombo %in% input$attack_combo_filter,
           is.null(input$attack_result_filter)| attResult %in% input$attack_result_filter,
           is.null(input$pass_dig_free_rating_filter) | pdfRate %in% input$pass_dig_free_rating_filter,
           is.null(input$attack_pace_filter) | attPace %in% input$attack_pace_filter,
           input$opponent_filter == "" | OppId %in% input$opponent_filter, 
           input$league_selector == "" | OppId %in% teams,
           is.null(input$pass_dig_free_filter) | pdf %in% input$pass_dig_free_filter,
           is.null(input$num_blockers_filter) | blockers %in% input$num_blockers_filter,
           is.null(input$rotation_filter) | rotation %in% input$rotation_filter,
           is.null(input$set_rating_filter) | setRate %in% input$set_rating_filter,
           is.null(input$set_call_filter) | setCall %in% input$set_call_filter,
           (length(input$set_zn) == 0) | setzn %in% input$set_zn,
           (input$attspeed_min == "" & input$attspeed_max == "") |(input$attspeed_max == "" & as.numeric(attSpeed) >= as.numeric(input$attspeed_min)) | (input$attspeed_min == "" & as.numeric(attSpeed) <= as.numeric(input$attspeed_max))
           | (as.numeric(attSpeed) >= as.numeric(input$attspeed_min) &  as.numeric(attSpeed) <= as.numeric(input$attspeed_max)),
           input$in_system_filter == "All" | (input$in_system_filter == "InSystem" & InSys == "TRUE") | (input$in_system_filter == "Out of System" & InSys == "FALSE")
    )
    }
    else if(input$filter_select == "Team"){
      print(length(input$set_zn))
         filter(shots_team(), is.null(input$attack_combo_filter) | attCombo %in% input$attack_combo_filter,
                          is.null(input$attack_result_filter)| attResult %in% input$attack_result_filter,
                          is.null(input$pass_dig_free_rating_filter) | pdfRate %in% input$pass_dig_free_rating_filter,
                          is.null(input$attack_pace_filter) | attPace %in% input$attack_pace_filter,
                          input$opponent_filter == "" | OppId %in% input$opponent_filter, 
                          input$league_selector == "" | TeamId %in% teams,
                          is.null(input$pass_dig_free_filter) | pdf %in% input$pass_dig_free_filter,
                          is.null(input$num_blockers_filter) | blockers %in% input$num_blockers_filter,
                          is.null(input$rotation_filter) | rotation %in% input$rotation_filter,
                          is.null(input$set_rating_filter) | setRate %in% input$set_rating_filter,
                          is.null(input$set_call_filter) | setCall %in% input$set_call_filter,
                          (current_season() < 18) || (length(input$set_zn) == 0) | setZone %in% input$set_zn,
                          (input$attspeed_min == "" & input$attspeed_max == "") |(as.numeric(attSpeed) >= input$attspeed_min & input$attspeed_max == "~") | (as.numeric(attSpeed) <= input$attspeed_max & input$attspeed_min == "~")
                          | ( input$attspeed_min >= as.numeric(attSpeed) & as.numeric(attSpeed) <= input$attspeed_max),
                          input$in_system_filter == "All" | (input$in_system_filter == "InSystem" & InSys == "TRUE") | (input$in_system_filter == "Out of System" & InSys == "FALSE")

      )
    }
    else if(input$filter_select == "Against Team"){
      
      
      filter(shots_team(), is.null(input$attack_combo_filter) | attCombo %in% input$attack_combo_filter,
             is.null(input$attack_result_filter)| attResult %in% input$attack_result_filter,
             is.null(input$pass_dig_free_rating_filter) | pdfRate %in% input$pass_dig_free_rating_filter,
             is.null(input$attack_pace_filter) | attPace %in% input$attack_pace_filter,
             input$opponent_filter == "" | TeamId %in% input$opponent_filter, 
             input$league_selector == "" | TeamId %in% teams,
             is.null(input$pass_dig_free_filter) | pdf %in% input$pass_dig_free_filter,
             is.null(input$num_blockers_filter) | blockers %in% input$num_blockers_filter,
             is.null(input$rotation_filter) | rotation %in% input$rotation_filter,
             is.null(input$set_rating_filter) | setRate %in% input$set_rating_filter,
             is.null(input$set_call_filter) | setCall %in% input$set_call_filter,
             (length(input$set_zn) == 0) | setzn %in% input$set_zn,
             (input$attspeed_min == "" & input$attspeed_max == "") | (as.numeric(attSpeed) >= input$attspeed_min & input$attspeed_max == "~") | (as.numeric(attSpeed) <= input$attspeed_max & input$attspeed_min == "~")
             | ( input$attspeed_min >= as.numeric(attSpeed) & as.numeric(attSpeed) <= input$attspeed_max),
             input$in_system_filter == "All" | (input$in_system_filter == "InSystem" & InSys == "TRUE") | (input$in_system_filter == "Out of System" & InSys == "FALSE")
      )
    }
    else if(input$filter_select == "Against League"){
      #Filter by league
      filter(shots_league(), is.null(input$attack_combo_filter) | attCombo %in% input$attack_combo_filter,
                          is.null(input$attack_result_filter)| attResult %in% input$attack_result_filter,
                          is.null(input$pass_dig_free_rating_filter) | pdfRate %in% input$pass_dig_free_rating_filter,
                          is.null(input$attack_pace_filter) | attPace %in% input$attack_pace_filter,
                          input$opponent_filter == "" | OppId %in% input$opponent_filter, 
                          input$League_Filter == "Pac-12" && OppId %in% teams, #For the case when pac-12 is selected and TeamId is in the league
                          input$league_selector == "" | TeamId %in% teams,
                          is.null(input$pass_dig_free_filter) | pdf %in% input$pass_dig_free_filter,
                          is.null(input$num_blockers_filter) | blockers %in% input$num_blockers_filter,
                          is.null(input$rotation_filter) | rotation %in% input$rotation_filter,
                          is.null(input$set_rating_filter) | setRate %in% input$set_rating_filter,
                          is.null(input$set_call_filter) | setCall %in% input$set_call_filter,
                          (length(input$set_zn) == 0) | setzn %in% input$set_zn,
                          (input$attspeed_min == "" & input$attspeed_max == "") | (as.numeric(attSpeed) >= input$attspeed_min & input$attspeed_max == "~") | (as.numeric(attSpeed) <= input$attspeed_max & input$attspeed_min == "~")
                          | ( input$attspeed_min >= as.numeric(attSpeed) & as.numeric(attSpeed) <= input$attspeed_max),
                          input$in_system_filter == "All" | (input$in_system_filter == "InSystem" & InSys == "TRUE") | (input$in_system_filter == "Out of System" & InSys == "FALSE"))
    }
    
    
    
  })
  
  hexbin_data = reactive({
    req(filtered_shots(), hexbinwidths(), input$hex_radius)
    
    calculate_hexbins_from_shots(filtered_shots(),
                                 binwidths = hexbinwidths(),
                                 min_radius_factor = input$hex_radius)
  })
  
  output$hexbinwidth_slider = renderUI({
    req(input$chart_type == "Hexagonal")
    
    sliderInput("hexbinwidth",
                "Hexagon Size (feet)",
                min = 0.5,
                max = 4,
                value = 1.5,
                step = 0.25)
  })
  
  hexbinwidths = reactive({
    req(input$hexbinwidth)
    rep(input$hexbinwidth, 2)
  })
  
  output$hex_radius_slider = renderUI({
    req(input$chart_type == "Hexagonal")
    
    sliderInput("hex_count",
                "Number of Bins",
                min = 1,
                max = 50,
                value = 15,
                step = 2)
  })
  
  alpha_range = reactive({
    req(input$chart_type == "Hexagonal")
    max_alpha = 0.98
    min_alpha = max_alpha - 0.25 
    c(min_alpha, max_alpha)
  })
  
  output$hex_metric_buttons = renderUI({
    req(input$chart_type == "Hexagonal")
    
    selectInput("hex_metric",
                "Hexagon Colors",
                choices = c("FG% vs. League Avg" = "bounded_fg_diff",
                            "FG%" = "bounded_fg_pct",
                            "Points Per Shot" = "bounded_points_per_shot"),
                selected = "bounded_fg_diff",
                selectize = FALSE)
  })
  
  

  
  
  output$heat_density_slider = renderUI({
    req(input$chart_type == "Heat Map")
    
    sliderInput("heat_density",
                "Color Density Adjustment",
                min = 0,
                max = 350,
                value = 200,
                step = 50)
  })
  
  
  
  shot_chart = reactive({
    req(filtered_shots(), input$filter_select, current_season(), input$chart_type)
    
    
    
    if (input$chart_type == "Hexagonal") {
      req(input$hex_metric, alpha_range())
      
      generate_hex_chart(
        filtered_shots(),
        input$hex_count,
        1
        
      )
    } else if (input$chart_type == "Scatter") {
      generate_scatter_chart(filtered_shots())
     
      
    } else if (input$chart_type == "Heat Map") {
      generate_heatmap_chart(filtered_shots(), input$heat_density)
      
    } else {
      stop("invalid chart type")
    }
  })
  
  #Main headers for the shot chart
  
  output$chart_header_player = renderText({
    req(input$filter_select)
    if(input$filter_select == "Player"){
      if(length(current_player()$name) == 1){
        paste0(current_player()$name)
      }
      else{
      paste0(current_player()$name, sep = ",")
      }
    }
    else if(input$filter_select == "Team"){
      team_names = subset(names(current_opponents()), current_opponents() %in% current_team())
      if(length(team_names) <= 1){
        paste0(team_names)
      }
      else{
        paste0(team_names, sep = ",")
      }
      
    }
    else if(input$filter_select == "Against Team"){
      team_names = subset(names(current_opponents()), current_opponents() %in% current_team())
      if(length(team_names) <= 1){
        paste0(team_names)
      }
      else{
      paste0(team_names, sep = ",")
      }
    }
    else if(input$filter_select == "Against League"){
      if(length(current_league()) <= 1){
        paste0(current_league())
      }
      else{
      paste0(current_league(), sep = ",")
      }
    }
  })
  
  #Currently displays the season
  output$chart_header_info = renderText({
    req(current_season())
    paste(current_season(), "Regular Season")
  })
  
  #Currently displays the player name
  output$chart_header_team = renderText({
    req(input$filter_select)
    if(input$filter_select == "Player"){
      if(length(current_player()$person_id) <= 1){
        
        paste0(current_player()$team_name)
        
      }
      else{
        paste0(current_player()$team_name, sep = ",")
      }
      
    }
   
   
  
  })
  
  #Data credit logo at the bottom
  output$shot_chart_footer = renderUI({
    req(shot_chart())
    
    tags$div(
      "Data via Pac-12",
      tags$br(),
      "Volleyballr"
    )
  })
  
  #Download Button
  
  output$downloadPlot <-downloadHandler(
    filename = function() { paste("'",current_season(),"'-'",input$chart_type,"'", '.png', sep='') },
    content = function(file) {
  
      if(input$filter_select == "Player"){

        if(length(filters_applied())> 0){
          result = filters_applied()

            N <- length(filtered_shots()$Y.Coord)
            
            kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
            main_title =paste0(paste(current_player()$name,sep = " " ,collapse = ", "), "  N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")
            
             title = paste0(result, collapse = "; ")
            finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-99, label=title, colour = "white", fontface = "bold", size = 5) 

        }
      else{

        N <- length(filtered_shots()$Y.Coord)
        kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
        main_title =paste0(paste(current_player()$name,sep = " " ,collapse = ", "), "  N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")
        finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none')  + annotate("text",x=-24, y=-49, label="", colour = "white", fontface = "bold", size = 5)
      }
      }
      else if(input$filter_select == "Team"){
        #Duplicate filter code here for the player plot

        if(length(filters_applied())> 0){
          result = filters_applied()

          N <- length(filtered_shots()$Y.Coord)
          
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
          main_title = paste0(current_team(), " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")
          
          title = paste0(result, collapse = "; ")
          finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-99, label=title, colour = "white", fontface = "bold", size = 5) 
          
        }
        else{
          N <- length(filtered_shots()$Y.Coord)
          print(N)
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100

          main_title = paste0(current_team(), " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")          

          finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none')
        }
        
     
      }
      else if(input$filter_select == "Against Team"){
        if(length(filters_applied())> 0){
          result = filters_applied()
          #Construct filters as seperate strings
          N <- length(filtered_shots()$Y.Coord)
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100

          title = paste0("N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")
          
          team_names = subset(names(current_opponents()), current_opponents() %in% current_team())
          
          main_title = paste0(team_names, " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")   
          
          title = paste0(result, collapse = "; ")
          
          finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-99, label=title, colour = "white", fontface = "bold", size = 5)
          
        }
        else{
          N <- length(filtered_shots()$Y.Coord)
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
          
          team_names = subset(names(current_opponents()), current_opponents() %in% current_team())

          main_title = paste0(team_names, " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")   
          
          title = paste0(result, collapse = "; ")
          
          finalPlot <- last_plot() + ggtitle(main_title)  + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-49, label=title, colour = "white", fontface = "bold", size = 5)
        }
 
      }
      else if(input$filter_select == "Against League"){
        if(length(filters_applied())> 0){
          result = filters_applied()
          N <- length(filtered_shots()$Y.Coord)
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
          
          team_names = subset(names(current_opponents()), current_opponents() %in% current_team())

          main_title = paste0(team_names, " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")   
          
          title = paste0(result, collapse = "; ")

          finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-49, label=title, colour = "white", fontface = "bold", size = 5)
          
        }
        else{
          
          #Here we need a vector to pull the names of the leagues
          N <- length(filtered_shots()$Y.Coord)
          kill_pct <- round(nrow(filtered_shots()[ which(filtered_shots()$attResult == "#"), ])/N, 2)*100
          
          team_names = subset(names(current_opponents()), current_opponents() %in% current_team())
          
          main_title = paste0(team_names, " N = ",N,"  (K% ", kill_pct,"%)", collapse = "; ")   
          
          title = paste0(result, collapse = "; ")
          
          finalPlot <- last_plot() + ggtitle(main_title) + theme(plot.title = element_text(size = 12, face = "bold"), legend.position='none') + annotate("text",x=-24, y=-49, label=title, colour = "white", fontface = "bold", size = 5)
        }
      }
      
      
      #filters < output$shot_filters_applied
    ggsave(file, finalPlot, device = "png", scale = 1, dpi = 300)
      #last_plot()
      #pdf(file, title="if you want any")
      #last_plot() # Or other graphics you want to have printed in your pdf
      #output$summary_stats_header
      #output$shot_filters_applied
      #dev.off()
    })
  
  #Player Photo
  output$player_photo = renderUI({
    req(input$filter_select)
    if(input$filter_select == "Player"){
    if (length(input$Player_Filter) == 0) {
      # if there is no player selected display the team
      tags$img(src = "UofU.png")
    } else {
      
      tags$img(src = player_photo_url(current_player()$first_name, current_player()$last_name, current_player()$person_num, current_player()$team_name), alt = "photo")
    }
    }
    else if(input$filter_select == "Team"){
      if (length(input$Team_Filter) == 0) {
        #If there is nothing selected, display the team logo
        tags$img(src = "UofU.png")
      } 
    }
    else if(input$filter_select == "Against Team"){
      if (length(input$Team_Filter) == 0) {
        #If there is nothing selected, display the team logo
        tags$img(src = "UofU.png")
      } else {
        tags$img(src = "UofU.png")
        #We should try and put photos up of the individual teams, but the user can select multiple teams so I don't know. 
        # tags$img(src = player_photo_url(current_player()$first_name, current_player()$last_name, current_player()$person_num, current_player()$team_name), alt = "photo")
      }
      
    }
    else if(input$filter_select == "Against League"){
      #League Filter Photo
      if (length(input$League_Filter) == 0) {
        # if there is no player selected display the team
        tags$img(src = "UofU.png")
        
      }
      else if("Pac-12" %in% input$League_Filter){
        tags$img(src = "pac12.png")
      }
      #Add other league photos here if necessary
    }
    
  })
#This is some code trying to figure out how to update the opponenet filter
#  output$opponent_filter = renderUI({
#    req(input$filter_select)
#    selectInput(inputId = "opponent_filter",
#                label = "Opponenet",
#                choices = c("None" = "", opponent_Vec),
#                selected = "",
#                multiple = TRUE,
#                selectize = FALSE)
    
#  })
  
 # output$opponent_filter = renderUI({
#    req(current_player_team_list())
#    selectInput(inputId = "opponent_filter",
#                label = "Opponenet",
#                choices = current_player_team_list(),
#                selected = default_season,
#                multiple = TRUE,
#                selectize = FALSE)
    
#  })
  

  
  output$court = renderPlot({
    req(shot_chart())
    withProgress({
      shot_chart()
    }, message = "Calculating...")
  }, height = 600, width = 800, bg = bg_color)
  
  #This function writes the current filters applied to the UI
  
  filters_applied = reactive({
    req(filtered_shots())
    filters = list()
    
    if (!(is.null(input$attack_combo_filter))) {
      filters[["Combo"]] = paste("Combo:", paste(input$attack_combo_filter, collapse = ", "))
    }
    
    if (!is.null(input$attack_result_filter)) {
      filters[["Att Result"]] = paste("Result:", paste(input$attack_result_filter, collapse = ", "))
    }
    
    if (!is.null(input$pass_dig_free_rating_filter)) {
      filters[["Rating"]] = paste("Rating:", paste(input$pass_dig_free_rating_filter, collapse = ", "))
    }
    
    if (!is.null(input$attack_pace_filter)) {
      filters[["Pace"]] = paste("Pace:", paste(input$attack_pace_filter, collapse = ','))
    }
    if (!(input$opponent_filter == "")) {
      LIST = names(current_opponents()[which(current_opponents() %in% input$opponent_filter)])
      filters[["Opponent"]] = paste("Opp:",paste(LIST, collapse = " "))

    }
    if (!(input$league_selector == "")) {
      LIST = input$league_selector
      filters[["League"]] = paste("League:",paste(LIST))
      
    }
    
    if (!is.null(input$pass_dig_free_filter)) {
      filters[["PDF"]] = paste("P-D-F:", paste(input$pass_dig_free_filter, collapse = ','))
    
    }
    if(!(input$in_system_filter == "All")){
      filters[["InSystem"]] = paste("InSys:", paste(input$in_system_filter, collapse = ','))
    }
    
    if(!is.null(input$num_blockers_filter)){
      filters[["Blockers"]] = paste("# Blockers:", paste(input$num_blockers_filter, collapse = ","))
    }
    
    if(!is.null(input$rotation_filter)){
      filters[["Rotation"]] = paste("Rotation #:", paste(input$rotation_filter, collapse = ","))
    }
    if(!is.null(input$set_rating_filter)){
      filters[["SetRate"]] = paste("Set Rate:", paste(input$set_rating_filter, collapse = ","))
    }
    if(!is.null(input$set_rating_filter)){
      filters[["Set Call"]] = paste("Set Call:", paste(input$set_call_filter, collapse = ","))
    }
    if(!is.null(input$set_zn)){
      filters[["Set Zone"]] = paste("Set Zone:", paste(input$set_zn, collapse = ","))
    }
    if(!(input$attspeed_min == "")){
      filters[["Speed Min"]] = paste("Speed Min:", input$attspeed_min)
    }
    if(!(input$attspeed_max == "")){
      filters[["Speed Max"]] = paste("Speed Max:", input$attspeed_max)
    }

    
    filters
  })
  
  #The filters applied form a vector of lists that are reactive to the user selecting different filters. 
  output$shot_filters_applied = renderUI({
    req(length(filters_applied()) > 0)
    
    div(class = "shot-filters",
        tags$h5("Shot Filters Applied"),
        lapply(filters_applied(), function(text) {
          div(text)
        })
    )
  })
  
#The player filter that opens when the user selects 'Player' from 'filter-select'
#This filter can select multiple players at once. 
  output$Player_Filter = renderUI({
    req(input$filter_select == "Player")
    
    selectInput(inputId = "Player_Filter",
                           label = "Player",
                          choices = c("None" = "", current_player_list()),
                         selected = default_player$person_id,
                        multiple = TRUE,
                        selectize = TRUE)
  })
  
  
#The team filter that opens when the user selects 'Team' from 'filter select'
#This allows the user to select the team for which the attacks have been AGAINST, not from. 
 output$Team_Filter = renderUI({
  req(input$filter_select == "Against Team")
  
   selectInput(inputId = "Team_Filter",
                label = "Team",
                choices = c("None" = "" ,fetch_teams(current_season())),
                selected = "", 
                multiple = TRUE,
                selectize = TRUE)
})
 
 #The "By Team" filter
 output$BTeam_Filter = renderUI({
   req(input$filter_select == "Team")
   
   selectInput(inputId = "BTeam_Filter",
               label = "Team",
               choices = c("None" = "",fetch_teams(current_season())),
               selected = "", 
               multiple = TRUE,
               selectize = TRUE)
 })
 
 #Needs to contain all the correct league names
output$League_Filter = renderUI({
  req(input$filter_select == "Against League")
  
  selectInput(inputId = "League_Filter",
              label = "League",
              choices = c("None", "Pac-12", "Big-10"),
              selected = "Pac-12",
              multiple = TRUE,
              selectize = TRUE)
              
              
  
  
})

output$opponent_filter = renderUI({
  req(current_opponents())
  
  selectInput(inputId = "opponent_filter",
              label = "VS Opponent",
              choices = c( "All" = "", current_opponents()),
              selected = "", 
              multiple = TRUE,
              selectize = FALSE)
  
  
})
  

  
  
  
  
  #output$summary_stats_header = renderText({
  #  req(current_player()$name)
  #  paste(current_player()$name, current_season())
  #})
  
#  output$summary_stats = renderUI({
#    req(filtered_shots(), shots())
#    req(nrow(filtered_shots()) > 0)
    #Need to work on the summary stats    
#    player_zone = filtered_shots() %>%
#      group_by(pdfRate) %>%
#      summarize(p = count(filtered_shots()$pdf == 'p'))
    #                fga = n(),
    #                pct = mean(shot_made_numeric),
    #                pct_as_text = fraction_to_percent_format(pct),
    #                points_per_shot = mean(shot_value * shot_made_numeric)) %>%
    #      arrange(desc(fga), desc(fgm))
    
    #    league_zone = shots()$league_averages %>%
    #      group_by(shot_zone_basic) %>%
    #      summarize(lg_fgm = sum(fgm),
    #                lg_fga = sum(fga),
    #               lg_pct = lg_fgm / lg_fga,
    #                lg_pct_as_text = fraction_to_percent_format(lg_pct),
    #                lg_points_per_shot = round(mean(shot_value * lg_pct), 2))
    
    #    merged = inner_join(player_zone, league_zone, by = "shot_zone_basic")
    
    #    overall = summarize(merged,
    #                        total_fgm = sum(fgm),
    #                        total_fga = sum(fga),
    #                        pct = total_fgm / total_fga,
    #                        pct_as_text = fraction_to_percent_format(pct),
    #                        points_per_shot = sum(points_per_shot * fga) / sum(fga),
    #                        lg_pct = sum(lg_fgm) / sum(lg_fga),
    #                        lg_pct_as_text = fraction_to_percent_format(lg_pct),
    #                        lg_points_per_shot = sum(lg_points_per_shot * lg_fga) / sum(lg_fga)
    #    )
    
 #   html = list(div(class = "row headers",
#                    span(class = "col-xs-4 col-md-3 zone-label", "Zone"),
#                    span(class = "col-xs-2 col-md-1 numeric", "FGM"),
#                    span(class = "col-xs-2 col-md-1 numeric", "FGA"),
#                    span(class = "col-xs-2 col-md-2 numeric", "FG%"),
#                    span(class = "col-xs-2 col-md-1 numeric", "Lg FG%"),
#                    span(class = "hidden-xs hidden-sm col-md-2 numeric", "Pts/Shot"),
#                    span(class = "hidden-xs hidden-sm col-md-1 numeric", "Lg Pts/Shot")
#    ))
    
#    for (i in 1:nrow(merged)) {
#      html[[i + 2]] = div(class = paste("row", ifelse(i %% 2 == 0, "even", "odd")),
#                          span(class = "col-xs-4 col-md-3 zone-label", merged$shot_zone_basic[i]),
#                          span(class = "col-xs-2 col-md-1 numeric", merged$fgm[i]),
#                          span(class = "col-xs-2 col-md-1 numeric", merged$fga[i]),
#                          span(class = "col-xs-2 col-md-2 numeric", merged$pct_as_text[i]),
#                          span(class = "col-xs-2 col-md-1 numeric", merged$lg_pct_as_text[i]),
#                          span(class = "hidden-xs hidden-sm col-md-2 numeric", round(merged$points_per_shot[i], 2)),
#                          span(class = "hidden-xs hidden-sm col-md-1 numeric", round(merged$lg_points_per_shot[i], 2))
#      )
#    }
#    
#    html[[length(html) + 1]] = div(class = "row overall",
#                                   span(class = "col-xs-4 col-md-3 zone-label", "Overall"),
#                                   span(class = "col-xs-2 col-md-1 numeric", overall$total_fgm),
#                                   span(class = "col-xs-2 col-md-1 numeric", overall$total_fga),
#                                   span(class = "col-xs-2 col-md-2 numeric", overall$pct_as_text),
#3                                   span(class = "col-xs-2 col-md-1 numeric", overall$lg_pct_as_text),
#                                   span(class = "hidden-xs hidden-sm col-md-2 numeric", round(overall$points_per_shot, 2)),
#                                   span(class = "hidden-xs hidden-sm col-md-1 numeric", round(overall$lg_points_per_shot, 2))
#    )
    
 #   html
#  })
})
