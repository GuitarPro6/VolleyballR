#This method generates the scatter chart based on the data, plots the base court plus all the points. 


generate_scatter_chart = function(shots) {
  shot_data <- shots[0,]
  flush.console()
  
  shot_data <- data.frame(-shots$loc_x, -shots$loc_y, shots$attack_flag)
  shots_data <- na.omit(shot_data, verbose = FALSE)
 

  head(shots_data$shots.attack_flag)
  base_court = court   
  if(nrow(shots_data) != 0){
   

  ggplot(court_points) + geom_path(
                       aes(court_points$x, court_points$y, group = desc),linetype = "solid", size = 2,
                       color = "#999999") + 
    coord_fixed(ylim = c(-50, -100), xlim = c(0, -50)) +
    theme_court(base_size = 22) + geom_path(data = line_points,aes(line_points$x,line_points$y),linetype = "solid", color = "#999999") + 
      geom_path(data = center_points_horizontal, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
      geom_path(data = center_points_vertical, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
      geom_point(data = shots_data,
      aes(x = shots_data$X.shots.loc_x, y = shots_data$X.shots.loc_y, color = shots_data$shots.attack_flag), alpha = 0.8, size = 2.8)  + scale_color_manual("", values = c(Kill = "#FDE725", Error = "#1F9D89", Block = "azure", Oball = "azure2", OBall = "azure3", BlockC = "azure4", None = "lightblue4"))
}
else{
  base_court
}

}

