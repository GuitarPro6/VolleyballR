circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                  y = center[2] + radius * sin(angles)))
}

theme_court = function(base_size = 16) {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "#f0f0f0"),
      plot.background = element_rect(fill = bg_color, color = bg_color),
      panel.background = element_rect(fill = bg_color, color = bg_color),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      legend.background = element_rect(fill = bg_color, color = bg_color),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

width = 53.5
height = 39
key_height = 39
inner_key_width = 0
outer_key_width = 39
three_point_radius = 0
three_point_side_radius = 0





court_points = data.frame(
  x = c(-1, -1, -51, -51, -1),
  y = c(-51, -100, -100, -51, -51),
  desc = "perimeter"
)

court_points = rbind(court_points , data.frame(
  x = c(-45.5, -45.5, -6.5, -6.5,-45.5),
  y = c(-90, -51, -51, -90, -90),
  desc = "inner_key"
))


#This will draw the dashed line
line_points = data.frame(
  x = c(-6.5, -45.5, -45.5, -6.5), y = -64, desc = "dottedLine")


center_points_horizontal = data.frame(
  x = c(-26.5,-25.5),
  y = c(-77,-77)
  
)

center_points_vertical = data.frame(
  x = c(-26,-26),
  y = c(-76.5, -77.5)
)

#foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
#foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
#foul_circle_bottom = filter(foul_circle, y  == 6.5) %>% mutate(desc = "foul_circle_bottom")

#hoop = circle_points(center = c(-19.5, -77), radius = 2, npoints = 360) 

#restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
# filter(y >= hoop_center_y) %>%
#mutate(desc = "restricted")

#three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
#short_three_circle = circle_points(center = c(0, hoop_center_y), radius = short_three_radius) %>% filter(y >= hoop_center_y)


#short_three_line = data.frame(
# x = c(three_point_side_radius, three_point_side_radius, short_three_circle$x, -three_point_side_radius, -three_point_side_radius),
#y = c(0, hoop_center_y, short_three_circle$y, hoop_center_y, 0),
#desc = "short_three_line"
#)

#court_without_three = rbind(court_points , foul_circle_top, foul_circle_bottom, restricted)

#court_points = rbind(court_without_three, three_point_line)
#court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))

#short_three_court_points = rbind(court_without_three, short_three_line)
#short_three_court_points = mutate(short_three_court_points , dash = (desc == "foul_circle_bottom"))


#This method draws the court, contains the coordinates that put limits on what can be drawn

court = ggplot() + geom_path(data = court_points,
                             aes(x = x, y = y, group = desc), 
                             size = 4,
                             color = "#999999") + 
  coord_fixed(ylim = c(-50, -100), xlim = c(-1, -50), ratio = 1) +
  theme_court(base_size = 22) + 
geom_path(data = line_points,aes(x = x, y = y), linetype = "solid", size = 2, color = "#999999") + 
geom_path(data = center_points_horizontal, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
geom_path(data = center_points_vertical, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1)