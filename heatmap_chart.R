generate_heatmap_chart = function(shots, size) {

    base_court = court    

   
  base_court +
    stat_density_2d(
      data = shots,
      aes(x = -loc_x, y = -loc_y,
          fill = ..density..),
      geom = "raster", contour = FALSE, interpolate = TRUE, n = size
    ) +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc), show.legend = FALSE,
              color = '#999999') +
    geom_path(data = center_points_horizontal, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
    geom_path(data = center_points_vertical, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
    scale_fill_gradientn(colors = inferno_colors, guide = FALSE ) +
    scale_colour_gradientn("Shot frequency    ",
                           limits = c(0, 1),
                           breaks = c(0, 1),
                           labels = c("lower", "higher"),
                           colours = inferno_colors,
                           guide = guide_colorbar(barwidth = 15)) + geom_path(data = line_points,aes(x = x, y = y, linetype = "dash"), show.legend = FALSE, color = '#999999') + guides(linetype=FALSE) + 
                            theme(legend.text = element_text(size = rel(0.6)))
}
#library(RColorBrewer)
#xy <- data.frame(raw_shots_data$X.Coord, raw_shots_data$Y.Coord)
#p <- hexbin(x = raw_shots_data$X.Coord, y = raw_shots_data$Y.Coord)
#m <- data.frame(p) 

#rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
#r <- rf(32)
#plot(p, colramp=rf)#

#d <- ggplot(raw_shots_data, aes(raw_shots_data$X.Coord, raw_shots_data$Y.Coord))
#d+ stat_binhex()
