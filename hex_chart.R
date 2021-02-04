generate_hex_chart = function(shots, metric = 15, alpha){
  ggplot(court_points) + geom_path(
    aes(court_points$x, court_points$y, group = desc),linetype = "solid", size = 2,
    color = "#999999") + 
    coord_fixed(ylim = c(-50, -100), xlim = c(0, -50)) +
    theme_court(base_size = 22) + geom_path(data = line_points,aes(line_points$x,line_points$y),linetype = "solid", color = "#999999") + 
    geom_path(data = line_points,aes(x = x, y = y, linetype = "dash"), color = '#999999') +
    geom_path(data = center_points_horizontal, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
    geom_path(data = center_points_vertical, aes(x = x, y = y), linetype = "solid", color = "#999999", size = 1) +
    stat_binhex(data = shots, aes(x = -shots$loc_x, y = -shots$loc_y), na.rm = TRUE, bins = as.numeric(metric))  + scale_fill_gradientn(colours=c("red", "yellow","white"), guide = guide_colorbar(barwidth = 15))  
    #scale_colour_gradientn("Shot frequency    ",
    #                       limits = c(0, 1),
    #                       breaks = c(0, 1),
    #                       labels = c("lower", "higher"),
    #                       colours = c("red", "white"),
    #                       guide = guide_colorbar(barwidth = 15)) + 
    #theme(legend.text = element_text(size = rel(1.6)))
 
    
    
    
    
    #   geom_polygon(data = hex_data$hex_data,
    #             aes_string(x = "adj_x", y = "adj_y", group = "hexbin_id",
    #                        fill = metric, alpha = "hex_attempts"),
    #             size = 0) +
    #     scale_fill_gradientn(paste0(fill_label, "   "),
    #                          colors = viridis_colors,
    #                          limit = fill_limit,
    #                          labels = label_formatter,
    #                          guide = guide_colorbar(barwidth = 15)) +
    #     scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    #     theme(legend.text = element_text(size = rel(0.6)))
}
