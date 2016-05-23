\name{legendMap-package}
\alias{legendMap-package}
\alias{legendMap}
\docType{package}
\title{
\packageTitle{legendMap}
}
\description{
\packageDescription{legendMap}
}
\details{

\packageDESCRIPTION{legendMap}
\packageIndices{legendMap}
Add a North arrow and or a scale bar for maps created with ggplot2.
}
\author{
\packageAuthor{legendMap}

Maintainer: \packageMaintainer{legendMap}
}
\examples{
# United States map
usa_map <- map_data("state")
P <- ggplot() + geom_polygon(data = usa_map, aes(x = long, y = lat, group = group)) + coord_map()

# Let us add the scale bar (without the North arrow)
P + scale_bar(lon = -130, lat = 26, 
              distance_lon = 500, distance_lat = 100, distance_legend = 200, 
              dist_unit = "km", orientation = FALSE)

# With the North arrow
P <- P + scale_bar(lon = -130, lat = 26,
                   distance_lon = 500, distance_lat = 100,
                   distance_legend = 200, dist_unit = "km")

# Modifying the theme a bit
P + theme(panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
          panel.background = element_rect(fill = NA, colour = NA), axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), axis.title = element_blank(),
          rect = element_blank(),
          plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))

# France map
france_map <- map_data("france")
P_fr <- ggplot() + geom_polygon(data = france_map, aes(x = long, y = lat, group = group)) + coord_map()

# Let us add the scale bar
P_fr <- P_fr + scale_bar(lon = -5, lat = 42.5, 
                         distance_lon = 100, distance_lat = 20, 
                         distance_legend = 40, dist_unit = "km", 
                         arrow_length = 100, arrow_distance = 60, arrow_north_size = 6)

# Modifying the theme a bit
P_fr + theme(panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
             panel.background = element_rect(fill = NA, colour = NA), axis.text.x = element_blank(),
             axis.text.y = element_blank(), axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(), axis.title = element_blank(),
             rect = element_blank(),
             plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
}
