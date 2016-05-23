\name{scale_bar}
\alias{scale_bar}
\title{Scale bar and orrientation arrow
}
\description{
This functions draws a scale bar on a ggplot object, and optionally an orientation arrow.
}
\usage{
scale_bar(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon, lat}{
longitude and latitude of the bottom left point of the first rectangle to draw
}
  \item{distance_lon}{
length of each rectangle
}
  \item{distance_lat}{
width of each rectangle
}
  \item{distance_legend}{
distance between rectangles and legend texts
}
  \item{dist_unit}{
units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles)
}
  \item{rec_colour, rec_colour_2}{
colour of the rectangles (default to black for both)
}
  \item{rec2_fill, rec_fill_2}{
filling colour of the rectangles (default to white, and black, resp.)
}
  \item{legend_colour}{
legend colour (default to black)
}
  \item{legend_size}{
legend size (default to 3)
}
  \item{orientation}{
(boolean) if TRUE (default), adds an orientation arrow to the plot
}
  \item{arrow_length}{
length of the arrow (default to 500 km)
}
  \item{arrow_distance}{
distance between the scale bar and the bottom of the arrow (default to 300 km)
}
  \item{arrow_north_size}{
size of the "N" letter (default to 6)
}
}
\value{
Returns a list of layers to add the scale bar to a ggplot2 graphic.
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