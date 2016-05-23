\name{create_scale_bar}
\alias{create_scale_bar}
\title{
Creates a list containing coordinates for the orientation arrow. (Internal use.)
}
\description{
Using an object returned by the scale_bar() function, returns a list with all necessary coordinates to plot the scale bar.
}
\usage{
create_scale_bar(lon, lat, distance_lon, distance_lat, distance_legend, dist_units = "km")
}
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
  \item{dist_units}{
units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles)
}
}
\value{
Return a list whose elements are :
\item{rectangle}{a data.frame containing the coordinates to draw the first rectangle}
\item{rectangle2}{a data.frame containing the coordinates to draw the second rectangle}
\item{legend}{a data.frame containing the coordinates of the legend texts, and the texts as well}
}
\author{
Ewen Gallic
}
