\name{create_orientation_arrow}
\alias{create_orientation_arrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creates a list containing coordinates for the orientation arrow. (Internal use.)
}
\description{
Using an object returned by the scale_bar() function, returns a list with all necessary coordinates to plot the North arrow.
}
\usage{
create_orientation_arrow(scale_bar, length, distance = 1, dist_units = "km")
}
\arguments{
  \item{scale_bar}{
result of create_scale_bar()
}
  \item{length}{
desired length of the arrow
}
  \item{distance}{
distance between legend rectangles and the bottom of the arrow
}
  \item{dist_units}{
units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
}
}
\value{
Returns a list containing :
\item{res}{coordinates to draw an arrow}
\item{coords_n}{coordinates of the middle of the arrow (where the "N" will be plotted)}
}
\author{
Ewen Gallic.
}