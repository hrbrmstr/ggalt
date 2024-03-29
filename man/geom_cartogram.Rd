% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_cartogram.r
\name{geom_cartogram}
\alias{geom_cartogram}
\title{Map polygons layer enabling the display of show statistical information}
\usage{
geom_cartogram(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  ...,
  map,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
}
\arguments{
\item{mapping}{Set of aesthetic mappings created by \code{\link[ggplot2:aes]{aes()}}. If specified and
\code{inherit.aes = TRUE} (the default), it is combined with the default mapping
at the top level of the plot. You must supply \code{mapping} if there is no plot
mapping.}

\item{data}{The data to be displayed in this layer. There are three
options:

If \code{NULL}, the default, the data is inherited from the plot
data as specified in the call to \code{\link[ggplot2:ggplot]{ggplot()}}.

A \code{data.frame}, or other object, will override the plot
data. All objects will be fortified to produce a data frame. See
\code{\link[ggplot2:fortify]{fortify()}} for which variables will be created.

A \code{function} will be called with a single argument,
the plot data. The return value must be a \code{data.frame}, and
will be used as the layer data. A \code{function} can be created
from a \code{formula} (e.g. \code{~ head(.x, 10)}).}

\item{stat}{The statistical transformation to use on the data for this
layer, either as a \code{ggproto} \code{Geom} subclass or as a string naming the
stat stripped of the \code{stat_} prefix (e.g. \code{"count"} rather than
\code{"stat_count"})}

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{map}{Data frame that contains the map coordinates.  This will
typically be created using \code{\link{fortify}} on a spatial object.
It must contain columns \code{x}, \code{long} or \code{longitude},
\code{y}, \code{lat} or \code{latitude} and \code{region} or \code{id}.}

\item{na.rm}{If \code{FALSE}, the default, missing values are removed with
a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{logical. Should this layer be included in the legends?
\code{NA}, the default, includes if any aesthetics are mapped.
\code{FALSE} never includes, and \code{TRUE} always includes.
It can also be a named logical vector to finely select the aesthetics to
display.}

\item{inherit.aes}{If \code{FALSE}, overrides the default aesthetics,
rather than combining with them. This is most useful for helper functions
that define both data and aesthetics and shouldn't inherit behaviour from
the default plot specification, e.g. \code{\link[ggplot2:borders]{borders()}}.}
}
\description{
This replicates the old behaviour of \code{geom_map()}, enabling specifying of
\code{x} and \code{y} aesthetics.
}
\section{Aesthetics}{

\code{geom_cartogram} understands the following aesthetics (required aesthetics are in bold):
\itemize{
  \item \code{map_id}
  \item \code{alpha}
  \item \code{colour}
  \item \code{fill}
  \item \code{group}
  \item \code{linetype}
  \item \code{size}
  \item \code{x}
  \item \code{y}
}
}

\examples{
\dontrun{
# When using geom_polygon, you will typically need two data frames:
# one contains the coordinates of each polygon (positions),  and the
# other the values associated with each polygon (values).  An id
# variable links the two together

ids <- factor(c("1.1", "2.1", "1.2", "2.2", "1.3", "2.3"))

values <- data.frame(
  id = ids,
  value = c(3, 3.1, 3.1, 3.2, 3.15, 3.5)
)

positions <- data.frame(
  id = rep(ids, each = 4),
  x = c(2, 1, 1.1, 2.2, 1, 0, 0.3, 1.1, 2.2, 1.1, 1.2, 2.5, 1.1, 0.3,
  0.5, 1.2, 2.5, 1.2, 1.3, 2.7, 1.2, 0.5, 0.6, 1.3),
  y = c(-0.5, 0, 1, 0.5, 0, 0.5, 1.5, 1, 0.5, 1, 2.1, 1.7, 1, 1.5,
  2.2, 2.1, 1.7, 2.1, 3.2, 2.8, 2.1, 2.2, 3.3, 3.2)
)

ggplot() +
  geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions)

ggplot() +
  geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions) +
  geom_cartogram(data=values, map=positions, aes(fill = value, map_id=id))

ggplot() +
  geom_cartogram(aes(x, y, map_id = id), map = positions, data=positions) +
  geom_cartogram(data=values, map=positions, aes(fill = value, map_id=id)) +
  ylim(0, 3)

# Better example
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)

if (require(maps)) {

  states_map <- map_data("state")

  ggplot() +
    geom_cartogram(aes(long, lat, map_id = region), map = states_map, data=states_map) +
    geom_cartogram(aes(fill = Murder, map_id = state), map=states_map, data=crimes)

  last_plot() + coord_map("polyconic")

  ggplot() +
    geom_cartogram(aes(long, lat, map_id=region), map = states_map, data=states_map) +
    geom_cartogram(aes(fill = value, map_id=state), map = states_map, data=crimesm) +
    coord_map("polyconic") +
    facet_wrap( ~ variable)
}
}
}
