% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_bkde2d.r
\name{geom_bkde2d}
\alias{geom_bkde2d}
\alias{stat_bkde2d}
\title{Contours from a 2d density estimate.}
\usage{
geom_bkde2d(
  mapping = NULL,
  data = NULL,
  stat = "bkde2d",
  position = "identity",
  bandwidth = NULL,
  range.x = NULL,
  lineend = "butt",
  contour = TRUE,
  linejoin = "round",
  linemitre = 1,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)

stat_bkde2d(
  mapping = NULL,
  data = NULL,
  geom = "density2d",
  position = "identity",
  contour = TRUE,
  bandwidth = NULL,
  grid_size = c(51, 51),
  range.x = NULL,
  truncate = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
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

\item{position}{Position adjustment, either as a string naming the adjustment
(e.g. \code{"jitter"} to use \code{position_jitter}), or the result of a call to a
position adjustment function. Use the latter if you need to change the
settings of the adjustment.}

\item{bandwidth}{the kernel bandwidth smoothing parameter. see
\code{\link[KernSmooth]{bkde2D}} for details. If \code{NULL},
it will be computed for you but will most likely not yield optimal
results. see \code{\link[KernSmooth]{bkde2D}} for details}

\item{range.x}{a list containing two vectors, where each vector contains the
minimum and maximum values of x at which to compute the estimate for
each direction. see \code{\link[KernSmooth]{bkde2D}} for details}

\item{lineend}{Line end style (round, butt, square).}

\item{contour}{If \code{TRUE}, contour the results of the 2d density
estimation}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

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

\item{...}{Other arguments passed on to \code{\link[ggplot2:layer]{layer()}}. These are
often aesthetics, used to set an aesthetic to a fixed value, like
\code{colour = "red"} or \code{size = 3}. They may also be parameters
to the paired geom/stat.}

\item{geom}{default geom to use with this stat}

\item{grid_size}{vector containing the number of equally spaced points in each
direction over which the density is to be estimated. see
\code{\link[KernSmooth]{bkde2D}} for details}

\item{truncate}{logical flag: if TRUE, data with x values outside the range
specified by range.x are ignored. see \code{\link[KernSmooth]{bkde2D}}
for details}
}
\description{
Perform a 2D kernel density estimation using \code{bkde2D} and display the
results with contours. This can be useful for dealing with overplotting
}
\details{
\if{html}{
A sample of the output from \code{geom_bkde2d()}:

\figure{geombkde2d01.png}{options: width="100\%" alt="Figure: geombkde2d01.png"}
}

\if{latex}{
A sample of the output from \code{geom_bkde2d()}:

\figure{geombkde2d01.png}{options: width=10cm}
}
}
\section{Computed variables}{

Same as \code{\link{stat_contour}}
}

\examples{
m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
       geom_point() +
       xlim(0.5, 6) +
       ylim(40, 110)

m + geom_bkde2d(bandwidth=c(0.5, 4))

m + stat_bkde2d(bandwidth=c(0.5, 4), aes(fill = ..level..), geom = "polygon")

# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsmall, aes(x, y)) +
       geom_bkde2d(bandwidth=c(0.5, 0.5), aes(colour = cut))
d

# If we turn contouring off, we can use use geoms like tiles:
d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "raster",
                aes(fill = ..density..), contour = FALSE)

# Or points:
d + stat_bkde2d(bandwidth=c(0.5, 0.5), geom = "point",
                aes(size = ..density..),  contour = FALSE)
}
\seealso{
\code{\link{geom_contour}} for contour drawing geom,
 \code{\link{stat_sum}} for another way of dealing with overplotting
}
