% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Circle.R
\name{maxAreaInscribedCircle}
\alias{maxAreaInscribedCircle}
\title{Maximum area circle inscribed in a convex polygon}
\usage{
maxAreaInscribedCircle(points, verbose = FALSE)
}
\arguments{
\item{points}{the vertices of the polygon in a two-columns matrix; their
order has no importance, since the procedure takes the convex hull of
these points (and does not check the convexity)}

\item{verbose}{argument passed to \code{\link[CVXR:psolve]{psolve}}}
}
\value{
A \code{Circle} object. The status of the optimization problem
  is given as an attribute of this circle. A warning is thrown if it is
  not optimal.
}
\description{
Computes the circle inscribed in a convex polygon with
  maximum area. This is the so-called \emph{Chebyshev circle}.
}
\examples{
library(PlaneGeometry)
hexagon <- rbind(
  c(-1.7, -1),
  c(-1.4, 0.4),
  c(0.3, 1.3),
  c(1.7, 0.6),
  c(1.3, -0.3),
  c(-0.4, -1.8)
)
opar <- par(mar = c(2, 2, 1, 1))
plot(NULL, xlim=c(-2, 2), ylim=c(-2, 2), xlab = NA, ylab = NA, asp = 1)
points(hexagon, pch = 19)
polygon(hexagon)
circ <- maxAreaInscribedCircle(hexagon)
draw(circ, col = "yellow2", border = "blue", lwd = 2)
par(opar)
# check optimization status:
attr(circ, "status")
}
\seealso{
\code{\link{maxAreaInscribedEllipse}}
}
