% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Scaling.R
\name{Scaling}
\alias{Scaling}
\title{R6 class representing a (non-uniform) scaling}
\description{
A (non-uniform) scaling is given by a center, a direction vector,
and a scale factor.
}
\examples{
Q <- c(1,1); w <- c(1,3); s <- 2
S <- Scaling$new(Q, w, s)
# the center is mapped to itself:
S$transform(Q)
# any vector \code{u} parallel to the direction vector is mapped to \code{s*u}:
u <- 3*w
all.equal(s*u, S$transform(u) - S$transform(c(0,0)))
# any vector perpendicular to the direction vector is mapped to itself
wt <- 3*c(-w[2], w[1])
all.equal(wt, S$transform(wt) - S$transform(c(0,0)))


## ------------------------------------------------
## Method `Scaling$new`
## ------------------------------------------------

Scaling$new(c(1,1), c(1,3), 2)

## ------------------------------------------------
## Method `Scaling$getMatrix`
## ------------------------------------------------

S <- Scaling$new(c(1,1), c(2,3), 2)
P <- c(1,5)
S$transform(P)
S$getMatrix() \%*\% c(P,1)
}
\references{
R. Goldman,
\emph{An Integrated Introduction to Computer Graphics and Geometric Modeling}.
CRC Press, 2009.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{center}}{get or set the center}

\item{\code{direction}}{get or set the direction}

\item{\code{scale}}{get or set the scale factor}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Scaling-new}{\code{Scaling$new()}}
\item \href{#method-Scaling-print}{\code{Scaling$print()}}
\item \href{#method-Scaling-transform}{\code{Scaling$transform()}}
\item \href{#method-Scaling-getMatrix}{\code{Scaling$getMatrix()}}
\item \href{#method-Scaling-asAffine}{\code{Scaling$asAffine()}}
\item \href{#method-Scaling-scaleCircle}{\code{Scaling$scaleCircle()}}
\item \href{#method-Scaling-clone}{\code{Scaling$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-new"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-new}{}}}
\subsection{Method \code{new()}}{
Create a new \code{Scaling} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$new(center, direction, scale)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{center}}{a point, the center of the scaling}

\item{\code{direction}}{a vector, the direction of the scaling}

\item{\code{scale}}{a number, the scale factor}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{Scaling} object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{Scaling$new(c(1,1), c(1,3), 2)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-print"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-print}{}}}
\subsection{Method \code{print()}}{
Show instance of a \code{Scaling} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{ignored}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-transform"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-transform}{}}}
\subsection{Method \code{transform()}}{
Transform a point or several points by the reference scaling.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$transform(M)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{M}}{a point or a two-column matrix of points, one point per row}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-getMatrix"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-getMatrix}{}}}
\subsection{Method \code{getMatrix()}}{
Augmented matrix of the scaling.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$getMatrix()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A 3x3 matrix.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{S <- Scaling$new(c(1,1), c(2,3), 2)
P <- c(1,5)
S$transform(P)
S$getMatrix() \%*\% c(P,1)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-asAffine"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-asAffine}{}}}
\subsection{Method \code{asAffine()}}{
Convert the reference scaling to an \code{Affine} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$asAffine()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-scaleCircle"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-scaleCircle}{}}}
\subsection{Method \code{scaleCircle()}}{
Scale a circle. The result is an ellipse.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$scaleCircle(circ)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{circ}}{a \code{Circle} object}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
An \code{Ellipse} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Scaling-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Scaling-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Scaling$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
