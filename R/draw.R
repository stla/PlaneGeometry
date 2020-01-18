#' @title Draw a geometric object
#'
#' @description Draw a geometric object on the current plot.
#'
#' @param x geometric object (\code{Triangle}, \code{Circle} or \code{Line})
#' @param ... arguments passed to \code{\link{lines}} for a \code{Triangle}
#' object, to \code{\link[plotrix]{draw.circle}} for a \code{Circle} object,
#' general graphical parameters for a \code{Line} object
#' @examples t <- Triangle$new(c(0,0), c(1,0), c(0.5,sqrt(3)/2))
#' plot(0, 0, type="n", asp = 1, xlim = c(0,2), ylim = c(0,2),
#'      xlab = NA, ylab = NA)
#' draw(t, col = "blue", lwd = 2)
#' draw(t$rotate(90, t$C), col = "green", lwd = 2)
#' @export
draw <- function(x, ...){
  UseMethod("draw")
}

#' @rdname draw
#' @export
draw.Triangle <- function(x, ...){
  A <- x$A; B <- x$B; C <- x$C
  lines(rbind(A,B,C,A), ...)
}

#' @rdname draw
#' @importFrom plotrix draw.circle
draw.Circle = function(x, ...) {
  # do some magic
}

#' @rdname draw
draw.Line = function(x, ...) {
  # do some magic
}
