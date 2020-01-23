#' @title Draw a geometric object
#'
#' @description Draw a geometric object on the current plot.
#'
#' @param x geometric object (\code{Triangle}, \code{Circle} or \code{Line})
#' @param ... arguments passed to \code{\link{lines}} for a \code{Triangle}
#' object, to \code{\link[DescTools]{DrawCircle}} for a \code{Circle} object,
#' to \code{\link[DescTools]{DrawEllipse}} for an \code{Ellipse} object,
#' general graphical parameters for a \code{Line} object
#' @examples # open new plot window
#' plot(0, 0, type="n", asp = 1, xlim = c(0,2.5), ylim = c(0,2.5),
#'      xlab = NA, ylab = NA)
#' grid()
#' # draw a triangle
#' t <- Triangle$new(c(0,0), c(1,0), c(0.5,sqrt(3)/2))
#' draw(t, col = "blue", lwd = 2)
#' draw(t$rotate(90, t$C), col = "green", lwd = 2)
#' # draw a circle
#' circ <- t$incircle()
#' draw(circ, col = "orange", border = "brown", lwd = 2)
#' # draw an ellipse
#' S <- Scaling$new(circ$center, direction = c(2,1), scale = 2)
#' draw(S$scaleCircle(circ), border = "grey", lwd = 2)
#' # draw a line
#' l <- Line$new(c(1,1), c(1.5,1.5), FALSE, TRUE)
#' draw(l, col = "red", lwd = 2)
#' perp <- l$perpendicular(c(2,1))
#' draw(perp, col = "yellow", lwd = 2)
#' @export
draw <- function(x, ...){
  UseMethod("draw")
}

#' @rdname draw
#' @export
draw.Triangle <- function(x, ...){
  A <- x$A; B <- x$B; C <- x$C
  lines(rbind(A,B,C,A), ...)
  invisible()
}

#' @rdname draw
#' @importFrom DescTools DrawCircle
#' @export
draw.Circle = function(x, ...) {
  center <- x$center
  DrawCircle(center[1L], center[2L], r.out = x$radius,
             theta.1 = 0, theta.2 = 2*pi, plot = TRUE, ...)
}

#' @rdname draw
#' @importFrom DescTools DrawArc
#' @export
draw.Arc = function(x, ...) {
  center <- x$center; r <- x$radius
  # if((x$alpha1 - x$alpha2) %% pi == 0){
  #   eialpha1 <- c(cos(x$alpha1), sin(x$alpha1))
  #   A <- center + r*eialpha1; B <- center - r*eialpha1
  #   draw(Line$new(A, B, FALSE, FALSE), ...) # NIMP !!
  # }else{
    DrawArc(center[1L], center[2L], rx = r, ry = r,
            theta.1 = x$alpha1, theta.2 = x$alpha2, plot = TRUE, ...)
  # }
}

#' @rdname draw
#' @importFrom DescTools DrawEllipse
#' @export
draw.Ellipse = function(x, ...) {
  center <- x$center
  alpha <- x$alpha
  if(x$degrees) alpha <- alpha * pi/180
  if("col" %in% names(list(...))){
    DrawEllipse(center[1L], center[2L],
                radius.x = x$rmajor, radius.y = x$rminor,
                rot = alpha, plot = TRUE, ...)
  }else{
    DrawEllipse(center[1L], center[2L],
                radius.x = x$rmajor, radius.y = x$rminor,
                rot = alpha, plot = TRUE, col = "transparent", ...)
  }
}

#' @rdname draw
#' @export
draw.Line = function(x, ...) {
  extendA <- x$extendA; extendB <- x$extendB
  if(extendA && extendB){
    do <- x$directionAndOffset()
    theta <- do$direction; offset <- do$offset
    if(sin(theta) != 0){
      abline(a = offset/sin(theta), b = -1/tan(theta), ...)
    }else{
      abline(v = offset/cos(theta), ...)
    }
  }else if(extendA){
    do <- x$directionAndOffset()
    theta <- do$direction; offset <- do$offset
    A <- x$A; B <- x$B
    bounds <- par("usr")
    if(sin(theta) != 0){
      curve(offset/sin(theta) - x/tan(theta), add = TRUE, n = 2,
            from = B[1], to = ifelse(A[1]<B[1], bounds[1L], bounds[2L]), ...)
    }else{
      M <- if(A[2] < B[2]) c(A[1], bounds[3L]) else c(A[1], bounds[4L])
      lines(rbind(B, M), ...)
    }
  }else if(extendB){
    do <- x$directionAndOffset()
    theta <- do$direction; offset <- do$offset
    A <- x$A; B <- x$B
    bounds <- par("usr")
    if(sin(theta) != 0){
      curve(offset/sin(theta) - x/tan(theta), add = TRUE, n = 2,
            from = A[1], to = ifelse(A[1]<B[1], bounds[2L], bounds[1L]), ...)
    }else{
      M <- if(A[2] < B[2]) c(A[1], bounds[4L]) else c(A[1], bounds[3L])
      lines(rbind(A, M), ...)
    }
  }else{
    lines(rbind(x$A, x$B), ...)
  }
  invisible()
}
