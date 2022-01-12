library(PlaneGeometry)
library(fitConic)

ell <- Ellipse$new(c(1,1), 3, 2, 30)
set.seed(666L)
npoints <- 5
pts <- ell$randomPoints(npoints, "on")
ptsNoise <- pts + matrix(rnorm(npoints*2, sd = 0.2), ncol = 2)

fit <- fitConic(ptsNoise, conicType = "e")
cfs <- fit$parA

ell2 <- EllipseFromEquation(cfs[1], cfs[2], cfs[3], cfs[4], cfs[5], cfs[6])

box <- ell$boundingbox()
plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
draw(ell, border = "blue", lwd = 2)
points(ptsNoise, pch = 19)
draw(ell2, border = "green", lwd = 2)


fitEllipse <- function(points){
  fit <- fitConic(points, conicType = "e")
  if(fit[["exitCode"]] != 1){
    stop("The ellipse fitting has failed.", call. = TRUE)
  }
  cfs <- fit[["parA"]]
  fittedEllipse <- EllipseFromEquation(
    cfs[1L], cfs[2L], cfs[3L], cfs[4L], cfs[5L], cfs[6L]
  )
  attr(fittedEllipse, "RSS") <- fit[["RSS"]]
  fittedEllipse
}

##############################################

#' # We add some noise to 30 points on an ellipse:
#' ell <- Ellipse$new(c(1, 1), 3, 2, 30)
#' set.seed(666L)
#' points <- ell$randomPoints(30, "on") + matrix(rnorm(30*2, sd = 0.2), ncol = 2)
#' # Now we fit an ellipse to these points:
#' ellFitted <- fitEllipse(points)
#' # let's draw all this stuff:
#' box <- ell$boundingbox()
#' plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
#' draw(ell, border = "blue", lwd = 2)
#' points(points, pch = 19)
#' draw(ellFitted, border = "green", lwd = 2)
