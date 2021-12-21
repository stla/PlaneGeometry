library(PlaneGeometry)

P1 <- c(-1,0); P2 <- c(0, 2); P3 <- c(3,0)
O <- c(1, -1)

P4 <- P1 + 2*(O-P1)
P5 <- P2 + 2*(O-P2)

ell <- EllipseFromFivePoints(P1, P2, P3, P4, P5)
ell$includes(P1); ell$includes(P2); ell$includes(P3)
ell$includes(P4); ell$includes(P5)


###########################################
ellipse <- function(xf1, yf1, xf2, yf2, k, new=TRUE,...){
  # xf1 and yf1 are the coordinates of your focus F1
  # xf2 and yf2 are the coordinates of your focus F2
  # k is your constant (sum of distances to F1 and F2 of any points on the ellipse)
  # new is a logical saying if the function needs to create a new plot or add an ellipse to an existing plot.
  # ... is any arguments you can pass to functions plot or lines (col, lwd, lty, etc.)
  t <- seq(0, 2*pi, by=pi/100)  # Change the by parameters to change resolution
  k/2 -> a  # Major axis
  xc <- (xf1+xf2)/2
  yc <- (yf1+yf2)/2  # Coordinates of the center
  dc <- sqrt((xf1-xf2)^2 + (yf1-yf2)^2)/2  # Distance of the foci to the center
  b <- sqrt(a^2 - dc^2)  # Minor axis
  phi <- atan(abs(yf1-yf2)/abs(xf1-xf2))  # Angle between the major axis and the x-axis
  xt <- xc + a*cos(t)*cos(phi) - b*sin(t)*sin(phi)
  yt <- yc + a*cos(t)*sin(phi) + b*sin(t)*cos(phi)
}

F1 <- c(-2, 1)
F2 <- c(3, 1)
A <- c(0, 4)

k <- sqrt(c(crossprod(A-F1))) + sqrt(c(crossprod(A-F2)))
a <- k/2
center <- (F1+F2)/2
dc <- sqrt((F1[1]-F2[1])^2 + (F1[2]-F2[2])^2)/2  # Distance of the foci to the center
b <- sqrt(a^2 - dc^2)  # Minor axis
phi <- atan(abs(F1[2]-F2[2])/abs(F1[1]-F2[1]))  # Angle between the major axis and the x-axis

ell <- Ellipse$new(center, a, b, phi, degrees = FALSE)
ell$includes(A)
