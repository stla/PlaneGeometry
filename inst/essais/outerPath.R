library(PlaneGeometry)

x0 <- 1
y0 <- -1
a <- 3
b <- 2
angle <- 30

ell <- Ellipse$new(c(x0, y0), a, b, angle)

outerPath <- function(x0, y0, a, b, angle, n = 360) {
  angle <- angle/360 * 2 * pi
  theta <- c(seq(0, 2 * pi, length.out = n), 0)

  slopes <- (-a * sin(theta) * sin(angle) + b * cos(theta) * cos(angle))/
    (-a * sin(theta) * cos(angle) - b * cos(theta) * sin(angle))
  crds <- cbind(a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle) + x0,
                a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle) + y0)
  intercepts <- crds[,2] - slopes*crds[,1]
  i <- 1:(n-1)
  x <- (intercepts[i] - intercepts[i+1])/(slopes[i+1] - slopes[i])
  y <- slopes[i]*x + intercepts[i]
  cbind(c(x, x[1]),  c(y, y[1]))
}

path <- outerPath(x0, y0, a, b, angle, n = 6)

box <- ell$boundingbox()
plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
draw(ell, col = "seaShell", border = "blue")
polygon(path)
