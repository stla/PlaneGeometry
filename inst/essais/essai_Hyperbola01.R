library(PlaneGeometry)

# asymptotes (input)
a1 <- 0; b1 <- 2
L1 <- LineFromInterceptAndSlope(a1, b1)
a2 <- -1; b2 <- -0.5
L2 <- LineFromInterceptAndSlope(a2, b2)
# point on hyperbola (input)
M <- c(4, 3)

hyperbola <- Hyperbola$new(L1, L2, M)

hyperbola$plot()
