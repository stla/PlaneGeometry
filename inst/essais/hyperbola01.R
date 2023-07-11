library(PlaneGeometry)

# asymptotes
a1 <- 0; b1 <- 1
l1 <- LineFromInterceptAndSlope(a1, b1)
a2 <- -1; b2 <- -0.5
l2 <- LineFromInterceptAndSlope(a2, b2)

# point on hyperbola
A <- c(4, 2)

# center
O <- intersectionLineLine(l1, l2)

#
f10 <- c(1, b1)
f20 <- c(1, b2)
lambdas <- solve(t(rbind(f10, f20)), A - O)
lambda1 <- lambdas[1L]
lambda2 <- lambdas[2L]
f1 <- lambda1 * f10
f2 <- lambda2 * f20
# A =
O + f1 + f2
# tangent at A
tgA <- f1 - f2

# parametric representation
g0 <- O
g1 <- A - O
g2 <- tgA

plot(NULL, asp = 1, xlim = c(-5, 5), ylim = c(-5, 5), xlab = "x", ylab = "y")
t_ <- seq(-2, 2, length.out = 100L)
H1 <- t(vapply(t_, function(t) {
  O + cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H1)
H2 <- t(vapply(t_, function(t) {
  O - cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H2)
points(rbind(A), pch = 19)
draw(l1, col = "red")
draw(l2, col = "red")


