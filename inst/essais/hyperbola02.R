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

# equation O + t f1 + 1/t f2
theta1 <- l1$directionAndOffset()$direction
theta2 <- l2$directionAndOffset()$direction
f10 <- c(sin(theta1), -cos(theta1))
f20 <- c(sin(theta2), -cos(theta2))
#lambdas <- solve(t(rbind(f10, f20)), A - O)
# M <- t(rbind(f10, f20))
detM <- cos(theta1 + theta2)
invM <- rbind(
  c(-cos(theta2), -sin(theta2)),
  c(cos(theta1), sin(theta1))
) / detM
lambdas <- invM %*% (A-O)

lambda1 <- lambdas[1L]
lambda2 <- lambdas[2L]
f1 <- lambda1 * f10
f2 <- lambda2 * f20
# A =
O + f1 + f2
# tangent at A
tgA <- f1 - f2

# parametric representation  g0 +/- cosh(t) g1 + sinh(t) g2
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

eq1 <- function(t) O + cosh(t) * g1 + sinh(t) * g2
eq2 <- function(t) O - cosh(t) * g1 + sinh(t) * g2

# vertices
t0 <- log(c(crossprod(g1-g2)) / c(crossprod(g1+g2))) / 4
v1 <- eq1(t0)
v2 <- eq2(-t0)
points(t(v1), pch = 19)
points(t(v2), pch = 19)
