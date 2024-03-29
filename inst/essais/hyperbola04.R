library(PlaneGeometry)

# "FULL" PLOT (derivation of bounding rectangle and t)

# asymptotes
a1 <- 0; b1 <- 2
l1 <- LineFromInterceptAndSlope(a1, b1)
a2 <- -1; b2 <- -0.5
l2 <- LineFromInterceptAndSlope(a2, b2)

l1$isParallel(l2)

# point on hyperbola
A <- c(4, 3)

# center
O <- intersectionLineLine(l1, l2)

# equation O + t f1 + 1/t f2
theta1 <- l1$directionAndOffset()$direction
theta2 <- l2$directionAndOffset()$direction
f10 <- c(sin(theta1), -cos(theta1))
f20 <- c(sin(theta2), -cos(theta2))
#lambdas <- solve(t(rbind(f10, f20)), A - O)
# M <- t(rbind(f10, f20))
detM <- sin(theta2 - theta1)
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
g1 <- A - O
g2 <- tgA
eq1 <- function(t) O + cosh(t) * g1 + sinh(t) * g2
eq2 <- function(t) O - cosh(t) * g1 + sinh(t) * g2

# vertices
t0 <- log(c(crossprod(g1-g2)) / c(crossprod(g1+g2))) / 4
v1 <- eq1(t0)
v2 <- eq2(-t0)

# |f1|=|f2|
lambdas_eq <- invM %*% (v1-O)
lambdaEq <- lambdas_eq[1L]
f1eq <- lambdaEq * f10
f2eq <- lambdaEq * f20
# v1 =
O + f1eq + f2eq
# tangent at v1
tgV <- f1eq - f2eq

# parametric representation  g0 +/- cosh(t) g1 + sinh(t) g2
g1 <- v1 - O
g2 <- tgV

# foci (see image)
a2 <- c(crossprod(v1 - O))
L <- Line$new(v1, v1 + tgV) # utilisation de tgV ici
I <- intersectionLineLine(l1, L)
b2 <- c(crossprod(v1 - I))
c <- sqrt(a2 + b2)
a <- sqrt(a2)
F1 <- O + c/a * (v1 - O)
F2 <- O - c/a * (v1 - O)
majorAxis <- Line$new(F1, F2)


# a good rectangle ####

u <- tgV / sqrt(c(crossprod(tgV)))
P1 <- F1 + sqrt(b2) * u
PP1 <- F1 - sqrt(b2) * u
P2 <- F2 - sqrt(b2) * u
PP2 <- F2 + sqrt(b2) * u

.htrigonometricEquation <- function(a, b, D) {
  # solution of a*cosh(x) + b*sinh(x) = D
  a2 <- a * a
  b2 <- b * b
  if(a2 > b2) {
    acosh(D / (a * sqrt(1 - b2/a2))) - atanh(b/a)
  } else if(a2 < b2) {
    asinh(D / (b * sqrt(1 - a2/b2))) - atanh(a/b)
  } else if(a == b) {
    log(D/A)
  } else {
    log(-D/A)
  }
}


plot(rbind(P1, P2, PP1, PP2), type = "p", asp = 1, xlab ="x", ylab = "y",
     pch = 19, col = "gray", xaxs = "i", yaxs = "i")
xmax <- par("usr")[2L]
t <- .htrigonometricEquation(g1[1L], g2[1L], xmax - O[1L])
t_ <- seq(-t, t, length.out = 100L)
H1 <- t(vapply(t_, function(t) {
  O + cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H1)
H2 <- t(vapply(t_, function(t) {
  O - cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H2)
points(rbind(A), pch = 19, col="blue")
draw(l1, col = "red")
draw(l2, col = "red")
points(t(v1), pch = 19)
points(t(v2), pch = 19)
points(t(F1), pch = 19, col = "green")
points(t(F2), pch = 19, col = "green")
draw(majorAxis, col = "yellow")




