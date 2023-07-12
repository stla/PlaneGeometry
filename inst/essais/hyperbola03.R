library(PlaneGeometry)

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
detM <- -sin(theta1)*cos(theta2) + sin(theta2)*cos(theta1) # = ? sin(theta2 - theta1)
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
g0 <- O
g1 <- v1 - O
g2 <- tgV

plot(NULL, asp = 1, xlim = c(-5, 5), ylim = c(-5, 5), xlab = "x", ylab = "y")
t_ <- seq(-5, 5, length.out = 100L)
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

# intersecting rectangle
h <- function(P) {
  det(cbind(P-O, g2))^2 - det(cbind(g1, P-O))^2 - det(cbind(g1, g2))^2
}

xmin <- -10
xmax <- 10
ymin <- -2
ymax <- 2

P1 <- c(xmin, ymin)
P2 <- c(xmax, ymin)
P3 <- c(xmax, ymax)
P4 <- c(xmin, ymax)

h(P1); h(P2); h(P3); h(P4) # tous positifs => tous à l'intérieur des branches
                           # est-ce général ?.. si tu ne sais pas, tu regardes
                           # le signe pour P = O... qui est clairement négatif,
                           # donc c'est général ! (et pas besoin de |f1|=|f2|)
# -> même signes, mais le rectangle coupe les asymptotes
# rq: suffit de regarder pour P1 et P3 il me semble
h(v2) # 0

# pour voir que le rectangle coupe une asymptote:
do <- l1$directionAndOffset()
theta <- do$direction
offset <- do$offset
s <- function(P) {
  cos(theta) * P[1L] + sin(theta) * P[2L] - offset
}
s(P1); s(P2); s(P3); s(P4) # -> changement de signes

# foci (see image)
a2 <- c(crossprod(v1 - O))
L <- Line$new(v1, v1+tgV) # utilisation de tgV ici
I <- intersectionLineLine(l1, L)
b2 <- c(crossprod(v1 - I))
c <- sqrt(a2 + b2)
a <- sqrt(a2)
F1 <- O + c/a * (v1 - O)
F2 <- O - c/a * (v1 - O)
points(t(F1), pch = 19, col = "green")
points(t(F2), pch = 19, col = "green")
majorAxis <- Line$new(F1, F2)
draw(majorAxis, col = "yellow")


# a good rectangle
x1 <- F1[1]
#t1 <- acosh(x1) nimp
x2 <- F2[1]
y1 <- F1[2L]
y2 <- F2[2L]
#t1 <- asinh(y1 - O[2]) nimp

# prend p, le semi-machin rectum, ça donne un point sur l'hyperbole,
# et pour un tel point on peut trouver t... ça ne suffit pas!!
L <- majorAxis$perpendicular(F1)
# ajouter méthode Line vecteur directeur unitaire ?
alpha <- L$directionAndOffset()$direction
u <- c(sin(alpha), -cos(alpha)) # c'est tgV normalisé !
P1 <- F1 + sqrt(b2) * u
PP1 <- F1 - sqrt(b2) * u
points(t(P1), pch = 19, col = "gray")
P2 <- F2 - sqrt(b2) * u
PP2 <- F2 + sqrt(b2) * u
points(t(P2), pch = 19, col = "gray")

plot(rbind(P1, P2, PP1, PP2), type = "p", asp = 1, xlab ="x", ylab = "y",
     pch = 19, col = "gray", xaxs = "i", yaxs = "i")
t_ <- seq(-t1, t1, length.out = 100L)
H1 <- t(vapply(t_, function(t) {
  O + cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H1)
H2 <- t(vapply(t_, function(t) {
  O - cosh(t) * g1 + sinh(t) * g2
}, numeric(2L)))
lines(H2)
#points(rbind(A), pch = 19, col="blue")
draw(l1, col = "red")
draw(l2, col = "red")
points(t(v1), pch = 19)
points(t(v2), pch = 19)
points(t(F1), pch = 19, col = "green")
points(t(F2), pch = 19, col = "green")
draw(majorAxis, col = "yellow")


# CONJECTURE
# a*cosh(x) - b*sinh(x) = K(a,b) * cosh(x - atanh(b/a))
a <- 7; b <- 2; x <- 1.5
( cosh(x - atanh(b/a)) / ( a*cosh(x) - b*sinh(x) ) )^2 * (a+b) * (a-b)

