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

# quadric equation ####
center <- hyperbola$center()
abce <- hyperbola$abce()
a2 <- abce$a^2
b2 <- abce$b^2
alpha <- atan(sqrt(b2/a2))
x <- center[1L]; y <- center[2L]
sine <- sin(alpha); cosine <- cos(alpha)
sine2 <- sine*sine; cosine2 <- 1-sine2
A <- (-a2*sine2 - b2*cosine2)
B <- 0#2*(b2-a2)*sine*cosine
C <- -a2*cosine2 - b2*sine2
( Det <- A*C - (B/2)^2 ) # must be negative
D <- -2*A*x - B*y
E <- -B*x - 2*C*y
( F <- A*x*x + B*x*y + C*y*y + a2*b2 )
c(A = A, B = B, C = C, D = D, E = E, F = F)

x <- M[1]; y <- M[2]
c(A*x*x, B*x*y, C*y^2, D*x, E*y, F)
A*x^2 + B*x*y + C*y^2 - D*x + E*y - F

### by developing the implicit equation with the determinants ($includes)
( a <- B[2L]^2 - A[2L]^2 )
( b <- 2*(A[1L]*A[2L] - B[1L]*B[2L]) )
( c <- B[1L]^2 - A[1L]^2 )
( d <- -(2*a*O[1L] + b*O[2L]) )
( e <- -(2*c*O[2L] + b*O[1L]) )
#e <- -B[1]^2*2*O[2] + A[1]^2*2*O[2] + 2*B[1]*O[1]*B[2] - 2*A[1]*O[1]*A[2]
x <- M[1]; y <- M[2]
( f <- -(a*x^2 + b*x*y + c*y^2 + d*x + e*y) )


Axx <- B[2L]^2 - A[2L]^2
Axy <- A[1L]*A[2L] - B[1L]*B[2L]
Ayy <- B[1L]^2 - A[1L]^2
Bx <- -(a*O[1L] + b*O[2L]/2)
By <- -(c*O[2L] + b*O[1L]/2)
C <- -(Axx*x^2 + 2*Axy*x*y + Ayy*y^2 + 2*Bx*x + 2*By*y)


#####
a1 <- 0; b1 <- 2
L1 <- LineFromInterceptAndSlope(a1, b1)
a2 <- -1; b2 <- -0.5
L2 <- LineFromInterceptAndSlope(a2, b2)
# point on hyperbola (input)
M <- c(4, 3)
hyperbola <- Hyperbola$new(L1, L2, M)
V1 <- hyperbola$vertices()$V1
OAB <- hyperbola$OAB()
O <- OAB$O
A <- OAB$A
B <- OAB$B

Axx <- B[2L]^2 - A[2L]^2
Axy <- A[1L]*A[2L] - B[1L]*B[2L]
Ayy <- B[1L]^2 - A[1L]^2
Bx <- -(Axx*O[1L] + Axy*O[2L])
By <- -(Ayy*O[2L] + Axy*O[1L])
x <- M[1L]; y <- M[2L]
C <- -(Axx*x^2 + 2*Axy*x*y + Ayy*y^2 + 2*Bx*x + 2*By*y)
list(Axx = Axx, Axy = Axy, Ayy = Ayy, Bx = Bx, By = By, C = C)
x <- V1[1]; y <- V1[2]
-(Axx*x^2 + 2*Axy*x*y + Ayy*y^2 + 2*Bx*x + 2*By*y)

f <- function(x,y) ((x-O[1])*B[2]-(y-O[2])*B[1])^2 - ((x-O[1])*A[2]-(y-O[2])*A[1])^2 - det(cbind(A,B))^2
library(spray)
f(lone(1,2), lone(2,2))

# triangle tangent-asymptotes ####

# take a hyperbola
L1 <- LineFromInterceptAndSlope(0, 2)
L2 <- LineFromInterceptAndSlope(-2, -0.15)
M <- c(2, 3)
hyperbola <- Hyperbola$new(L1, L2, M)
# take a point on the hyperbola and the tangent at this point
OAB <- hyperbola$OAB()
O <- OAB$O; A <- OAB$A; B <- OAB$B
t <- 0.1
P <- O + cosh(t)*A + sinh(t)*B
tgt <- Line$new(P, P + sinh(t)*A + cosh(t)*B)
# the triangle of interest
C <- intersectionLineLine(L1, tgt)
D <- intersectionLineLine(L2, tgt)
trgl <- Triangle$new(O, C, D)
# plot
opar <- par(mar = c(4, 4, 1, 1))
hyperbola$plot(lwd = 2)
draw(L1, col = "red")
draw(L2, col = "red")
text(t(O), "O", pos = 3)
points(t(P), pch = 19, col = "blue")
text(t(P), "P", pos = 4)
draw(tgt, col = "blue", lwd = 2)
text(t(C), "C", pos = 2)
text(t(D), "D", pos = 4)
trgl$plot(add = TRUE, col = "yellow")
par(opar)
# theorem checking: area of the triangle does not depend on
# the choice of P; more precisely, it is equal to ab
trgl$area()
with(hyperbola$abce(), a * b)


# "hvab theta"
library(fitConic)
L1 <- LineFromInterceptAndSlope(0, 2)
L2 <- LineFromInterceptAndSlope(-2, -0.15)
M <- c(4, 3)
hyperbola <- Hyperbola$new(L1, L2, M)
OAB <- hyperbola$OAB()
O <- OAB$O
A <- OAB$A
B <- OAB$B
eq <- hyperbola$equation()
parA <- c(eq$Axx, 2*eq$Axy, eq$Ayy, 2*eq$Bx, 2*eq$By, eq$C)
print(AtoG(parA))
a <- hyperbola$abce()$a
b <- hyperbola$abce()$b
L2$directionAndOffset()$direction - atan(a/b) - pi # theta
2*pi - L1$directionAndOffset()$direction + 0.4791294 # atan(a/b)
# clean:
theta <- AtoG(parA)$parG[5L]
L1direction <- theta - atan(a/b) + 2*pi
L2direction <- theta + atan(a/b) + pi

L1offset <- cos(L1direction) * O[1L] + sin(L1direction)  * O[2L]
L2offset <- cos(L2direction) * O[1L] + sin(L2direction)  * O[2L]

u1 <- c(-sin(L1direction), cos(L1direction))
V1 <- O + a * u1 # noo !!

alpha <- (L1direction + L2direction) / 2
offset <- cos(alpha) * O[1] + sin(alpha) * O[2L]
bissect <- LineFromEquation(cos(alpha), sin(alpha), -offset)
u1 <- c(-sin(alpha), cos(alpha))
O + a * u1 # V1

#' @title Hyperbola object from the hyperbola equation.
#' @description Create the \code{Hyperbola} object representing the hyperbola
#'   with the given implicit equation.
#' @param eq named vector or list of the six parameters \code{Axx}, \code{Axy},
#'   \code{Ayy}, \code{Bx}, \code{By}, \code{C}
#' @return A \code{Hyperbola} object.
#' @export
#' @importFrom fitConic AtoG
HyperbolaFromEquation <- function(eq) {
  parA <- c(
    eq[["Axx"]], 2*eq[["Axy"]], eq[["Ayy"]],
    2*eq[["Bx"]], 2*eq[["By"]], eq[["C"]]
  )
  hvabtheta <- AtoG(parA)$parG
  h <- hvabtheta[1L]
  v <- hvabtheta[2L]
  a <- hvabtheta[3L]
  b <- hvabtheta[4L]
  theta <- hvabtheta[5L]
  beta <- atan(a / b)
  alpha <- theta + 3 * pi / 2
  u1 <- c(-sin(alpha), cos(alpha))
  M <- c(h, v) + a * u1 # V1
  L1direction <- theta - beta + 2*pi
  L2direction <- theta + beta + pi
  L1offset <- cos(L1direction) * h + sin(L1direction) * v
  L2offset <- cos(L2direction) * h + sin(L2direction) * v
  L1 <- LineFromEquation(cos(L1direction), sin(L1direction), -L1offset)
  L2 <- LineFromEquation(cos(L2direction), sin(L2direction), -L2offset)
  Hyperbola$new(L1, L2, M) -> H
}

