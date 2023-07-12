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
L1 <- LineFromInterceptAndSlope(0, 2)
L2 <- LineFromInterceptAndSlope(-2, -0.15)
M <- c(4, 3)
hyperbola <- Hyperbola$new(L1, L2, M)
OAB <- hyperbola$OAB()
O <- OAB$O
A <- OAB$A
B <- OAB$B

t <- 2
P <- O + cosh(t)*A + sinh(t)*B
tg <- Line$new(P, P + sinh(t)*A + cosh(t)*B)

C <- intersectionLineLine(L1, tg)
D <- intersectionLineLine(L2, tg)

trgl <- Triangle$new(O, C, D)
trgl$area()
