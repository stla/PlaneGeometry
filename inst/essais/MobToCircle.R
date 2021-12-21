library(PlaneGeometry)

CircToMob <- function(circ){
  r <- circ$radius
  C <- PlaneGeometry:::.toCplx(circ$center)
  A <- 1/r * rbind(c(C, r*r - C*Conj(C)), c(1, -Conj(C)))
  Mobius$new(A)
}

MobToCirc <- function(T){
  Circle$new(PlaneGeometry:::.fromCplx(T$a/T$c), abs(1/Re(T$c)))
}

T <- CircToMob(Circle$new(c(1,1), 2))
#T$c <- 1+1i
M <- T$getM()
-M[1,1] * M[1,2] / 2
-Arg(M[1,1])/pi - 1.5


circ <- MobToCirc(T)

T$transformCircle(circ)

M <- T$inverse()$getM()
S <- Mobius$new(Conj(M))
S$transformCircle(circ)
