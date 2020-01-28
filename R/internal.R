.isInteger <- function(x){
  is.numeric(x) && all(is.finite(x)) && !any(is.na(x)) && all(trunc(x) == x)
}

.toCplx <- function(M){
  if(isTRUE(all.equal(M, Inf))){
    Inf
  }else{
    complex(real = M[1L], imaginary = M[2L])
  }
}

.fromCplx <- function(z){
  c(Re(z), Im(z))
}

.Mod2 <- function(z){
  Re(z)*Re(z) + Im(z)*Im(z)
}

.distance <- function(A, B){
  sqrt(c(crossprod(A-B)))
}

.dot <- function(u, w = NULL){
  c(crossprod(u, w))
}

.vlength <- function(v){
  sqrt(c(crossprod(v)))
}

.LineLineIntersection <- function (P1, P2, Q1, Q2) {
  dx1 <- P1[1L] - P2[1L]
  dx2 <- Q1[1L] - Q2[1L]
  dy1 <- P1[2L] - P2[2L]
  dy2 <- Q1[2L] - Q2[2L]
  D <- det(rbind(c(dx1, dy1), c(dx2, dy2)))
  if (D == 0) {
    return(c(Inf, Inf))
  }
  D1 <- det(rbind(P1, P2))
  D2 <- det(rbind(Q1, Q2))
  c(
    det(rbind(c(D1, dx1), c(D2, dx2))),
    det(rbind(c(D1, dy1), c(D2, dy2)))
  ) / D
}

.collinear <- function(A, B, C, tol = 0) {
  notdistinct <-
    isTRUE(all.equal(A,B)) || isTRUE(all.equal(A,C)) || isTRUE(all.equal(B,C))
  if(notdistinct) return(TRUE)
  AB <- B-A; AC <- C-A
  z <- (AB[1] - 1i*AB[2]) * (AC[1] + 1i*AC[2])
  re <- Re(z); im <- Im(z)
  1 / (1 + im*im/re/re) >= 1 - tol
}

.CircleLineIntersection00 <- function(A1, A2, r) {
  x1 <- A1[1L]; y1 <- A1[2L]
  x2 <- A2[1L]; y2 <- A2[2L]
  dx <- x2 - x1; dy <- y2 - y1
  dr2 <- dx*dx + dy*dy
  D <- det(cbind(A1,A2))
  Delta <- r*r*dr2 - D*D
  if(Delta < 0){
    return(NULL)
  }
  if(Delta < sqrt(.Machine$double.eps)){
    return(D/dr2 * c(dy, -dx))
  }
  sgn <- ifelse(dy < 0, -1, 1)
  Ddy <- D*dy
  sqrtDelta <- sqrt(Delta)
  I1 <- c(
    Ddy + sgn*dx * sqrtDelta,
    -D*dx + abs(dy)*sqrtDelta
  ) / dr2
  I2 <- c(
    Ddy - sgn*dx * sqrtDelta,
    -D*dx - abs(dy)*sqrtDelta
  ) / dr2
  list(I1 = I1, I2 = I2)
}

.SteinerChain_phi0 <- function(c0, n, shift){
  R <- c0$radius; O <- c0$center
  sine <- sin(pi/n)
  Cradius <- R / (1+sine)
  Cside <- Cradius*sine
  circles0 <- vector("list", n+1)
  for(i in 1:n){
    beta <- (i-1+shift)*2*pi/n
    pti <- Cradius*c(cos(beta), sin(beta)) + O
    circ1 <- Circle$new(pti, Cside)
    circles0[[i]] <- circ1
  }
  circles0[[n+1]] <- Circle$new(O, R-2*Cside)
  circles0
}

.inversion2conjugateMobius <- function(iota){
  C <- .toCplx(iota$pole)
  k <- iota$power
  Mobius$new(rbind(c(C, k-C*Conj(C)), c(1, -Conj(C))))
}

.MobiusMappingThreePoints2ZeroOneInf <- function(z1, z2, z3){
  if(z1 == Inf){
    K <- z2 - z3
    return(Mobius$new(rbind(c(0,K),c(1,-z3))))
  }
  if(z2 == Inf){
    return(Mobius$new(rbind(c(1,-z1),c(1,-z3))))
  }
  if(z3 == Inf){
    K <- 1 / (z2 - z1)
    return(Mobius$new(rbind(c(K, -K*z1),c(0,1))))
  }
  K <- (z2 - z3) / (z2 - z1)
  Mobius$new(rbind(c(K, -K*z1),c(1,-z3)))
}

.ellipsePoints <- function(t, O, a, b, alpha){
  x <- a*cos(t); y <- b*sin(t)
  cosalpha <- cos(alpha); sinalpha <- sin(alpha)
  cbind(
    x = O[1] + cosalpha*x - sinalpha*y,
    y = O[2] + sinalpha*x + cosalpha*y
  )
}

.circlePoints <- function(t, O, r){
  cbind(
    x = O[1] + r*cos(t),
    y = O[2] + r*sin(t)
  )
}

.solveTrigonometricEquation <- function(a, b, D = 0){
  # a*cos(x) + b*sin(x) = D
  if(D == 0){
    atan2(b, a) + c(pi/2 , -pi/2)
  }
  # https://socratic.org/questions/how-do-you-use-linear-combinations-to-solve-trigonometric-equations
}

.circleAsEllipse <- function(circ){
  Ellipse$new(circ$center, circ$radius, circ$radius, 0)
}

.EllipseFromCenterAndEigen <- function(center, e){
  v <- e$vectors[,2L]
  alpha <- (atan2(v[2L],v[1L]) * 180/pi) %% 180
  a <- 1/sqrt(e$values[2L])#.vlength(v/r)
  b <- a * sqrt(e$values[2L]/e$values[1L])
  Ellipse$new(center, a, b, alpha)
}
