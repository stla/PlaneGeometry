.isInteger <- function(x){
  is.numeric(x) && all(is.finite(x)) && !any(is.na(x)) && all(trunc(x) == x)
}

.isPoint <- function(M){
  is.numeric(M) && length(M) == 2L && !any(is.na(M)) && all(is.finite(M))
}

.toCplx <- function(M){
  if(isTRUE(all.equal(M, Inf, check.attributes = FALSE))){
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
    isTRUE(all.equal(A, B, check.attributes = FALSE)) ||
    isTRUE(all.equal(A, C, check.attributes = FALSE)) ||
    isTRUE(all.equal(B, C, check.attributes = FALSE))
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
  # solve a*cos(x) + b*sin(x) = D
  if(D == 0){
    return((atan2(b, a) + c(pi, -pi)/2) %% (2*pi))
  }
  d <- sqrt(a*a+b*b)
  if(abs(D)/d > 1) return(NULL)
  if(D == d) return(atan2(b,a) %% (2*pi))
  if(D == -d) return((atan2(b,a)+pi) %% (2*pi))
  e <- acos(D/d)
  (atan2(b,a) + c(e, -e)) %% (2*pi)
  # https://socratic.org/questions/how-do-you-use-linear-combinations-to-solve-trigonometric-equations
  # D = sqrt(a*a+b*b) * cos(x - atan2(b,a))
  # cos(x - atan2(b,a)) = D / sqrt(a*a+b*b)
  # x - atan2(b,a) = acos(D / sqrt(a*a+b*b)) or -acos(D / sqrt(a*a+b*b))
}

.circleAsEllipse <- function(circ){
  Ellipse$new(circ$center, circ$radius, circ$radius, 0)
}

.EllipseFromCenterAndEigen <- function(center, e){
  v <- e$vectors[,2L]
  alpha <- (atan2(v[2L],v[1L]) * 180/pi) %% 180
  a <- 1/sqrt(e$values[2L])
  b <- 1/sqrt(e$values[1L])
  Ellipse$new(center, a, b, alpha)
}

# .Jordan2x2 <- function(M){
#   detM <- det(M)
#   trM <- M[1L,1L] + M[2L,2L]
#   if(abs(trM*trM - 4*detM) < sqrt(.Machine$double.eps)){
#     lambda <- trM/2
#     if(isTRUE(all.equal(M, diag(c(lambda,lambda))))){
#       list(P = diag(2L), J = M)
#     }else{
#       N <- M - diag(c(lambda,lambda))
#       if(isTRUE(all.equal(N[,2L], c(0,0)))){
#         v2 <- c(1,0)
#         v1 <- N[,1L]
#       }else{
#         v2 <- c(0,1)
#         v1 <- N[,2L]
#       }
#       list(P = cbind(v1,v2), J = rbind(c(1,lambda),c(0,1)))
#     }
#   }else{
#     eig <- eigen(M)
#     list(P = eig$vectors, J = diag(eig$values))
#   }
# }

`%**%` <- function(M, k){
  Reduce(`%*%`, replicate(k, M, simplify = FALSE))
}
