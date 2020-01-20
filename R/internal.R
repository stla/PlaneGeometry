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
  AB <- B-A; AC <- C-A
  z <- (AB[1] - 1i*AB[2]) * (AC[1] + 1i*AC[2])
  re <- Re(z); im <- Im(z)
  1 / (1 + im*im/re/re) >= 1 - tol
}
