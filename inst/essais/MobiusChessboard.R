

Mt <- function(gamma, t){
  h <- sqrt(1 - Mod(gamma)^2)
  d2 <- h^t * (cos(t*pi/2) + 1i*sin(t*pi/2))
  d1 <- Conj(d2)
  A11 <- Re(d1) - 1i*Im(d1)/h
  A12 <- Im(d2) * gamma / h
  rbind(
    c(A11, A12),
    c(Conj(A11), Conj(A12))
  )
}

img <- matrix(NA_real_, nrow = 512L, ncol = 512L)
x <- y <- seq(-4, 4, length.out = 512L)
Mob <- Mobius$new(Mt(0.4 + 0.5i, 1))
for(i in 1L:512L){
  for(j in 1L:512L){
    P <- Mob$transform(c(x[i], y[j]))
    img[i, j] <- ifelse(
      floor(P[1]) %% 2 == 0,
      ifelse(floor(P[2]) %% 2 == 0, "black", "white"),
      ifelse(floor(P[2]) %% 2 == 0, "white", "black")
    )
  }
}
