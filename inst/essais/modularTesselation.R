library(PlaneGeometry)
library(elliptic) # for the unimodular matrices

# Möbius transformations
T <- Mobius$new(rbind(c(0,-1), c(1,0)))
U <- Mobius$new(rbind(c(1,1), c(0,1)))
R <- U$compose(T)
# R**t, generalized power
Rt <- function(t){
  R$gpower(t)
}

# starting circles
I <- Circle$new(c(0, 1.5), 0.5)
TI <- T$transformCircle(I)

# modified Cayley transformation
Phi <- Mobius$new(rbind(c(1i, 1), c(1, 1i)))


draw_pair <- function(M, u, compose = FALSE){
  if(compose) M <- M$compose(T)
  A <- M$compose(Rt(u))$compose(Phi)
  C <- A$transformCircle(I)
  draw(C, col = "magenta")
  C <- A$transformCircle(TI)
  draw(C, col = "magenta")
  if(!compose){
    draw_pair(M, u, compose=TRUE)
  }
}

n <- 8L
transfos <- unimodular(n)

fplot <- function(u){
  opar <- par(mar = c(0,0,0,0), bg = "black")
  plot(NULL, asp = 1, xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
       axes = FALSE, xlab = NA, ylab = NA)
  for(i in 1L:dim(transfos)[3L]){
    transfo <- transfos[, , i]
    M <- Mobius$new(transfo)
    draw_pair(M, u)
    M <- M$inverse()
    draw_pair(M, u)
    diag(transfo) <- -diag(transfo)
    M <- Mobius$new(transfo)
    draw_pair(M, u)
    M <- M$inverse()
    draw_pair(M, u)
    d <- diag(transfo)
    if(d[1L] != d[2L]){
      diag(transfo) <- rev(diag(transfo))
      M <- Mobius$new(transfo)
      draw_pair(M, u)
      M <- M$inverse()
      draw_pair(M, u)
    }
  }
}

library(gifski)
u_ <- seq(0, 3, length.out = 181)[-1]
save_gif(
  for(u in u_){
    fplot(u)
  },
  width = 512,
  height = 512,
  delay = 0.1
)
