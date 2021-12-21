library(PlaneGeometry)
library(freegroup)

uniqueWith <- function(v, f){
  size <- length(v)
  for(i in seq_len(size-1L)){
    j <- i + 1L
    while(j <= size){
      if(f(v[[j]], v[[i]])){
        v <- v[-j]
        size <- size - 1L
      }else{
        j <- j + 1L
      }
    }
  }
  v[1L:size]
}

a <- alpha(1)
A <- inverse(a)
b <- alpha(2)
B <- inverse(b)

# words of size n
n <- 6L
G <- do.call(expand.grid, rep(list(c("a", "A", "b", "B")), n))
G <- split(as.matrix(G), 1:nrow(G))
G <- lapply(G, function(w){
  sum(do.call(c.free, lapply(w, function(x) switch(x, a=a, A=A, b=b, B=B))))
})
G <- uniqueWith(G, free_equal)
sizes <- vapply(G, total, numeric(1L))
Gn <- G[sizes == n]

transfo2seq <- function(g){
  seq <- c()
  gr <- reduce(g)[[1L]]
  for(j in 1L:ncol(gr)){
    monomial <- gr[, j]
    t <- c("a", "b")[monomial[1L]]
    i <- monomial[2L]
    if(i < 0L){
      i <- -i
      t <- toupper(t)
    }
    seq <- c(seq, rep(t, i))
  }
  seq
}

Mob_a <- Mobius$new(rbind(c(sqrt(2), 1i), c(-1i, sqrt(2))))
Mob_A <- Mob_a$inverse()

# starting circles ####
Ca <- Line$new(c(-1,0), c(1,0))
Rc <- sqrt(2)/4
yI <- -3*sqrt(2)/4
CA <- Circle$new(c(0,yI), Rc)
theta <- -0.5
T <- c(Rc*cos(theta), yI+Rc*sin(theta))
P <- c(T[1]+T[2]*tan(theta), 0)
PT <- sqrt(c(crossprod(T-P)))
xTprime <- P[1]+PT
xPprime <- -yI/tan(theta)
PprimeTprime <- abs(xTprime-xPprime)
Rcprime <- abs(yI*PprimeTprime/xPprime)
Cb <- Circle$new(c(xTprime, -Rcprime), Rcprime)
CB <- Circle$new(c(-xTprime, -Rcprime), Rcprime)

toCplx <- function(xy) complex(real = xy[1], imaginary = xy[2])
Mob_b <- Mobius$new(rbind(
  c(toCplx(Cb$center), c(crossprod(Cb$center))-Cb$radius^2),
  c(1, -toCplx(CB$center))
))
Mob_B <- Mob_b$inverse()

MOBS <- list(a = Mob_a, A = Mob_A, b = Mob_b, B = Mob_B)
GCIRCLES <- list(a = Ca, A = CA, b = Cb, B = CB)

circle <- function(g){
  seq <- transfo2seq(g)
  mobs <- MOBS[seq]
  mobius <- Reduce(function(M1, M2) M1$compose(M2), mobs[-n])
  mobius$transformGcircle(GCIRCLES[[seq[n]]])
}


# plot ####
fplot <- function(){
  opar <- par(mar = c(0,0,0,0), bg = "black")
  plot(NULL, asp = 1, xlim = c(-3,3), ylim = c(-3,3),
       axes = FALSE, xlab = NA, ylab = NA)
  draw(Ca); draw(CA); draw(Cb); draw(CB)
  C1 <- Mob_A$transformCircle(CA)
  C2 <- Mob_A$transformCircle(CB)
  C3 <- Mob_A$transformCircle(Cb)
  draw(C1, lwd = 2, border = "red")
  draw(C2, lwd = 2, border = "red")
  draw(C3, lwd = 2, border = "red")
  C1 <- Mob_a$transformLine(Ca)
  C2 <- Mob_a$transformCircle(Cb)
  C3 <- Mob_a$transformCircle(CB)
  draw(C1, lwd = 2, border = "green")
  draw(C2, lwd = 2, border = "green")
  draw(C3, lwd = 2, border = "green")
  C1 <- Mob_b$transformLine(Ca)
  C2 <- Mob_b$transformCircle(CA)
  C3 <- Mob_b$transformCircle(Cb)
  draw(C1, lwd = 2, border = "blue")
  draw(C2, lwd = 2, border = "blue")
  draw(C3, lwd = 2, border = "blue")
  C1 <- Mob_B$transformLine(Ca)
  C2 <- Mob_B$transformCircle(CA)
  C3 <- Mob_B$transformCircle(CB)
  draw(C1, lwd = 2, border = "yellow")
  draw(C2, lwd = 2, border = "yellow")
  draw(C3, lwd = 2, border = "yellow")
  for(g in Gn){
    circ <- circle(g)
    draw(circ, lwd = 2, border = "orange")
  }
}
