library(PlaneGeometry)
library(freegroup)

.a <- alpha(1)
.A <- inverse(.a)
.b <- alpha(2)
.B <- inverse(.b)

tofree <- function(l){
  switch(l,
         a = .a,
         A = .A,
         b = .b,
         B = .B)
}

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


# words of size n
n <- 6L
G <- do.call(expand.grid, rep(list(c("a", "A", "b", "B")), n))
G <- split(as.matrix(G), 1:nrow(G))
G <- lapply(G, function(w){
  sum(do.call(c.free, lapply(w, tofree)))
})
G <- uniqueWith(G, free_equal)
sizes <- sapply(G, total)
G <- lapply(G, `[[`, 1L)
Gn <- G[sizes == n]

# Mobius transformations ####
toCplx <- function(xy) complex(real = xy[1], imaginary = xy[2])
a <- Mobius$new(rbind(c(sqrt(2),1i), c(-1i,sqrt(2))))
b <- Mobius$new(rbind(
  c(toCplx(Cb$center), c(crossprod(Cb$center))-Cb$radius^2),
  c(1, -toCplx(CB$center))
))
A <- a$inverse()
B <- b$inverse()

Gn <- lapply(Gn, function(arr){
  transfos <- vector("list", length = ncol(arr))
  for(j in seq_len(ncol(arr))){
    power <- arr[2, j]
    isneg <- power < 0
    k <- abs(power)
    elemindex <- arr[1L, j]
    if(isneg){
      if(elemindex == 1){
        mobius <- A
      }else{
        mobius <- B
      }
    }else{
      if(elemindex == 1){
        mobius <- a
      }else{
        mobius <- b
      }
    }
    if(j == ncol(arr)){
      transfos[[j]] <- mobius$power(k-1L)
      elem = c("a", "b")[elemindex]
      if(isneg) elem <- toupper(elem)
    }else{
      transfos[[j]] <- mobius$power(k)
    }
  }
  list(compo = Reduce(function(M0,M1) M1$compose(M0), transfos), elem = elem)
})



# plot ####
opar <- par(mar = c(0,0,0,0), bg = "whitesmoke")
plot(NULL, asp = 1, xlim = c(-3,3), ylim = c(-3,3),
     axes = FALSE, xlab = NA, ylab = NA)
draw(Ca); draw(CA); draw(Cb); draw(CB)
draw(A$transformCircle(CA), lwd = 2, border = "red")
draw(A$transformCircle(CB), lwd = 2, border = "red")
draw(A$transformCircle(Cb), lwd = 2, border = "red")
draw(a$transformLine(Ca),   lwd = 2, border = "green")
draw(a$transformCircle(CB), lwd = 2, border = "green")
draw(a$transformCircle(Cb), lwd = 2, border = "green")
draw(b$transformLine(Ca),   lwd = 2, border = "blue")
draw(b$transformCircle(Cb), lwd = 2, border = "blue")
draw(b$transformCircle(CA), lwd = 2, border = "blue")
draw(B$transformLine(Ca),   lwd = 2, border = "yellow")
draw(B$transformCircle(CA), lwd = 2, border = "yellow")
draw(B$transformCircle(CB), lwd = 2, border = "yellow")

toCirc <- function(l){
  switch(l, a = Ca, b = Cb, A = CA, B = CB)
}

circles <- lapply(Gn, function(g){
  mobius <- g[["compo"]]
  if(g[["elem"]] == "a"){
    mobius$transformLine(toCirc("a"))
  }else{
    mobius$transformCircle(toCirc(g[["elem"]]))
  }
})
invisible(lapply(circles, draw, col = "orange"))
