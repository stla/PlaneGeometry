library(PlaneGeometry)

# function to construct the "children" ####
ApollonianChildren <- function(inversions, circles1){
  m <- length(inversions)
  n <- length(circles1)
  circles2 <- list()
  for(i in 1:n){
    circ <- circles1[[i]]
    k <- attr(circ, "inversion")
    for(j in 1:m){
      if(j != k){
        circle <- inversions[[j]]$invertCircle(circ)
        attr(circle, "inversion") <- j
        circles2 <- append(circles2, circle)
      }
    }
  }
  circles2
}

ApollonianGasket <- function(c0, n, phi, shift, depth){
  circles0 <- SteinerChain(c0, n, phi, shift)
  # construct the inversions ####
  inversions <- vector("list", n + 1)
  for(i in 1:n){
    inversions[[i]] <- inversionFixingThreeCircles(
      c0, circles0[[i]], circles0[[(i %% n) + 1]]
    )
  }
  inversions[[n+1]] <- inversionSwappingTwoCircles(c0, circles0[[n+1]])
  # first generation of children
  circles1 <- list()
  for(i in 1:n){
    ip1 <- (i %% n) + 1
    for(j in 1:(n+1)){
      if(j != i && j != ip1){
        circle <- inversions[[i]]$invertCircle(circles0[[j]])
        attr(circle, "inversion") <- i
        circles1 <- append(circles1, circle)
      }
    }
  }
  # construct children ####
  allCircles <- vector("list", depth)
  allCircles[[1]] <- circles0
  allCircles[[2]] <- circles1
  for(i in 3:depth){
    allCircles[[i]] <- ApollonianChildren(inversions, allCircles[[i-1]])
  }
  allCircles
}



MyMatPow <- function(gamma, t){
  g2 <- Mod(gamma)^2
  h <- sqrt(1-g2)
  d2 <- h^t * (cos(t*pi/2) + 1i*sin(t*pi/2))
  d1 <- Conj(d2)
  H11 <- Re(d1) - 1i*Im(d1)/h
  H12 <- Im(d2) * gamma / h
  H21 <- Conj(H12)
  H22 <- Conj(H11)
  rbind(c(H11, H12), c(H21, H22))
}

Mt <- function(gamma, t){
  Mobius$new(MyMatPow(gamma, t))
}










library(viridisLite) # for the colors
c0 <- Circle$new(c(0,0), 1) # the exterior circle
depth <- 5
colors <- plasma(depth)
ApollonianCircles <- ApollonianGasket(c0, n = 3, phi = 0.1, shift = 0.5, depth)
# plot ####
# center0 <- c0$center
# radius0 <- c0$radius
xlim <- c(-1.1, 1.1)
ylim <- c(-1.1, 1.1)
opar <- par(mar = c(0, 0, 0, 0))
fplot <- function(gamma, t){
  plot(NULL, type = "n", xlim = xlim, ylim = ylim,
       xlab = NA, ylab = NA, axes = FALSE, asp = 1)
  draw(c0, border = "black", lwd = 2)
  Mob <- Mt(gamma, t)
  for(i in 1:depth){
    for(circ in ApollonianCircles[[i]]){
      draw(Mob$transformCircle(circ), col = colors[i])
    }
  }
}

fanim <- function(){
  gamma <- 0.5 + 0.4i
  t_ <- seq(0, 2, length.out = 91)[-91]
  for(t in t_){
    fplot(gamma, t)
  }
}


library(gifski)
save_gif(
  fanim(),
  "ApollonianMobius.gif",
  width = 512, height = 512,
  delay = 0.1
)



par(opar)
