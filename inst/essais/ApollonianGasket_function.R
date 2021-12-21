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

library(viridisLite) # for the colors
c0 <- Circle$new(c(0,0), 3) # the exterior circle
depth <- 5
colors <- plasma(depth)
ApollonianCircles <- ApollonianGasket(c0, n = 3, phi = 0.3, shift = 0, depth)

# plot ####
center0 <- c0$center
radius0 <- c0$radius
xlim <- center0[1] + c(-radius0 - 0.1, radius0 + 0.1)
ylim <- center0[2] + c(-radius0 - 0.1, radius0 + 0.1)
opar <- par(mar = c(0, 0, 0, 0))
plot(NULL, type = "n", xlim = xlim, ylim = ylim,
     xlab = NA, ylab = NA, axes = FALSE, asp = 1)
draw(c0, border = "black", lwd = 2)
for(i in 1:depth){
  for(circ in ApollonianCircles[[i]]){
    draw(circ, col = colors[i])
  }
}
par(opar)


fplot <- function(shift){
  gasket <- ApollonianGasket(c0, n = 3, phi = 0.3, shift = shift, depth)
  par(mar = c(0, 0, 0, 0))
  plot(NULL, type = "n", xlim = xlim, ylim = ylim,
       xlab = NA, ylab = NA, axes = FALSE, asp = 1)
  draw(c0, border = "black", lwd = 2)
  for(i in 1:depth){
    for(circ in gasket[[i]]){
      draw(circ, col = colors[i])
    }
  }
}

fanim <- function(){
  shifts <- seq(0, 3, length.out = 101)[-101]
  for(shift in shifts){
    fplot(shift)
  }
}

library(gifski)
save_gif(
  fanim(),
  "ApollonianGasket.gif",
  width = 512, height = 512,
  delay = 0.1
)
