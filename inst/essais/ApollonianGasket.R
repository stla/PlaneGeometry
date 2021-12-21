library(PlaneGeometry)

# starting circles ####
c0 <- Circle$new(c(0,0), 3) # the exterior circle
n <- 3
circles0 <- SteinerChain(c0, n, phi = 0.25, shift = 0)

# construct the inversions ####
inversions = vector("list", n+1)
for(i in 1:n){
  inversions[[i]] = inversionFixingThreeCircles(
    c0, circles0[[i]], circles0[[(i %% n) + 1]]
  )
}
inversions[[n+1]] = inversionSwappingTwoCircles(c0, circles0[[n+1]])

# first generation of children
circles1 = list()
for(i in 1:n){
  ip1 = (i %% n) + 1
  for(j in 1:(n+1)){
    if(j != i && j != ip1){
      circle <- inversions[[i]]$invertCircle(circles0[[j]])
      attr(circle, "inversion") <- i
      circles1 <- append(circles1, circle)
    }
  }
}


# function to construct the "children" ####
children <- function(inversions, circles1){
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
  return(circles2)
}

# construct children ####
depth <- 5
allCircles <- vector("list", depth)
allCircles[[1]] <- circles0
allCircles[[2]] <- circles1
for(i in 3:depth){
  allCircles[[i]] <- children(inversions, allCircles[[i-1]])
}

# plot ####
colors <- viridisLite::plasma(depth)
plot(NULL, type = "n", xlim = c(-3.1, 3.1), ylim = c(-3.1, 3.1),
     xlab = NA, ylab = NA, axes = FALSE, asp = 1)
draw(c0, border = "black")
for(i in 1:depth){
  for(circ in allCircles[[i]]){
    draw(circ, col = colors[i])
  }
}
