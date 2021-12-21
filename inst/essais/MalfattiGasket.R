library(rgl)

unitSphere <- subdivision3d(icosahedron3d(), depth = 4)
unitSphere$vb[4, ] <-
  apply(unitSphere$vb[1:3, ], 2, function(x) sqrt(sum(x * x)))
unitSphere$normals <- unitSphere$vb
drawSphere <- function(circle, ...){
  center <- circle$center
  radius <- circle$radius
  sphere <- scale3d(unitSphere, radius, radius, radius)
  shade3d(translate3d(sphere, center[1], center[2], 0), ...)
}

toCplx <- function(M){
  M[1] + 1i * M[2]
}
fromCplx <- function(z){
  c(Re(z), Im(z))
}

distance <- function(A, B){
  sqrt(c(crossprod(B - A)))
}

innerSoddyRadius <- function(r1, r2, r3){
  1 / (1/r1 + 1/r2 + 1/r3 + 2 * sqrt(1/r1/r2 + 1/r2/r3 + 1/r3/r1))
}

innerSoddyCircle <- function(c1, c2, c3, ...){
  radius <- innerSoddyRadius(c1$radius, c3$radius, c3$radius)
  center <- Triangle$new(c1$center, c2$center, c3$center)$equalDetourPoint()
  c123 <- Circle$new(center, radius)
  drawSphere(c123, ...)
  list(
    list(type = "ccc", c1 = c123, c2 = c1, c3 = c2),
    list(type = "ccc", c1 = c123, c2 = c2, c3 = c3),
    list(type = "ccc", c1 = c123, c2 = c1, c3 = c3)
  )
}

side.circle.circle <- function(A, B, cA, cB, ...){
  if(A[2] > B[2]){
    return(side.circle.circle(B, A, cB, cA, ...))
  }
  rA <- cA$radius
  rB <- cB$radius
  oA <- cA$center
  oB <- cB$center
  zoA <- toCplx(oA)
  zoB <- toCplx(oB)
  zB <- toCplx(A)
  alpha <- acos((B[1] - A[1]) / distance(A, B))
  zX1 <- exp(-1i * alpha) * (zoA - zB)
  zX2 <- exp(-1i * alpha) * (zoB - zB)
  soddyR <- innerSoddyRadius(rA, rB, Inf)
  if(Re(zX1) < Re(zX2)){
    Y <- (2 * rA * sqrt(rB) / (sqrt(rA) + sqrt(rB)) + Re(zX1)) +
      sign(Im(zX1)) * 1i * soddyR
  }else{
    Y <- (2 * rB * sqrt(rA) / (sqrt(rA) + sqrt(rB)) + Re(zX2)) +
      sign(Im(zX1)) * 1i * soddyR
  }
  M <- fromCplx(Y * exp(1i * alpha) + zB)
  cAB <- Circle$new(M, soddyR)
  drawSphere(cAB, ...)
  list(
    list(type = "ccc", c1 = cAB, c2 = cA, c3 = cB),
    list(type = "ccl", cA = cA, cB = cAB, A = A, B = B),
    list(type = "ccl", cA = cAB, cB = cB, A = A, B = B)
  )
}

side.side.circle <- function(A, B, C, circle, ...){
  zA <- toCplx(A)
  zO <- toCplx(circle$center)
  vec <- zA - zO
  zP <- zO + circle$radius * vec / Mod(vec)
  P <- fromCplx(zP)
  OP <- P - circle$center
  onTangent <- P + c(-OP[2], OP[1])
  L1 <- Line$new(P, onTangent)
  P1 <- intersectionLineLine(L1, Line$new(A, C))
  P2 <- intersectionLineLine(L1, Line$new(A, B))
  incircle <- Triangle$new(A, P1, P2)$incircle()
  drawSphere(incircle, ...)
  list(
    list(type = "cll", A = A, B = B, C = C, circle = incircle),
    list(type = "ccl", cA = circle, cB = incircle, A = A, B = B),
    list(type = "ccl", cA = circle, cB = incircle, A = A, B = C)
  )
}

Newholes <- function(holes, color){
  newholes <- list()
  for(i in 1:3){
    hole <- holes[[i]]
    holeType <- hole[["type"]]
    if(holeType == "ccc"){
      x <- with(hole, innerSoddyCircle(c1, c2, c3, color = color))
    }else if(holeType == "ccl"){
      x <- with(hole, side.circle.circle(A, B, cA, cB, color = color))
    } else if (holeType == "cll"){
      x <- with(hole, side.side.circle(A, B, C, circle, color = color))
    }
    newholes <- c(newholes, list(x))
  }
  newholes
}

MalfattiCircles <- function(A, B, C){
  Triangle$new(A, B, C)$MalfattiCircles()
}

drawTriangularGasket <- function(mcircles, A, B, C, colors, depth){
  C1 <- mcircles[[1]]
  C2 <- mcircles[[2]]
  C3 <- mcircles[[3]]
  triangle <- cbind(rbind(A, B, C), 0)
  triangles3d(triangle, col = "yellow", alpha = 0.2)
  holes <- list(
    side.circle.circle(A, B, C1, C2, color = colors[1]),
    side.circle.circle(B, C, C2, C3, color = colors[1]),
    side.circle.circle(C, A, C3, C1, color = colors[1]),
    innerSoddyCircle(C1, C2, C3, color = colors[1]),
    side.side.circle(A, B, C, C1, color = colors[1]),
    side.side.circle(B, A, C, C2, color = colors[1]),
    side.side.circle(C, A, B, C3, color = colors[1])
  )

  for(d in 1:depth){
    n_holes <- length(holes)
    Holes <- list()
    for(i in 1:n_holes){
      Holes <- append(Holes, Newholes(holes[[i]], colors[d + 1]))
    }
    holes <- do.call(list, Holes)
  }
}

ApollonianChildren <- function(inversions, circles1){
  m <- length(inversions)
  n <- length(circles1)
  circles2 <- list()
  for(i in 1:n){
    circ <- circles1[[i]]
    k <- attr(circ, "inversion")
    for(j in 1:m){
      if (j != k){
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
  inversions[[n + 1]] <- inversionSwappingTwoCircles(c0, circles0[[n + 1]])
  # first generation of children
  circles1 <- list()
  for(i in 1:n){
    ip1 <- (i %% n) + 1
    for(j in 1:(n + 1)){
      if (j != i && j != ip1){
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
    allCircles[[i]] <- ApollonianChildren(inversions, allCircles[[i - 1]])
  }
  allCircles
}


drawCircularGasket <- function(c0, n, phi, shift, depth, colors){
  ApollonianCircles <- ApollonianGasket(c0, n, phi, shift, depth)
  for(i in 1:depth){
    for(circ in ApollonianCircles[[i]]){
      drawSphere(circ, color = colors[i])
    }
  }
}


library(viridisLite)
A <- c(-5, -4)
B <- c(5, -2)
C <- c(0, 6)
depth <- 3
colors <- viridis(depth + 1)
n1 <- 3
n2 <- 4
n3 <- 5
depth2 <- 3
phi1 <- 0.2
phi2 <- 0.3
phi3 <- 0.4
shift <- 0
colors2 <- plasma(depth2)
mcircles <- MalfattiCircles(A, B, C)


open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
drawTriangularGasket(mcircles, A, B, C, colors, depth)
drawCircularGasket(mcircles[[1]], n1, phi1, shift, depth2, colors2)
drawCircularGasket(mcircles[[2]], n2, phi2, shift, depth2, colors2)
drawCircularGasket(mcircles[[3]], n3, phi3, shift, depth2, colors2)
