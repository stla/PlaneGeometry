library(PlaneGeometry)
library(rgl)

# the spheres in rgl, obtained with the `spheres3d` function, are not smooth;
# the way we use below provides pretty spheres
unitSphere <- subdivision3d(icosahedron3d(), depth = 4)
unitSphere$vb[4, ] <-
  apply(unitSphere$vb[1:3, ], 2, function(x) sqrt(sum(x * x)))
unitSphere$normals <- unitSphere$vb
drawSphere <- function(circle, ...){
  center <- circle$center
  radius <- abs(circle$radius)
  sphere <- scale3d(unitSphere, radius, radius, radius)
  shade3d(translate3d(sphere, center[1], center[2], 0), ...)
}

iteration <- function(circlesWithIndicator, inversions) {
  out <- list()
  for(j in seq_along(circlesWithIndicator)) {
    circle <- circlesWithIndicator[[j]][["circle"]]
    indic  <- circlesWithIndicator[[j]][["indic"]]
    for(i in 1L:4L) {
      if(i != indic) {
        circleWithIndicator <- list(
          "circle" = inversions[[i]]$invertCircle(circle),
          "indic"  = i
        )
        out <- append(out, list(circleWithIndicator))
      }
    }
  }
  out
}

gasket <- function(circlesWithIndicator, inversions, depth, colors) {
  if(depth > 0){
    circlesWithIndicator <- iteration(circlesWithIndicator, inversions)
    for(i in seq_along(circlesWithIndicator)){
      drawSphere(circlesWithIndicator[[i]]$circle, color = colors[1L])
    }
    colors <- colors[-1L]
    gasket(circlesWithIndicator, inversions, depth-1L, colors)
  }
}

drawGasket <- function(triangle, depth, colors) {
  Mcircles <- triangle$MalfattiCircles()
  Mtriangle <- Triangle$new(
    Mcircles[[1L]]$center, Mcircles[[2L]]$center, Mcircles[[3L]]$center
  )
  soddyO <- Mtriangle$outerSoddyCircle()
  Mcircles <- append(Mcircles, list(soddyO))
  for(i in 1L:4L){
    lines3d(
      cbind(Mcircles[[i]]$asEllipse()$path(), 0),
      color = "black", lwd = 2
    )
  }
  inversions <- vector("list", 4L)
  circlesWithIndicator <- vector("list", 4L)
  inversions[[1L]] <- inversionFixingThreeCircles(
    soddyO, Mcircles[[2L]], Mcircles[[3L]]
  )
  inversions[[2L]] <- inversionFixingThreeCircles(
    soddyO, Mcircles[[1L]], Mcircles[[3L]]
  )
  inversions[[3L]] <- inversionFixingThreeCircles(
    soddyO, Mcircles[[1L]], Mcircles[[2L]]
  )
  inversions[[4L]] <- inversionFixingThreeCircles(
    Mcircles[[1L]], Mcircles[[2L]], Mcircles[[3L]]
  )
  for(i in 1L:4L) {
    circlesWithIndicator[[i]] <-
      list("circle" = inversions[[i]]$invertCircle(Mcircles[[i]]), "indic" = i)
    drawSphere(circlesWithIndicator[[i]]$circle, color = colors[1L])
  }
  colors <- colors[-1L]
  gasket(circlesWithIndicator, inversions, depth, colors)
}

CircularMalfattiGasket <- function(C, depth, colors) {
  A <- c(0,0); B <- c(1,0)
  t <- Triangle$new(A, B, C)
  Mcircles = t$MalfattiCircles()
  Mtriangle <- Triangle$new(
    Mcircles[[1L]]$center, Mcircles[[2L]]$center, Mcircles[[3L]]$center
  )
  soddyO <- Mtriangle$outerSoddyCircle()
  center <- soddyO$center; radius = -soddyO$radius
  A1 = (A-center)/radius; B1 = (B-center)/radius; C1 = (C-center)/radius;
  t1 = Triangle$new(A1, B1, C1);
  drawGasket(t1, depth, colors)
}

open3d(windowRect = 50 + c(0, 0, 900, 300))
mfrow3d(1, 3)
view3d(0, 0, zoom = 0.7)
CircularMalfattiGasket(
  C = c(0, sqrt(3/2)), depth = 2L,
  colors = c("yellow", "orangered", "darkmagenta")
)
next3d()
view3d(0, 0, zoom = 0.7)
CircularMalfattiGasket(
  C = c(1, sqrt(3/2)), depth = 2L,
  colors = c("yellow", "orangered", "darkmagenta")
)
next3d()
view3d(0, 0, zoom = 0.7)
CircularMalfattiGasket(
  C = c(2, sqrt(3/2)), depth = 2L,
  colors = c("yellow", "orangered", "darkmagenta")
)

