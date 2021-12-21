soddyCircle <- function(cA, cB, cC){
  tr <- Triangle$new(cA$center, cB$center, cC$center)
  edp <- tr$equalDetourPoint()
  r1 <- cA$radius
  r2 <- cB$radius
  r3 <- cC$radius
  r <- 1/(1/r1+1/r2+1/r3+2*sqrt(1/r1/r2+1/r2/r3+1/r3/r1))
  Circle$new(edp, r)
}

apollony <- function(c1, c2, c3, n){
  soddycircle <- soddyCircle(c1, c2, c3)
  if(n == 1){
    soddycircle
  }else{
    c(
      apollony(c1, c2, soddycircle, n-1),
      apollony(c1, soddycircle, c3, n-1),
      apollony(soddycircle, c2, c3, n-1)
    )
  }
}

fractal <- function(n){
  c1 = Circle$new(c(1, -1/sqrt(3)), 1)
  c2 = Circle$new(c(-1, -1/sqrt(3)), 1)
  c3 = Circle$new(c(0, sqrt(3) - 1/sqrt(3)), 1)
  do.call(c, lapply(1:n, function(i) apollony(c1, c2, c3, i)))
}


circs <- fractal(4)

plot(NULL, type = "n", xlim = c(-1,1), ylim = c(-1,1), asp = 1)
lapply(circs, draw)








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




# plot ####
open3d(windowRect = c(50, 50, 562, 562))
bg3d(color = "#363940")
view3d(35, 60, zoom = 0.95)
for(circ in circs){
  drawSphere(circ, color = "darkred")
}
# animation ####
movie3d(
  spin3d(axis = c(0, 0, 1), rpm = 15),
  duration = 4, fps = 15,
  movie = "Apollony", dir = ".",
  convert = "magick convert -dispose previous -loop 0 -delay 1x%d %s*.png %s.%s",
  startTime = 1/60
)
