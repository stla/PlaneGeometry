library(PlaneGeometry)

isoconj <- function(t, P){
  coords <- t$pointToTrilinear(P)
  x <- coords[1L]
  y <- coords[2L]
  z <- coords[3L]
  a <- t$a()
  b <- t$b()
  c <- t$c()
  t$trilinearToPoint(y*z, x*z, x*y)
  # t$trilinearToPoint(1/x, 1/y, 1/z)
#  t$trilinearToPoint(a*a*y*z, b*b*x*z, c*c*x*y)
}

tr <- Triangle$new(c(0,0), c(2,2), c(3,0))

H <- tr$orthocenter()
isoconj(tr, H)
tr$circumcenter()

( I <- tr$incenter() )
isoconj(tr, I)
