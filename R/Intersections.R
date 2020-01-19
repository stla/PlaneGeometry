#' Intersection of two circles
#' @description Return the intersection of two circles.
#'
#' @param circ1,circ2 two \code{Circle} objects
#'
#' @return \code{NULL} if there is no intersection,
#' a point if the circles touch, a list of two points if the circles meet at
#' two points, a circle if the two circles are identical
#' @export
intersectionCircleCircle <- function(circ1, circ2) {
  r1 <- circ1$radius; r2 <- circ2$radius
  center1 <- circ1$center; center2 <- circ2$center
  if(isTRUE(all.equal(c(center1,r1), c(center2,r2)))){
    return(circ1)
  }
  d2 <- c(crossprod(center1 - center2))
  sumRadii2 <- (r1+r2)^2
  if(d2 > sumRadii2 || d2 < abs(r1-r2)){
    return(NULL)
  }
  touch <- d2 == sumRadii2
  x <- center1[1L] - center2[1L]; y <- center1[2L] - center2[2L]
  lsquared <- x*x + y*y
  cosine <- (r2^2 - r1^2 - lsquared) / (r1*sqrt(4*lsquared))
  atg2 <- atan2(y, x)
  theta <- atg2 + acos(cosine)
  P1 <- center1 + r1 * c(cos(theta), sin(theta))
  if(touch) return(P1)
  theta <- atg2 - acos(cosine)
  list(
    P1 = P1,
    P2 = center1 + r1 * c(cos(theta), sin(theta))
  )
}
