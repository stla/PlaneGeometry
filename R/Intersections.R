#' Intersection of two circles
#' @description Return the intersection of two circles.
#'
#' @param circ1,circ2 two \code{Circle} objects
#' @param epsilon a small positive number used for the numerical accuracy
#'
#' @return \code{NULL} if there is no intersection,
#' a point if the circles touch, a list of two points if the circles meet at
#' two points, a circle if the two circles are identical.
#' @export
intersectionCircleCircle <- function(circ1, circ2,
                                     epsilon = sqrt(.Machine$double.eps)) {
  r1 <- circ1$radius; r2 <- circ2$radius
  center1 <- circ1$center; center2 <- circ2$center
  if(isTRUE(all.equal(c(center1,r1), c(center2,r2)))){
    return(circ1)
  }
  d2 <- c(crossprod(center1 - center2))
  sumRadii2 <- (r1+r2)^2
  if(d2 > sumRadii2 + epsilon || d2 < (r1-r2)^2 - epsilon){
    return(NULL)
  }
  touch <- sumRadii2 - d2 < epsilon || d2 - (r1-r2)^2 < epsilon
  x <- center1[1L] - center2[1L]; y <- center1[2L] - center2[2L]
  lsquared <- x*x + y*y
  cosine <- max(min((r2^2 - r1^2 - lsquared) / (r1*sqrt(4*lsquared)), 1), -1)
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

#' Intersection of a circle and a line
#' @description Return the intersection of a circle and a line.
#'
#' @param circ a \code{Circle} object
#' @param line a \code{Line} object
#' @param strict logical, whether to take into account \code{line$extendA} and
#' \code{line$extendB} if they are not both \code{TRUE}
#'
#' @return \code{NULL} if there is no intersection;
#' a point if the infinite line is tangent to the circle, or \code{NULL}
#' if \code{strict=TRUE} and the point is not on the line (segment or half-line);
#' a list of two points if the circle and the infinite line meet at
#' two points, when \code{strict=FALSE}; if \code{strict=TRUE} and the line is
#' a segment or a half-line, this can return \code{NULL} or a single point.
#'
#' @examples circ <- Circle$new(c(1,1), 2)
#' line <- Line$new(c(2,-2), c(1,2), FALSE, FALSE)
#' intersectionCircleLine(circ, line)
#' intersectionCircleLine(circ, line, strict = TRUE)
#' @export
intersectionCircleLine <- function(circ, line, strict = FALSE){
  C <- circ$center
  intersections <- .CircleLineIntersection00(line$A - C, line$B - C, circ$radius)
  if(is.null(intersections)) return(NULL)
  if(is.list(intersections)){
    I1I2 <- lapply(intersections, function(I){I + C})
    if(strict && (!line$extendA || !line$extendB)){
      I1 <- I1I2[[1L]]; I2 <- I1I2[[2L]]
      ontheline1 <-
        suppressMessages(line$includes(I1, strict = TRUE, checkCollinear = FALSE))
      ontheline2 <-
        suppressMessages(line$includes(I2, strict = TRUE, checkCollinear = FALSE))
      if(ontheline1 && ontheline2){
        return(I1I2)
      }else if(ontheline1){
        message(
          sprintf(
            paste0(
              "The infinite line meets the circle at two points, but one of them",
              " is not on the %s."
            ), ifelse(line$extendA || line$extendB, "half-line", "segment")
          )
        )
        return(I1)
      }else if(ontheline2){
        message(
          sprintf(
            paste0(
              "The infinite line meets the circle at two points, but one of them",
              " is not on the %s."
            ), ifelse(line$extendA || line$extendB, "half-line", "segment")
          )
        )
        return(I2)
      }else{
        message(
          sprintf(
            paste0(
              "The infinite line meets the circle at two points, but none of them",
              " is on the %s."
            ), ifelse(line$extendA || line$extendB, "half-line", "segment")
          )
        )
        return(NULL)
      }
    }else{
      return(I1I2)
    }
  }
  I <- intersections + C
  if(strict && (!line$extendA || !line$extendB)){
    ontheline <-
      suppressMessages(line$includes(I, strict = TRUE, checkCollinear = FALSE))
    if(ontheline){
      I
    }else{
      message(
        sprintf(
          paste0(
            "The infinite line is tangent to the circle, but the tangency point",
            " does not belong to the %s."
          ), ifelse(line$extendA || line$extendB, "half-line", "segment")
        )
      )
      NULL
    }
  }else{
    I
  }
}

# TODO intersectionLineLine with strict argument

#' @export
intersectionLineLine <- function(line1, line2, strict = FALSE){
  .LineLineIntersection(line1$A, line1$B, line2$A, line2$B)
}
