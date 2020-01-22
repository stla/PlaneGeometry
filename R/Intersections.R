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
    return(circ1$clone(deep = TRUE))
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


#' Intersection of two lines
#' @description Return the intersection of two lines.
#'
#' @param line1,line2 two \code{Line} objects
#' @param strict logical, whether to take into account the extensions of the
#' lines (\code{extendA} and \code{extendB})
#' @return If \code{strict = FALSE} this returns either a point, or \code{NULL}
#' if the lines are parallel, or a bi-infinite line if the two lines coincide.
#' If \code{strict = TRUE}, this can also return a half-infinite line or
#' a segment.
#'
#' @export
intersectionLineLine <- function(line1, line2, strict = FALSE){
  if(line1$isEqual(line2)){
    if(line1$extendA && line1$extendB && line2$extendA && line2$extendB){
      return(line1$clone(deep = TRUE))
    }else{
      if(!strict){
#        line1$extendA <- line1$extendB <- TRUE # should I do "clone" ?
        return(Line$new(line1$A, line1$B, TRUE, TRUE))
      }
      # case 1: one bi-infinite line
      if(line1$extendA && line1$extendB){
        return(line2$clone(deep = TRUE))
      }
      if(line2$extendA && line2$extendB){
        return(line1$clone(deep = TRUE))
      }
      # case 2: two half-lines
      A <- line1$A; B <- line1$B; C <- line2$A; D <- line2$B
      if(line1$extendA && line2$extendA){
        # origin line1: B; origin line2 : D
        extend1 <- A; extend2 <- C
        origin1 <- B; origin2 <- D
        sameDirection <- crossprod(extend1-origin1, extend2-origin2) > 0
        if(sameDirection){
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(line2$clone(deep = TRUE))
          }
          return(line1$clone(deep = TRUE))
        }else{
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(Line$new(origin1, origin2, FALSE, FALSE))
          }
          return(NULL)
        }
      }
      if(line1$extendA && line2$extendB){
        extend1 <- A; extend2 <- D
        origin1 <- B; origin2 <- C
        sameDirection <- crossprod(extend1-origin1, extend2-origin2) > 0
        if(sameDirection){
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(line2$clone(deep = TRUE))
          }
          return(line1$clone(deep = TRUE))
        }else{
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(Line$new(origin1, origin2, FALSE, FALSE))
          }
          return(NULL)
        }
      }
      if(line1$extendB && line2$extendA){
        extend1 <- B; extend2 <- C
        origin1 <- A; origin2 <- D
        sameDirection <- crossprod(extend1-origin1, extend2-origin2) > 0
        if(sameDirection){
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(line2$clone(deep = TRUE))
          }
          return(line1$clone(deep = TRUE))
        }else{
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(Line$new(origin1, origin2, FALSE, FALSE))
          }
          return(NULL)
        }
      }
      if(line1$extendB && line2$extendB){
        extend1 <- B; extend2 <- D
        origin1 <- A; origin2 <- C
        sameDirection <- crossprod(extend1-origin1, extend2-origin2) > 0
        if(sameDirection){
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(line2$clone(deep = TRUE))
          }
          return(line1$clone(deep = TRUE))
        }else{
          if(suppressMessages(line1$includes(origin2, strict = TRUE, checkCollinear = FALSE))){
            return(Line$new(origin1, origin2, FALSE, FALSE))
          }
          return(NULL)
        }
      }
      # case 3: one half-line and one segment
      if(line1$extendA){
        extend <- A; origin <- B
        S1 <- C; S2 <- D
        line <- Line$new(extend, origin, TRUE, FALSE)
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE)) &&
           suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, S2, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, origin, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S2, origin, FALSE, FALSE))
        }
        return(NULL)
      }
      if(line1$extendB){
        extend <- B; origin <- A
        S1 <- C; S2 <- D
        line <- Line$new(extend, origin, TRUE, FALSE)
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE)) &&
           suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, S2, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, origin, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S2, origin, FALSE, FALSE))
        }
        return(NULL)
      }
      if(line2$extendA){
        extend <- C; origin <- D
        S1 <- A; S2 <- B
        line <- Line$new(extend, origin, TRUE, FALSE)
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE)) &&
           suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, S2, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S1, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S1, origin, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S2, strict = TRUE, checkCollinear = FALSE))){
          return(Line$new(S2, origin, FALSE, FALSE))
        }
        return(NULL)
      }
      if(line2$extendB){
        extend <- D; origin <- C
        S1 <- A; S2 <- B
        line <- Line$new(extend, origin, TRUE, FALSE)
        if(suppressMessages(line$includes(S1, checkCollinear = FALSE)) &&
           suppressMessages(line$includes(S2, checkCollinear = FALSE))){
          return(Line$new(S1, S2, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S1, checkCollinear = FALSE))){
          return(Line$new(S1, origin, FALSE, FALSE))
        }
        if(suppressMessages(line$includes(S2, checkCollinear = FALSE))){
          return(Line$new(S2, origin, FALSE, FALSE))
        }
        return(NULL)
      }
      # case 4: two segments
      # # https://matlabgeeks.com/tips-tutorials/computational-geometry/find-intersection-of-two-lines-in-matlab/
      # p <- A; r <- B-A
      # q <- C; s <- D-C
      # p <- C; r <- D-C
      # q <- A; s <- B-A
      # cross <- function(A, B) det(cbind(A,B))
      # r_cross_s <- cross(r, s) # = 0
      # q_p_cross_r = cross(q-p, r)
      # if(abs(q_p_cross_r) > sqrt(.Machine$double.eps)){
      #   return(NULL)
      # }
      # t0 <- abs(c(crossprod(q-p,r))) / c(crossprod(r))
      # u0 <- abs(c(crossprod(q-p,s))) / c(crossprod(s))
      # cat("t0: ", t0, "\n"); cat("u0: ", u0, "\n")
      # if(t0 <=1 && u0 <= 1){
      #   return(Line$new(q+u0*s, p+t0*r, FALSE, FALSE))
      # }
      # return(NULL)
      if(suppressMessages(line1$includes(C, strict = TRUE, checkCollinear = FALSE)) &&
         suppressMessages(line1$includes(D, strict = TRUE, checkCollinear = FALSE))){
        return(line2$clone(deep = TRUE))
      }
      if(suppressMessages(line2$includes(A, strict = TRUE, checkCollinear = FALSE)) &&
         suppressMessages(line2$includes(B, strict = TRUE, checkCollinear = FALSE))){
        return(line1$clone(deep = TRUE))
      }
      if(line1$directionAndOffset()$direction %% pi == 0){
        p <- min(A[2L],B[2L]); q <- max(A[2L],B[2L])
        i <- match(p, c(A[2L],B[2L]))
        r <- min(C[2L],D[2L]); s <- max(C[2L],D[2L])
        j <- match(r, c(C[2L],D[2L]))
      }else{
        p <- min(A[1L],B[1L]); q <- max(A[1L],B[1L])
        i <- match(p, c(A[1L],B[1L]))
        r <- min(C[1L],D[1L]); s <- max(C[1L],D[1L])
        j <- match(r, c(C[1L],D[1L]))
      }
      if(q < r) return(NULL)
      if(q == r){
        if(i == 1L) return(B) else return(A)
      }
      if(i == 1L){
        P <- A
        Q <- B
      }else{
        P <- B
        Q <- A
      }
      if(j == 1L){
        R <- C
        S <- D
      }else{
        R <- D
        S <- C
      }
      if(suppressMessages(line1$includes(R, strict = TRUE, checkCollinear = FALSE))){
        return(Line$new(R, Q, FALSE, FALSE))
      }
      if(suppressMessages(line1$includes(S, strict = TRUE, checkCollinear = FALSE))){
        return(Line$new(P, S, FALSE, FALSE))
      }
    }
  }
  if(line1$isParallel(line2)){
    message("Distinct parallel lines")
    return(NULL)
  }
  I <- .LineLineIntersection(line1$A, line1$B, line2$A, line2$B)
  if(!strict){
    return(I)
  }
  if(suppressMessages(line1$includes(I, strict = TRUE, checkCollinear = FALSE)) &&
     suppressMessages(line2$includes(I, strict = TRUE, checkCollinear = FALSE))){
    I
  }else{
    NULL
  }
}
