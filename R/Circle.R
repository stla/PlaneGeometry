#' @title R6 class representing a circle
#'
#' @description A circle is given by a center and a radius,
#' named \code{center} and \code{radius}.
#'
#' @export
#' @importFrom R6 R6Class
Circle <- R6Class(

  "Circle",

  private = list(
    .center = c(NA_real_, NA_real_),
    .radius = NA_real_
  ),

  active = list(
    #' @field center get or set the center
    center = function(value) {
      if (missing(value)) {
        private[[".center"]]
      } else {
        center <- as.vector(value)
        stopifnot(
          is.numeric(center),
          length(center) == 2L,
          !any(is.na(center))
        )
        private[[".center"]] <- center
      }
    },

    #' @field radius get or set the radius
    radius = function(value) {
      if (missing(value)) {
        private[[".radius"]]
      } else {
        radius <- as.vector(value)
        stopifnot(
          is.numeric(radius),
          length(radius) == 1L,
          radius >= 0,
          !is.na(radius)
        )
        private[[".radius"]] <- radius
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Circle} object.
    #' @param center the center
    #' @param radius the radius
    #' @return A new \code{Circle} object.
    #' @examples circ <- Circle$new(c(1,1), 1)
    #' circ
    #' circ$center
    #' circ$center <- c(0,0)
    #' circ
    initialize = function(center, radius) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center))
      )
      radius <- as.vector(radius)
      stopifnot(
        is.numeric(radius),
        length(radius) == 1L,
        radius >= 0,
        !is.na(radius)
      )
      private[[".center"]] <- center
      private[[".radius"]] <- radius
    },

    #' @description Show instance of a circle object.
    #' @param ... ignored
    #' @examples Circle$new(c(0,0), 2)
    print = function(...) {
      cat("Circle:\n")
      cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
    },

    #' @description Power of a point with respect to the circle.
    #' @param M point
    #' @return A number.
    power = function(M) {
      private[[".radius"]] -> radius
      c(crossprod(M - private[[".center"]])) - radius*radius
    },

    #' @description Radical center of two circles.
    #' @param circ2 a \code{Circle} object
    #' @seealso \code{\link{radicalCenter}} for the radical center of three circles.
    radicalCenter = function(circ2){
      C1 <- primitive[[".center"]]; C2 <- circ2$center
      k < primitive[[".radius"]]^2 - circ2$radius^2;
      C1_C2 <- C2 - C1
      C1C2sqr <- c(crossprod(C1_C2))
      K <- if(C1C2sqr == 0){
        c(Inf, Inf)
      }else{
        (C1+C2)/2 + k/2 * C1_C2/C1C2sqr
      }
      c(C1[1L], K/C1[1L]) # quid if C1[1] = 0 ?
    },

    #' @description Radical axis of two circles.
    #' @param circ2 a \code{Circle} object
    #' @return A \code{Line} object.
    radicalAxis = function(circ2){
      C1 <- primitive[[".center"]]; C2 <- circ2$center
      if(isTRUE(all.equal(C1,C2))){
        stop("The two circles must have distinct centers.")
      }
      l <- Line$new(C1, C2)
      R <- self$radicalCenter(circ2)
      l$perpendicular(R, TRUE, TRUE)
    }

  )
)

#' Radical center
#' Returns the radical center of three circles.
#'
#' @param circ1,circ2,circ3 \code{Circle} objects
#'
#' @return A point.
#' @export
radicalCenter <- function(circ1, circ2, circ3){
  l1 <- circ1$radicalAxis(circ2)
  l2 <- circ1$radicalAxis(circ3)
  .LineLineIntersection(l1$A, l1$B, l2$A, l2$B)
}
