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

    #' @description Check whether the circle equals another circle.
    #' @param circ a \code{Circle object}
    isEqual = function(circ){
      c0 <- private[[".center"]]; r0 <- private[[".radius"]]
      c1 <- circ$center; r1 <- circ$radius
      isTRUE(all.equal(c(c0[1L],c0[2L],r0), c(c1[1L],c1[2L],r1)))
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
      C1 <- private[[".center"]]; C2 <- circ2$center
      k <- private[[".radius"]]^2 - circ2$radius^2;
      C1_C2 <- C2 - C1
      C1C2sqr <- c(crossprod(C1_C2))
      K <- if(C1C2sqr == 0){
        c(Inf, Inf)
      }else{
        (C1+C2)/2 + k/2 * C1_C2/C1C2sqr
      }
      K/C1[1L] # quid if C1[1] = 0 ?
    },

    #' @description Radical axis of two circles.
    #' @param circ2 a \code{Circle} object
    #' @return A \code{Line} object.
    radicalAxis = function(circ2){
      C1 <- private[[".center"]]; C2 <- circ2$center
      if(isTRUE(all.equal(C1,C2))){
        stop("The two circles must have distinct centers.")
      }
      l <- Line$new(C1, C2, TRUE, TRUE)
      R <- self$radicalCenter(circ2)
      l$perpendicular(R, TRUE, TRUE)
    },

    #' @description Rotate the circle.
    #' @param alpha angle of rotation
    #' @param O center of rotation
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return A \code{Circle} object.
    rotate = function(alpha, O, degrees = TRUE){
      alpha <- as.vector(alpha)
      stopifnot(
        is.numeric(alpha),
        length(alpha) == 1L,
        !is.na(alpha)
      )
      O <- as.vector(O)
      stopifnot(
        is.numeric(O),
        length(O) == 2L,
        !any(is.na(O))
      )
      if(degrees){
        alpha <- alpha * pi/180
      }
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      At <- private[[".center"]] - O
      RAt <- c(cosalpha*At[1L]-sinalpha*At[2L], sinalpha*At[1L]+cosalpha*At[2L])
      Circle$new(RAt + O, private[[".radius"]])
    },

    #' @description Translate the circle.
    #' @param v the vector of translation
    #' @return A \code{Circle} object.
    translate = function(v){
      v <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(v) == 2L,
        !any(is.na(v))
      )
      Circle$new(private[[".center"]] + v, private[[".radius"]])
    },

    #' @description Invert the circle.
    #' @param inversion an \code{Inversion} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invert = function(inversion){
      inversion$invertCircle(self)
    }


  )
)

#' Radical center
#' @description Returns the radical center of three circles.
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
