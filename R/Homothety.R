#' @title R6 class representing a homothety
#'
#' @description A homothety is given by a center and a scale factor.
#'
#' @export
#' @importFrom R6 R6Class
Homothety <- R6Class(

  "Homothety",

  private = list(
    .center = c(NA_real_, NA_real_),
    .scale = NA_real_
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
          !any(is.na(center)),
          all(is.finite(center))
        )
        private[[".center"]] <- center
      }
    },

    #' @field scale get or set the scale factor of the homothety
    scale = function(value) {
      if (missing(value)) {
        private[[".scale"]]
      } else {
        scale <- as.vector(value)
        stopifnot(
          is.numeric(scale),
          length(scale) == 1L,
          !is.na(scale),
          is.finite(scale)
        )
        private[[".scale"]] <- scale
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Homothety} object.
    #' @param center a point, the center of the homothety
    #' @param scale a number, the scale factor of the homothety
    #' @return A new \code{Homothety} object.
    #' @examples Homothety$new(c(1,1), 2)
    initialize = function(center, scale) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      stopifnot(
        is.numeric(scale),
        length(scale) == 1L,
        !is.na(scale),
        is.finite(scale)
      )
      private[[".center"]] <- center
      private[[".scale"]] <- scale
    },

    #' @description Show instance of a \code{Homothety} object.
    #' @param ... ignored
    print = function(...) {
      private[[".center"]] -> center
      private[[".scale"]] -> scale
      cat("Homothety:\n")
      cat("    center: ", toString(center), "\n", sep = "")
      cat("     scale: ", toString(scale), "\n", sep = "")
    },

    #' @description Transform a point or several points by the reference homothety.
    #' @param M a point or a two-column matrix of points, one point per row
    transform = function(M) {
      if(is.matrix(M)){
        stopifnot(
          ncol(M) == 2L,
          is.numeric(M)
        )
      }else{
        M <- as.vector(M)
        stopifnot(
          is.numeric(M),
          length(M) == 2L
        )
        M <- rbind(M)
      }
      stopifnot(
        !any(is.na(M)),
        all(is.finite(M))
      )
      private[[".center"]] -> O
      private[[".scale"]] -> s
      out <- t((1-s)*O + s*t(M))
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description Transform a circle by the reference homothety.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object.
    transformCircle = function(circ) {
      Circle$new(self$transform(circ$center),
                 abs(private[[".scale"]])*circ$radius)
    },

    #' @description Augmented matrix of the homothety.
    #' @return A 3x3 matrix.
    #' @examples H <- Homothety$new(c(1,1), 2)
    #' P <- c(1,5)
    #' H$transform(P)
    #' H$getMatrix() %*% c(P,1)
    getMatrix = function(){
      private[[".center"]] -> O
      private[[".scale"]] -> s
      W <- (1-s)*O
      cbind(rbind(diag(c(s,s)), 0), c(W, 1))
    },

    #' @description Convert the reference homothety to an \code{Affine} object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    }
  )
)
