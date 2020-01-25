#' @title R6 class representing a rotation
#'
#' @description A rotation is given by an angle (\code{theta}) and a center.
#'
#' @export
#' @importFrom R6 R6Class
Rotation <- R6Class(

  "Rotation",

  private = list(
    .theta = NA_real_,
    .center = c(NA_real_, NA_real_),
    .degrees = NA
  ),

  active = list(
    #' @field theta get or set the angle of the rotation
    theta = function(value) {
      if (missing(value)) {
        private[[".theta"]]
      } else {
        theta <- as.vector(value)
        stopifnot(
          is.numeric(theta),
          length(theta) == 1L,
          !is.na(theta),
          is.finite(theta)
        )
        private[[".theta"]] <- theta
      }
    },

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

    #' @field degrees get or set the \code{degrees} field
    degrees = function(value) {
      if (missing(value)) {
        private[[".degrees"]]
      } else {
        degrees <- as.vector(value)
        stopifnot(
          is.logical(degrees),
          length(degrees) == 1L,
          !is.na(degrees)
        )
        private[[".degrees"]] <- degrees
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Rotation} object.
    #' @param theta a number, the angle of the rotation
    #' @param center a point, the center of the rotation
    #' @param degrees logical, whether \code{theta} is given in degrees
    #' @return A new \code{Rotation} object.
    #' @examples Rotation$new(60, c(1,1))
    initialize = function(theta, center, degrees = TRUE) {
      theta <- as.vector(theta)
      stopifnot(
        is.numeric(theta),
        length(theta) == 1L,
        !is.na(theta),
        is.finite(theta)
      )
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      degrees <- as.vector(degrees)
      stopifnot(
        is.logical(degrees),
        length(degrees) == 1L,
        !is.na(degrees)
      )
      private[[".theta"]] <- theta
      private[[".center"]] <- center
      private[[".degrees"]] <- degrees
    },

    #' @description Show instance of a \code{Rotation} object.
    #' @param ... ignored
    print = function(...) {
      private[[".theta"]] -> theta
      private[[".center"]] -> center
      private[[".degrees"]] -> degrees
      cat("Rotation:\n")
      cat("    theta: ",
          sprintf("%s %s", theta,
                  ifelse(degrees,
                         ifelse(theta %in% c(0,1), "degree", "degrees"),
                         ifelse(theta %in% c(0,1), "radian", "radians"))
          ), "\n", sep = "")
      cat("   center: ", toString(center), "\n", sep = "")
    },

    #' @description Rotate a point or several points.
    #' @param M a point or a two-column matrix of points, one point per row
    rotate = function(M) {
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
      private[[".theta"]] -> theta
      private[[".center"]] -> O
      private[[".degrees"]] -> degrees
      if(degrees) theta <- theta * pi/180
      costheta <- cos(theta); sintheta <- sin(theta)
      Mt <- sweep(M, 2L, O)
      out <- sweep(
        cbind(
          costheta*Mt[,1L]-sintheta*Mt[,2L],
          sintheta*Mt[,1L]+costheta*Mt[,2L]
        ),
        2L, O, "+")
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description An alias of \code{rotate}.
    #' @param M a point or a two-column matrix of points, one point per row
    transform = function(M){
      self$rotate(M)
    },

    #' @description Rotate a circle.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object.
    rotateCircle = function(circ) {
      Circle$new(self$rotate(circ$center), circ$radius)
    },

    #' @description An alias of \code{rotateCircle}.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object.
    transformCircle = function(circ) {
      self$rotateCircle(circ)
    },

    #' @description Rotate a line.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    rotateLine = function(line) {
      Line$new(self$rotate(line$A), self$rotate(line$B),
               line$extendA, line$extendB)
    },

    #' @description An alias of \code{rotateLine}.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    transformLine = function(line) {
      self$rotateLine(line)
    },

    #' @description Augmented matrix of the rotation.
    #' @return A 3x3 matrix.
    #' @examples R <- Rotation$new(60, c(1,1))
    #' P <- c(1,5)
    #' R$rotate(P)
    #' R$getMatrix() %*% c(P,1)
    getMatrix = function(){
      private[[".theta"]] -> theta
      private[[".center"]] -> O
      private[[".degrees"]] -> degrees
      if(degrees) theta <- theta * pi / 180
      costheta <- cos(theta); sintheta <- sin(theta)
      W <- c(
        O[1]*(1-costheta) + O[2]*sintheta,
        -O[1]*sintheta + O[2]*(1-costheta)
      )
      cbind(c(costheta, sintheta, 0), c(-sintheta, costheta, 0), c(W, 1))
    },

    #' @description Convert the reference rotation to an \code{Affine} object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    }


  )
)
