#' @title R6 class representing an axis-scaling
#'
#' @description An axis-scaling is given by a center, and two scale factors
#' \code{sx} and \code{sy}, one for the x-axis and one for the y-axis.
#'
#' @export
#' @importFrom R6 R6Class
ScalingXY <- R6Class(

  "ScalingXY",

  private = list(
    .center = c(NA_real_, NA_real_),
    .sx = NA_real_,
    .sy = NA_real_
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

    #' @field sx get or set the scale factor of the x-axis
    sx = function(value) {
      if (missing(value)) {
        private[[".sx"]]
      } else {
        sx <- as.vector(value)
        stopifnot(
          is.numeric(sx),
          length(sx) == 1L,
          !is.na(sx),
          is.finite(sx)
        )
        private[[".sx"]] <- sx
      }
    },

    #' @field sy get or set the scale factor of the y-ayis
    sy = function(value) {
      if (missing(value)) {
        private[[".sy"]]
      } else {
        sy <- as.vector(value)
        stopifnot(
          is.numeric(sy),
          length(sy) == 1L,
          !is.na(sy),
          is.finite(sy)
        )
        private[[".sy"]] <- sy
      }
    }
  ),

  public = list(
    #' @description Create a new \code{ScalingXY} object.
    #' @param center a point, the center of the scaling
    #' @param sx a number, the scale factor of the x-axis
    #' @param sy a number, the scale factor of the y-axis
    #' @return A new \code{ScalingXY} object.
    #' @examples ScalingXY$new(c(1,1), 4, 2)
    initialize = function(center, sx, sy) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      stopifnot(
        is.numeric(sx),
        length(sx) == 1L,
        !is.na(sx),
        is.finite(sx)
      )
      stopifnot(
        is.numeric(sy),
        length(sy) == 1L,
        !is.na(sy),
        is.finite(sy)
      )
      private[[".center"]] <- center
      private[[".sx"]] <- sx
      private[[".sy"]] <- sy
    },

    #' @description Show instance of a \code{ScalingXY} object.
    #' @param ... ignored
    print = function(...) {
      private[[".center"]] -> center
      private[[".sx"]] -> sx
      private[[".sy"]] -> sy
      cat("Axis-scaling:\n")
      cat("    center: ", toString(center), "\n", sep = "")
      cat("   x-scale: ", toString(sx), "\n", sep = "")
      cat("   y-scale: ", toString(sy), "\n", sep = "")
    },

    #' @description Transform a point or several poinys by the reference axis-scaling.
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
      private[[".sx"]] -> sx
      private[[".sy"]] -> sy
      out <- t(c(sx,sy) * (t(M)-O) + O)
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description Augmented matrix of the axis-scaling.
    #' @return A 3x3 matrix.
    #' @examples S <- ScalingXY$new(c(1,1), 4, 2)
    #' P <- c(1,5)
    #' S$transform(P)
    #' S$getMatrix() %*% c(P,1)
    getMatrix = function(){
      private[[".sx"]] -> sx; private[[".sy"]] -> sy
      cbind(rbind(diag(c(sx,sy)),0), c(self$transform(c(0,0)),1))
    },

    #' @description Convert the reference axis-scaling to an \code{Affine}
    #' object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    }

    # #' @description Scale an ellipse. The result is an ellipse.
    # #' @param ell an \code{Ellipse} object
    # #' @return An \code{Ellipse} object.
    # scaleEllipse = function(ell){
    #   if(is(ell, "Circle")) ell <- .circleAsEllipse(ell)
    #   private[[".center"]] -> Q
    #   private[[".sx"]] -> sx; private[[".sy"]] -> sy
    #   C <- ell$center#; a <- ell$a; b <- ell$b; alpha <- ell$alpha
    #   O <- self$transform(C)
    #   v1 <- ell$semiMajorAxis()
    #   v2 <- ell$semiMinorAxis()
    #   w1 <- self$transform(v1$B) - O## c(sx,sy)*(v1$A-v1$B)
    #   w2 <- c(sx,sy)*(v2$B-v2$A)
    #   # l'image d'un axis n'est pas l'axis de l'image !
    #   a <- .vlength(w1)
    #   b <- .vlength(w2)
    #   alpha <- atan2(w1[2L],w1[1L]) * 180/pi
    #   Ellipse$new(O, a, b, alpha %% 180, degrees = TRUE)
    # }
  )
)
