#' @title R6 class representing a (non-uniform) scaling
#'
#' @description A (non-uniform) scaling is given by a center, a direction,
#' and a scale factor.
#'
#' @references R. Goldman,
#' \emph{An Integrated Introduction to Computer Graphics and Geometric Modeling}.
#' CRC Press, 2009.
#'
#' @export
#' @importFrom R6 R6Class
Scaling <- R6Class(

  "Scaling",

  private = list(
    .center = c(NA_real_, NA_real_),
    .direction = c(NA_real_, NA_real_),
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

    #' @field direction get or set the direction
    direction = function(value) {
      if (missing(value)) {
        private[[".direction"]]
      } else {
        w <- as.vector(value)
        stopifnot(
          is.numeric(w),
          length(w) == 2L,
          !any(is.na(center)),
          all(is.finite(w)),
          any(w != 0)
        )
        private[[".direction"]] <- w
      }
    },

    #' @field scale get or set the scale factor
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
    #' @description Create a new \code{Scaling} object.
    #' @param center a point, the center of the scaling
    #' @param direction a vector, the direction of the scaling
    #' @param scale a number, the scale factor
    #' @return A new \code{Scaling} object.
    #' @examples Scaling$new(c(1,1), c(1,3), 2)
    initialize = function(center, direction, scale) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      direction <- as.vector(direction)
      stopifnot(
        is.numeric(direction),
        length(direction) == 2L,
        !any(is.na(direction)),
        all(is.finite(direction)),
        any(direction != 0)
      )
      stopifnot(
        is.numeric(scale),
        length(scale) == 1L,
        !is.na(scale),
        is.finite(scale)
      )
      private[[".center"]] <- center
      private[[".direction"]] <- direction
      private[[".scale"]] <- scale
    },

    #' @description Show instance of a \code{Scaling} object.
    #' @param ... ignored
    print = function(...) {
      private[[".center"]] -> center
      private[[".direction"]] -> w
      private[[".scale"]] -> scale
      cat("Scaling:\n")
      cat("    center: ", toString(center), "\n", sep = "")
      cat(" direction: ", toString(w), "\n", sep = "")
      cat("     scale: ", toString(scale), "\n", sep = "")
    },

    #' @description Transform a point or several points by the reference scaling.
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
      private[[".center"]] -> Q
      private[[".direction"]] -> w
      private[[".scale"]] -> s
      wQ <- -Q
      theta <- -atan2(w[2L], w[1L])
      M <- sweep(M, 2L, wQ, "+")
      costheta <- cos(theta); sintheta <- sin(theta)
      M <-
        cbind(costheta*M[,1L]-sintheta*M[,2L], sintheta*M[,1L]+costheta*M[,2L])
      M <- cbind(s*M[,1L], M[,2L])
      sintheta <- -sintheta
      M <-
        cbind(costheta*M[,1L]-sintheta*M[,2L], sintheta*M[,1L]+costheta*M[,2L])
      out <- sweep(M, 2L, wQ)
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description Augmented matrix of the scaling.
    #' @return A 3x3 matrix.
    #' @examples S <- Scaling$new(c(1,1), c(2,3), 2)
    #' P <- c(1,5)
    #' S$transform(P)
    #' S$getMatrix() %*% c(P,1)
    getMatrix = function(){
      private[[".center"]] -> Q
      private[[".direction"]] -> w
      private[[".scale"]] -> s
      w1 <- w[1L]; w2 <- w[2L]
      # M1 <- cbind(rbind(c(w1,w2),c(-w2,w1),c(0,0)), c(Q,1))
      # M2 <- cbind(rbind(s*c(w1,w2),c(-w2,w1),c(0,0)), c(Q,1))
      M1 <- cbind(rbind(c(w1,w2),c(-w2,w1),Q), c(0,0,1))
      M2 <- cbind(rbind(s*c(w1,w2),c(-w2,w1),Q), c(0,0,1))
      M <- solve(M1) %*% M2 # top-left corner always symmetric ?
      M[,3L] <- M[3L,]
      M[3L,] <- c(0,0,1)
      M
    },

    #' @description Convert the reference scaling to an \code{Affine} object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    },

    #' @description Scale a circle. The result is an ellipse.
    #' @param circ a \code{Circle} object
    #' @return An \code{Ellipse} object.
    scaleCircle = function(circ){
      private[[".direction"]] -> w
      C <- circ$center; R <- circ$radius
      O <- self$transform(C)
      lw <- sqrt(c(crossprod(w)))
      A1 <- self$transform(C + R*w/lw)
      wt <- c(-w[2L], w[1L])
      A2 <- self$transform(C + R*wt/lw)
      if(private[[".scale"]] >= 1){
        r1 <- sqrt(c(crossprod(A1-O)))
        r2 <- sqrt(c(crossprod(A2-O)))
        alpha <- atan2(A1[2L]-O[2L],A1[1L]-O[1L]) * 180/pi
      }else{
        r1 <- sqrt(c(crossprod(A2-O)))
        r2 <- sqrt(c(crossprod(A1-O)))
        alpha <- atan2(A2[2L]-O[2L],A2[1L]-O[1L]) * 180/pi
      }
      Ellipse$new(O, r1, r2, alpha, degrees = TRUE)
    }
  )
)
