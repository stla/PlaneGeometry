#' @title R6 class representing a shear transformation
#'
#' @description A shear is given by a vertex, two perpendicular vectors,
#' and an angle.
#'
#' @examples P <- c(0,0); w <- c(1,0); ratio <- 1; angle <- 45
#' shear <- Shear$new(P, w, ratio, angle)
#' wt <- ratio * c(-w[2], w[1])
#' Q <- P + w; R <- Q + wt; S <- P + wt
#' A <- shear$transform(P)
#' B <- shear$transform(Q)
#' C <- shear$transform(R)
#' D <- shear$transform(S)
#' plot(0, 0, type = "n", asp = 1, xlim = c(0,1), ylim = c(0,2))
#' lines(rbind(P,Q,R,S,P), lwd = 2) # unit square
#' lines(rbind(A,B,C,D,A), lwd = 2, col = "blue") # image by shear
#'
#' @references R. Goldman,
#' \emph{An Integrated Introduction to Computer Graphics and Geometric Modeling}.
#' CRC Press, 2009.
#'
#' @export
#' @importFrom R6 R6Class
Shear <- R6Class(

  "Shear",

  private = list(
    .vertex = c(NA_real_, NA_real_),
    .vector = c(NA_real_, NA_real_),
    .ratio = NA_real_,
    .angle = NA_real_,
    .degrees = NA
  ),

  active = list(
    #' @field vertex get or set the vertex
    vertex = function(value) {
      if (missing(value)) {
        private[[".vertex"]]
      } else {
        vertex <- as.vector(value)
        stopifnot(
          is.numeric(vertex),
          length(vertex) == 2L,
          !any(is.na(vertex)),
          all(is.finite(vertex))
        )
        private[[".vertex"]] <- vertex
      }
    },

    #' @field vector get or set the first vector
    vector = function(value) {
      if (missing(value)) {
        private[[".vector"]]
      } else {
        vector <- as.vector(value)
        stopifnot(
          is.numeric(vector),
          length(vector) == 2L,
          !any(is.na(vector)),
          all(is.finite(vector)),
          any(vector != 0)
        )
        private[[".vector"]] <- vector
      }
    },

    #' @field ratio get or set the ratio between the length of \code{vector}
    #' and the length of the second vector, perpendicular to the first one
    ratio = function(value) {
      if (missing(value)) {
        private[[".ratio"]]
      } else {
        ratio <- as.vector(value)
        stopifnot(
          is.numeric(ratio),
          length(ratio) == 1L,
          !is.na(ratio),
          is.finite(ratio),
          ratio > 0
        )
        private[[".ratio"]] <- ratio
      }
    },

    #' @field angle get or set the angle
    angle = function(value) {
      if (missing(value)) {
        private[[".angle"]]
      } else {
        angle <- as.vector(value)
        stopifnot(
          is.numeric(angle),
          length(angle) == 1L,
          !is.na(angle),
          is.finite(angle)
        )
        if(private[[".degrees"]] && (angle >= 90 || angle <= -90)){
          stop("The angle must be strictly between -90 degrees and 90 degrees.")
        }
        if(!private[[".degrees"]] && (angle >= pi/2 || angle <= -pi/2)){
          stop("The angle must be strictly between -pi/2 radians and pi/2 radians.")
        }
        private[[".angle"]] <- angle
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
        angle <- private[[".angle"]]
        if(degrees && (angle >= 90 || angle <= -90)){
          stop("The angle must be strictly between -90 degrees and 90 degrees.")
        }
        if(!degrees && (angle >= pi/2 || angle <= -pi/2)){
          stop("The angle must be strictly between -pi/2 radians and pi/2 radians.")
        }
        private[[".degrees"]] <- degrees
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Shear} object.
    #' @param vertex a point
    #' @param vector a vector
    #' @param ratio a positive number, the ratio between the length of \code{vector}
    #' and the length of the second vector, perpendicular to the first one
    #' @param angle an angle strictly between -90 degrees and 90 degrees
    #' @param degrees logical, whether \code{angle} is given in degrees
    #' @return A new \code{Shear} object.
    #' @examples Shear$new(c(1,1), c(1,3), 0.5, 30)
    initialize = function(vertex, vector, ratio, angle, degrees = TRUE) {
      vertex <- as.vector(vertex)
      stopifnot(
        is.numeric(vertex),
        length(vertex) == 2L,
        !any(is.na(vertex)),
        all(is.finite(vertex))
      )
      vector <- as.vector(vector)
      stopifnot(
        is.numeric(vector),
        length(vector) == 2L,
        !any(is.na(vector)),
        all(is.finite(vector)),
        any(vector != 0)
      )
      ratio <- as.vector(ratio)
      stopifnot(
        is.numeric(ratio),
        length(ratio) == 1L,
        !is.na(ratio),
        is.finite(ratio),
        ratio > 0
      )
      angle <- as.vector(angle)
      stopifnot(
        is.numeric(angle),
        length(angle) == 1L,
        !is.na(angle),
        is.finite(angle)
      )
      if(degrees && (angle >= 90 || angle <= -90)){
        stop("The angle must be strictly between -90 degrees and 90 degrees.")
      }
      if(!degrees && (angle >= pi/2 || angle <= -pi/2)){
        stop("The angle must be strictly between -pi/2 radians and pi/2 radians.")
      }
      degrees <- as.vector(degrees)
      stopifnot(
        is.logical(degrees),
        length(degrees) == 1L,
        !is.na(degrees)
      )
      private[[".vertex"]] <- vertex
      private[[".vector"]] <- vector
      private[[".ratio"]] <- ratio
      private[[".angle"]] <- angle
      private[[".degrees"]] <- degrees
    },

    #' @description Show instance of a \code{Shear} object.
    #' @param ... ignored
    print = function(...) {
      private[[".vertex"]] -> vertex
      private[[".vector"]] -> vector
      private[[".ratio"]] -> ratio
      private[[".angle"]] -> angle
      cat("Shear:\n")
      cat("        vertex: ", toString(vertex), "\n", sep = "")
      cat("  first vector: ", toString(vector), "\n", sep = "")
      cat(" second vector: ", toString(ratio*c(-vector[2L],vector[1L])), "\n", sep = "")
      cat("         angle: ",
          sprintf("%s %s", alpha,
                  ifelse(private[[".degrees"]],
                         ifelse(angle %in% c(0,1), "degree", "degrees"),
                         ifelse(angle %in% c(0,1), "radian", "radians"))
          ), "\n", sep = "")
    },

    #' @description Transform a point by the reference shear.
    #' @param M a point
    transform = function(M) {
      M <- as.vector(M)
      stopifnot(
        is.numeric(M),
        length(M) == 2L,
        !any(is.na(M)),
        all(is.finite(M))
      )
      Mat <- self$getMatrix()
      as.vector(Mat %*% c(M,1))[1L:2L]
    },

    #' @description Augmented matrix of the shear.
    #' @return A 3x3 matrix.
    #' @examples S <- Shear$new(c(1,1), c(1,3), 0.5, 30)
    #' S$getMatrix()
    getMatrix = function(){
      private[[".vertex"]] -> Q
      private[[".vector"]] -> w
      private[[".ratio"]] -> ratio
      private[[".angle"]] -> angle
      if(private[[".degrees"]]) angle <- angle * pi/180
      wt <- ratio*c(-w[2L],w[1L])
      M1 <- cbind(rbind(w,wt,Q), c(0,0,1))
      M2 <- cbind(rbind(w, tan(angle)*w+wt,Q), c(0,0,1))
      M <- solve(M1) %*% M2
      M[,3L] <- M[3L,]; M[3L,] <- c(0,0,1)
      M
    }
  )
)
