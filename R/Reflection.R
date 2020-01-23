#' @title R6 class representing a reflection
#'
#' @description A reflection is given by a line.
#'
#' @export
#' @importFrom R6 R6Class
Reflection <- R6Class(

  "Reflection",

  private = list(
    .line = NULL
  ),

  active = list(
    #' @field line get or set the line of the reflection
    line = function(value) {
      if (missing(value)) {
        private[[".line"]]
      } else {
        stopifnot(
          is(value, "Line")
        )
        private[[".line"]] <- Line$new(value$A, value$B, TRUE, TRUE)
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Reflection} object.
    #' @param line a \code{Line} object
    #' @return A new \code{Reflection} object.
    #' @examples l <- Line$new(c(1,1), c(1.5,1.5), FALSE, TRUE)
    #' Reflection$new(l)
    initialize = function(line) {
      stopifnot(
        is(line, "Line")
      )
      private[[".line"]] <- Line$new(line$A, line$B, TRUE, TRUE)
    },

    #' @description Show instance of a reflection object.
    #' @param ... ignored
    print = function(...) {
      line <- private[[".line"]]
      cat("Reflection with respect to the line passing through A and B.\n")
      cat("       A: ", toString(line$A), "\n", sep = "")
      cat("       B: ", toString(line$B), "\n", sep = "")
    },

    #' @description Reflect a point.
    #' @param M a point, \code{Inf} allowed
    reflect = function(M) {
      if(isTRUE(all.equal(M,Inf))) return(Inf)
      line <- private[[".line"]]
      if(line$includes(M)) return(M)
      perp <- line$perpendicular(M, FALSE, FALSE)
      M + 2 * (perp$A - perp$B)
    },

    #' @description Reflect a circle.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object.
    reflectCircle = function(circ) {
      Circle$new(self$reflect(circ$center), circ$radius)
    },

    #' @description Reflect a line.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    reflectLine = function(line) {
      Line$new(self$reflect(line$A), self$reflect(line$B),
               line$extendA, line$extendB)
    },

    #' @description Augmented matrix of the reflection.
    #' @return A 3x3 matrix.
    #' @examples R <- Reflection$new(Line$new(c(2,2), c(4,5)))
    #' P <- c(1,5)
    #' R$reflect(P)
    #' R$getMatrix() %*% c(P,1)
    getMatrix = function(){
      private[[".line"]] -> line
      Q <- line$A; w <- line$B - line$A
      wt <- c(-w[2L], w[1L])
      M1 <- cbind(rbind(w,wt,Q),c(0,0,1))
      M2 <- cbind(rbind(w,-wt,Q),c(0,0,1))
      M <- solve(M1) %*% M2
      M[,3L] <- M[3L,]; M[3L,] <- c(0,0,1)
      M
    },

    #' @description Convert the reference reflection to an \code{Affine} object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    }

  )
)
