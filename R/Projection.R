#' @title R6 class representing a projection
#'
#' @description A projection on a line \code{D} parallel to another line
#' \code{Delta} is given by the line of projection (\code{D})
#' and the directrix line (\code{Delta}).
#'
#' @export
#' @importFrom R6 R6Class
Projection <- R6Class(

  "Projection",

  private = list(
    .D = NULL,
    .Delta = NULL
  ),

  active = list(
    #' @field D get or set the projection line
    D = function(value) {
      if (missing(value)) {
        private[[".D"]]
      } else {
        stopifnot(
          is(value, "Line")
        )
        private[[".D"]] <- Line$new(value$A, value$B, TRUE, TRUE)
      }
    },

    #' @field Delta get or set the directrix line
    Delta = function(value) {
      if (missing(value)) {
        private[[".Delta"]]
      } else {
        stopifnot(
          is(value, "Line")
        )
        private[[".Delta"]] <- Line$new(value$A, value$B, TRUE, TRUE)
      }
    }

  ),

  public = list(
    #' @description Create a new \code{Projection} object.
    #' @param D,Delta two \code{Line} objects; they must meet (not parallel)
    #' @return A new \code{Projection} object.
    #' @examples D <- Line$new(c(1,1), c(5,5))
    #' Delta <- Line$new(c(0,0), c(3,4))
    #' Projection$new(D, Delta)
    initialize = function(D, Delta) {
      stopifnot(
        is(D, "Line"),
        is(Delta, "Line"),
        !D$isParallel(Delta)
      )
      private[[".D"]] <- Line$new(D$A, D$B, TRUE, TRUE)
      private[[".Delta"]] <- Line$new(Delta$A, Delta$B, TRUE, TRUE)
    },

    #' @description Show instance of a reflection object.
    #' @param ... ignored
    print = function(...) {
      D <- private[[".D"]]
      cat("Projection onto the line D passing through A and B parallel to ",
          "the line Delta passing through P and Q.\n", sep = "")
      cat("       A: ", toString(D$A), "\n", sep = "")
      cat("       B: ", toString(D$B), "\n", sep = "")
      cat("       P: ", toString(Delta$A), "\n", sep = "")
      cat("       Q: ", toString(Delta$B), "\n", sep = "")
    },

    #' @description Project a point.
    #' @param M a point
    project = function(M) {
      D <- private[[".D"]]
      if(D$includes(M)) return(M)
      Delta <- private[[".Delta"]]
      u <- Delta$B - Delta$A
      do <- D$directionAndOffset()
      ab <- c(cos(do$direction), sin(do$direction))
      k <- - (.dot(ab,M) - do$offset) / .dot(ab,u)
      k*u + M
    },

    #' @description An alias of \code{project}.
    #' @param M a point
    transform = function(M){
      self$project(M)
    },


    #' @description Augmented matrix of the projection.
    #' @return A 3x3 matrix.
    #' @examples P <- Projection$new(Line$new(c(2,2), c(4,5)), Line$new(c(0,0), c(1,1)))
    #' M <- c(1,5)
    #' P$project(M)
    #' P$getMatrix() %*% c(M,1)
    getMatrix = function(){
      b <- self$project(c(0,0))
      col1 <- self$project(c(1,0)) - b
      col2 <- self$project(c(0,1)) - b
      cbind(rbind(cbind(col1,col2),0), c(b,1))
    },

    #' @description Convert the reference projection to an \code{Affine} object.
    asAffine = function(){
      M <- self$getMatrix()
      Affine$new(M[-3L,-3L], M[-3L,3L])
    }

  )
)
