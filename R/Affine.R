#' @title R6 class representing an affine transformation.
#'
#' @description An affine transformation is given by an invertible 2x2 matrix
#' (a linear transformation) and a vector (the "intercept").
#'
#' @export
#' @importFrom R6 R6Class
Affine <- R6Class(

  "Affine",

  private = list(
    .A = matrix(NA_real_, 2L, 2L),
    .b = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field A get or set the matrix \code{A}
    A = function(value) {
      if (missing(value)) {
        private[[".A"]]
      } else {
        A <- value
        stopifnot(
          is.numeric(A),
          is.matrix(A),
          nrow(A) == 2L,
          ncol(A) == 2L,
          !any(is.na(A)),
          all(is.finite(A)),
          det(A) != 0
        )
        private[[".A"]] <- A
      }
    },

    #' @field b get or set the vector \code{b}
    b = function(value) {
      if (missing(value)) {
        private[[".b"]]
      } else {
        b <- as.vector(value)
        stopifnot(
          is.numeric(b),
          length(b) == 2L,
          !any(is.na(b)),
          all(is.finite(b))
        )
        private[[".b"]] <- b
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Affine} object.
    #' @param A the 2x2 invertible matrix of the affine transformation
    #' @param b the shift vector of the affine transformation
    #' @return A new \code{Affine} object.
    initialize = function(A, b) {
      stopifnot(
        is.numeric(A),
        is.matrix(A),
        nrow(A) == 2L,
        ncol(A) == 2L,
        !any(is.na(A)),
        all(is.finite(A)),
        det(A) != 0
      )
      b <- as.vector(b)
      stopifnot(
        is.numeric(b),
        length(b) == 2L,
        !any(is.na(b)),
        all(is.finite(b))
      )
      private[[".A"]] <- A
      private[[".b"]] <- b
    },

    #' @description Show instance of an \code{Affine} object.
    #' @param ... ignored
    #' @examples Affine$new(rbind(c(3.5,2),c(0,4)), c(-1, 1.25))
    print = function(...) {
      captA <- capture.output(private[[".A"]])[-1L]
      captA[1L] <- substring(captA[1L], 6L)
      captA[2L] <- substring(captA[2L], 6L)
      captB <- capture.output(cbind(private[[".b"]]))[-1L]
      captB[1L] <- substring(captB[1L], 6L)
      captB[2L] <- substring(captB[2L], 6L)
      cat("Affine transformation Ax+b\n")
      cat(" A: / ", captA[1L], " \\\n    \\ ", captA[2L], " /\n", sep = "")
      cat(" b: / ", captB[1L], " \\\n    \\ ", captB[2L], " /\n", sep = "")
    },

    #' @description The 3x3 matrix representing the affine transformation.
    get3x3matrix = function(){
      rbind(cbind(private[[".A"]],private[[".b"]]), c(0,0,1))
    },

    #' @description The inverse affine transformation.
    inverse = function(){
      M <- solve(self$get3x3matrix())
      Affine$new(M[-3L,-3L], M[-3L,3L])
    },

    #' @description Compose the reference affine transformation with another
    #' affine transformation.
    #' @param transfo an \code{Affine} object
    #' @param left logical, whether to compose at left or at right (i.e.
    #' returns \code{f1 o f0} or \code{f0 o f1})
    #' @return An \code{Affine} object.
    compose = function(transfo, left = TRUE){
      M0 <- self$get3x3matrix()
      M1 <- transfo$get3x3matrix()
      M <- if(left) M0 %*% M1 else M1 %*% M0
      Affine$new(M[-3L,-3L], M[-3L,3L])
    },

    #' @description Transform a point by the reference affine transformation.
    #' @param M a point
    transform = function(M){
      M <- as.vector(M)
      stopifnot(
        is.numeric(M),
        length(M) == 2L,
        !any(is.na(M)),
        all(is.finite(M))
      )
      c(private[[".A"]] %*% M) + private[[".b"]]
    }
  )
)


#' Affine transformation mapping three given points to three given points
#' @description Return the affine transformation which sends
#' \code{P1} to \code{Q1}, \code{P2} to \code{Q2} and \code{P3} to \code{Q3}.
#'
#' @param P1,P2,P3 three non-collinear points
#' @param Q1,Q2,Q3 three non-collinear points
#'
#' @return An \code{Affine} object.
#' @export
AffineMappingThreePoints <- function(P1, P2, P3, Q1, Q2, Q3){
  if(.collinear(P1,P2,P3)) stop("P1, P2 and P3 are collinear.")
  if(.collinear(Q1,Q2,Q3)) stop("Q1, Q2 and Q3 are collinear.")
  f1 <- Affine$new(cbind(P2-P1, P3-P1), P1)
  f2 <- Affine$new(cbind(Q2-Q1, Q3-Q1), Q1)
  f1$inverse()$compose(f2)
}
