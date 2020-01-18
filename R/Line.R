#' @title R6 class representing a line
#'
#' @description A line is given by two distinct points,
#' named \code{A} and \code{B}, and two logical values \code{extendA}
#' and \code{extendB}, indicating whether the line must be extended
#' beyond \code{A} and \code{B} respectively.
#'
#' @export
#' @importFrom R6 R6Class
Line <- R6Class(

  "Line",

  private = list(
    .A = c(NA_real_, NA_real_),
    .B = c(NA_real_, NA_real_),
    .extendA = NA,
    .extendB = NA
  ),

  active = list(
    #' @field A get or set the point A
    A = function(value) {
      if (missing(value)) {
        private[[".A"]]
      } else {
        A <- as.vector(value)
        stopifnot(
          is.numeric(A),
          length(A) == 2L,
          !any(is.na(A))
        )
        private[[".A"]] <- A
      }
    },

    #' @field B get or set the point B
    B = function(value) {
      if (missing(value)) {
        private[[".B"]]
      } else {
        B <- as.vector(value)
        stopifnot(
          is.numeric(B),
          length(B) == 2L,
          !any(is.na(B))
        )
        private[[".B"]] <- B
      }
    },

    #' @field extendA get or set \code{extendA}
    extendA = function(value){
      if (missing(value)) {
        private[[".extendA"]]
      } else {
        extendA <- as.vector(value)
        stopifnot(
          is.logical(extendA),
          length(extendA) == 1L,
          !is.na(extendA)
        )
        private[[".extendA"]] <- extendA
      }
    },

    #' @field extendB get or set \code{extendB}
    extendB = function(value){
      if (missing(value)) {
        private[[".extendB"]]
      } else {
        extendB <- as.vector(value)
        stopifnot(
          is.logical(extendB),
          length(extendB) == 1L,
          !is.na(extendB)
        )
        private[[".extendB"]] <- extendB
      }
    }

  ),

  public = list(
    #' @description Create a new \code{Line} object.
    #' @param A,B points
    #' @param extendA,extendB logical values
    #' @return A new \code{Line} object.
    #' @examples l <- Line$new(c(1,1), c(1.5,1.5), FALSE, TRUE)
    #' l
    #' l$A
    #' l$A <- c(0,0)
    #' l
    initialize = function(A, B, extendA, extendB) {
      A <- as.vector(A); B <- as.vector(B)
      stopifnot(
        is.numeric(A),
        length(A) == 2L,
        !any(is.na(A))
      )
      stopifnot(
        is.numeric(B),
        length(B) == 2L,
        !any(is.na(B))
      )
      stopifnot(any(A != B))
      extendA <- as.vector(extendA); extendB <- as.vector(extendB)
      stopifnot(
        is.logical(extendA),
        length(extendA) == 1L,
        !is.na(extendA)
      )
      stopifnot(
        is.logical(extendB),
        length(extendB) == 1L,
        !is.na(extendB)
      )
      private[[".A"]] <- A
      private[[".B"]] <- B
      private[[".extendA"]] <- extendA
      private[[".extendB"]] <- extendB
    },

    #' @description Show instance of a line object
    #' @param ... ignored
    #' @examples Line$new(c(0,0), c(1,0), FALSE, TRUE)
    print = function(...) {
      cat("    Line:\n")
      cat("       A: ", toString(private[[".A"]]), "\n", sep = "")
      cat("       B: ", toString(private[[".B"]]), "\n", sep = "")
      cat(" extendA: ", toString(private[[".extendA"]]), "\n", sep = "")
      cat(" extendB: ", toString(private[[".extendB"]]), "\n", sep = "")
    }
  )
)
