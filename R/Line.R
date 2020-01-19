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
      extendA <- private[[".extendA"]]; extendB <- private[[".extendB"]]
      cat("Line:\n")
      cat("       A: ", toString(private[[".A"]]), "\n", sep = "")
      cat("       B: ", toString(private[[".B"]]), "\n", sep = "")
      cat(" extendA: ", toString(extendA), "\n", sep = "")
      cat(" extendB: ", toString(extendB), "\n", sep = "")
      if(extendA && extendB){
        cat("Infinite line passing through A and B.\n")
      }else if(extendA){
        cat("Half-line with origin B and passing through A.\n")
      }else if(extendB){
        cat("Half-line with origin A and passing through B.\n")
      }else{
        cat("Segment joining A and B.\n")
      }
    },

    #' @description Direction (angle between 0 and 2pi)
    #' and offset (positive number) of the line.
    #' @details The equation of the line is
    #' \ifelse{html}{\out{cos(&theta;)x+sin(&theta;)y=d}}{\eqn{\cos(\theta)x+\sin(\theta)y=d}{cos(theta)x+sin(theta)y=d}}
    #' where \ifelse{html}{\out{&theta;}}{\eqn{\theta}{theta}} is the direction
    #' and \ifelse{html}{\out{d}}{\eqn{\d}{d}} is the offset.
    directionAndOffset = function(){
      A <- private[[".A"]]; B <- private[[".B"]]
      if(A[1L] == B[1L]){
        if(A[1L] > 0){
          list(direction = 0, offset = A[1L])
        }else{
          list(direction = pi, offset = -A[1L])
        }
      }else{
        x <- B[1L] - A[1L]
        y <- B[2L] - A[2L]
        # sgn <- sign(x)*sign(y)
        # intercept <-
        #   retistruct::line.line.intersection(A, B, c(0,0), c(0,1))[2L]
        theta <- -atan2(x, y) # if(y >= 0) atan2(y, x) else atan(y, x)
        offset <- A[1]*cos(theta)+A[2]*sin(theta)
        if(offset < 0){
          theta <- theta + pi
          offset <- -offset
        }
        # theta <- if(sgn >= 0){
        #   if(x < 0){
        #     atan2(y, x)
        #   }else{
        #     atan2(y, -x)
        #   }
        # }else{
        #   if(x < 0){
        #     atan2(y, -x)
        #   }else{
        #     atan2(y, x)
        #   }
        # }
        # if(intercept > 0){
        #   theta <- theta + pi
        # }else{
        #   theta <- theta + 2*pi
        # }
        list(direction = theta %% (2*pi), offset = offset)
      }
    },

    #' @description Perpendicular line passing through a given point.
    #' @param M the point through which the perpendicular passes.
    #' @param extendH logical, whether to extend the perpendicular line
    #' beyond the meeting point
    #' @param extendM logical, whether to extend the perpendicular line
    #' beyond the point \code{M}
    #' @return A \code{Line} object; its two points are the
    #' meeting point and the point \code{M}.
    perpendicular = function(M, extendH = FALSE, extendM = TRUE) {
      A <- private[[".A"]]; B <- private[[".B"]]
      A_B <- B - A
      v <- c(-A_B[2L], A_B[1L])
      H <- retistruct::line.line.intersection(A, B, M, M+v)
      Line$new(H, M, extendH, extendM)
    }
# projection ?
  )
)
