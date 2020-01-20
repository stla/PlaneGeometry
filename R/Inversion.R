#' @title R6 class representing an inversion
#'
#' @description An inversion is given by a pole (a point) and a power (a number,
#' possibly negative).
#'
#' @export
#' @importFrom R6 R6Class
Inversion <- R6Class(

  "Inversion",

  private = list(
    .pole = c(NA_real_, NA_real_),
    .power = NA_real_
  ),

  active = list(
    #' @field pole get or set the pole
    pole = function(value) {
      if (missing(value)) {
        private[[".pole"]]
      } else {
        pole <- as.vector(value)
        stopifnot(
          is.numeric(pole),
          length(pole) == 2L,
          !any(is.na(pole))
        )
        private[[".pole"]] <- pole
      }
    },

    #' @field power get or set the power
    power = function(value) {
      if (missing(value)) {
        private[[".power"]]
      } else {
        power <- as.vector(value)
        stopifnot(
          is.numeric(power),
          length(power) == 1L,
          !is.na(power)
        )
        private[[".power"]] <- power
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Inversion} object.
    #' @param pole the pole
    #' @param power the power
    #' @return A new \code{Inversion} object.
    initialize = function(pole, power) {
      pole <- as.vector(pole)
      stopifnot(
        is.numeric(pole),
        length(pole) == 2L,
        !any(is.na(pole))
      )
      power <- as.vector(power)
      stopifnot(
        is.numeric(power),
        length(power) == 1L,
        !is.na(power)
      )
      private[[".pole"]] <- pole
      private[[".power"]] <- power
    },

    #' @description Show instance of an inversion object.
    #' @param ... ignored
    #' @examples Inversion$new(c(0,0), 2)
    print = function(...) {
      cat("Inversion:\n")
      cat("      pole: ", toString(private[[".pole"]]), "\n", sep = "")
      cat("     power: ", toString(private[[".power"]]), "\n", sep = "")
    },

    #' @description Inversion of a point.
    #' @param M a point
    #' @return A point.
    invert = function(M) {
      pole <- private[[".pole"]]; k <- private[[".power"]]
      A_M <- M - A
      pole + k/c(crossprod(A_M)) * A_M
    },

    #' @description Inversion of a circle.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invertCircle = function(circ){
      c0 <- private[[".pole"]]; k <- private[[".power"]]
      c1 <- circ$center
      r1 <- circ$radius
      D1 <- (c1[1L] - c0[1L])^2 + (c1[2L] - c0[2L])^2 - r1*r1
      if(D1 != 0){
        s <- k / D1
        Circle$new(c0 + s*(c1-c0), abs(s)*r1);
      }else{
        Ot <- c0 - c1
        R180 <- -Ot + c1
        R90 <- c(-Ot[2L], Ot[1L]) + c1
        Line$new(self$invert(R180), sel$invert(R90), TRUE, TRUE)
      }
    },

    #' @description Inversion of a line.
    #' @param line a \code{Line} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invertLine = function(line){
      A <- line$A: B <- line$B
      if(.collinear(A, B, pole)){
        line
      }else{
        Ap <- self$invert(A); Bp <- self$invert(B)
        Triangle$new(private[[".pole"]], Ap, Bp)$circumcircle()
      }
    }
  )
)
