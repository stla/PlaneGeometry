#' @title R6 class representing an inversion
#'
#' @description An inversion is given by a pole (a point) and a power (a number,
#' possibly negative, but not zero).
#'
#' @seealso \code{\link{inversionMappingCircle2Circle}},
#' \code{\link{inversionFixingTwoCircles}},
#' \code{\link{inversionFixingThreeCircles}} to create some inversions.
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
          !is.na(power),
          power != 0
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
        !is.na(power),
        power != 0
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
    #' @return A point, the image of \code{M}.
    invert = function(M) {
      pole <- private[[".pole"]]; k <- private[[".power"]]
      pole_M <- M - pole
      pole + k/c(crossprod(pole_M)) * pole_M
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


#' Inversion mapping a circle to a circle
#' @description Return the inversion which maps a given circle to another given
#' circle.
#'
#' @param circ1,circ2 \code{Circle} objects
#' @param positive logical, whether the sign of the desired inversion power
#' must be positive or negative
#'
#' @return An \code{Inversion} object, which maps \code{circ1} to \code{circ2}.
#' @export
inversionMappingCircle2Circle <- function(circ1, circ2, positive = TRUE){
  warning("This function does not work yet.")
  c1 <- circ1$center; r1 <- circ1$radius
  c2 <- circ2$center; r2 <- circ2$radius
  a <- r1/r2
  if(positive){
    O <- c1 + a/abs(1 - a) * (c2 - c1) # issue if a = 1 !
    Inversion$new(O, a * abs(c(crossprod(O - c2)) - r2*r2))
  }else{
    O <- c1 + a/(1 + a) * (c2 - c1)
    Inversion$new(O, -a * abs(c(crossprod(O - c2)) - r2*r2))
  }
}

#' Inversion fixing two circles
#' @description Return the inversion which lets invariant two given circles.
#'
#' @param circ1,circ2 \code{Circle} objects
#'
#' @return An \code{Inversion} object, which maps \code{circ1} to \code{circ1}
#' and \code{circ2} to \code{circ2}.
#' @export
inversionFixingTwoCircles <- function(circ1, circ2){
  O <- circ1$radicalCenter(circ2)
  Inversion$new(O, circ1$power(O));
}

#' Inversion fixing three circles
#' @description Return the inversion which lets invariant three given circles.
#'
#' @param circ1,circ2,circ3 \code{Circle} objects
#'
#' @return An \code{Inversion} object, which lets each of \code{circ1},
#' \code{circ2} and \code{circ3} invariant.
#' @export
inversionFixingThreeCircles <- function(circ1, circ2, circ3){
  Rc <- radicalCenter(circ1, circ2, circ3)
  Inversion$new(Rc, circ1$power(Rc))
}
