#' @title R6 class representing an inversion
#'
#' @description An inversion is given by a pole (a point) and a power (a number,
#' possibly negative, but not zero).
#'
#' @seealso \code{\link{inversionSwappingTwoCircles}},
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
          !any(is.na(pole)),
          all(is.finite(pole))
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
          is.finite(power),
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
        !any(is.na(pole)),
        all(is.finite(pole))
      )
      power <- as.vector(power)
      stopifnot(
        is.numeric(power),
        length(power) == 1L,
        !is.na(power),
        is.finite(power),
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
    #' @param M a point or \code{Inf}
    #' @return A point or \code{Inf}, the image of \code{M}.
    invert = function(M) {
      pole <- private[[".pole"]]
      if(isTRUE(all.equal(Inf, M, check.attributes = FALSE))) return(pole)
      M <- as.vector(M)
      stopifnot(
        is.numeric(M),
        length(M) == 2L,
        !any(is.na(M)),
        all(is.finite(M))
      )
      if(isTRUE(all.equal(pole, M, check.attributes = FALSE))) return(Inf)
      k <- private[[".power"]]
      pole_M <- M - pole
      pole + k/c(crossprod(pole_M)) * pole_M
    },

    #' @description An alias of \code{invert}.
    #' @param M a point or \code{Inf}
    #' @return A point or \code{Inf}, the image of \code{M}.
    transform = function(M){
      self$invert(M)
    },

    #' @description Inversion of a circle.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object or a \code{Line} object.
    #' @examples # A Pappus chain
    #' # https://www.cut-the-knot.org/Curriculum/Geometry/InversionInArbelos.shtml
    #' opar <- par(mar = c(0,0,0,0))
    #' plot(0, 0, type = "n", asp = 1, xlim = c(0,6), ylim = c(-4,4),
    #'      xlab = NA, ylab = NA, axes = FALSE)
    #' A <- c(0,0); B <- c(6,0)
    #' ABsqr <- c(crossprod(A-B))
    #' iota <- Inversion$new(A, ABsqr)
    #' C <- iota$invert(c(8,0))
    #' Sigma1 <- Circle$new((A+B)/2, sqrt(ABsqr)/2)
    #' Sigma2 <- Circle$new((A+C)/2, sqrt(c(crossprod(A-C)))/2)
    #' draw(Sigma1); draw(Sigma2)
    #' circ0 <- Circle$new(c(7,0), 1)
    #' iotacirc0 <- iota$invertCircle(circ0)
    #' draw(iotacirc0)
    #' for(i in 1:6){
    #'   circ <- circ0$translate(c(0,2*i))
    #'   iotacirc <- iota$invertCircle(circ)
    #'   draw(iotacirc)
    #'   circ <- circ0$translate(c(0,-2*i))
    #'   iotacirc <- iota$invertCircle(circ)
    #'   draw(iotacirc)
    #' }
    #' par(opar)
    invertCircle = function(circ){
      stopifnot(is(circ, "Circle"))
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
        Line$new(self$invert(R180), self$invert(R90), TRUE, TRUE)
      }
    },

    #' @description An alias of \code{invertCircle}.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object or a \code{Line} object.
    transformCircle = function(circ){
      self$invertCircle(circ)
    },

    #' @description Inversion of a line.
    #' @param line a \code{Line} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invertLine = function(line){
      stopifnot(is(line, "Line"))
      A <- line$A: B <- line$B
      if(.collinear(A, B, private[[".pole"]])){
        line
      }else{
        Ap <- self$invert(A); Bp <- self$invert(B)
        Triangle$new(private[[".pole"]], Ap, Bp)$circumcircle()
      }
    },

    #' @description An alias of \code{invertLine}.
    #' @param line a \code{Line} object
    #' @return A \code{Circle} object or a \code{Line} object.
    transformLine = function(line){
      self$invertLine(line)
    },

    #' @description Compose the reference inversion with another inversion.
    #' The result is a Möbius transformation.
    #' @param iota1 an \code{Inversion} object
    #' @param left logical, whether to compose at left or at right (i.e.
    #' returns \code{iota1 o iota0} or \code{iota0 o iota1})
    #' @return A \code{Mobius} object.
    compose = function(iota1, left = TRUE) {
      if(!left) return(iota1$compose(self))
      Mob0 <- .inversion2conjugateMobius(self)
      Mob1 <- .inversion2conjugateMobius(iota1)
      M0 <- Mob0$getM()
      Mob3 <- Mobius$new(Conj(M0))
      Mob3$compose(Mob1)
    }
  )
)


#' Inversion swapping two circles
#' @description Return the inversion which swaps two given circles.
#'
#' @param circ1,circ2 \code{Circle} objects
#' @param positive logical, whether the sign of the desired inversion power
#' must be positive or negative
#'
#' @return An \code{Inversion} object, which maps \code{circ1} to \code{circ2}
#' and \code{circ2} to \code{circ1}.
#' @export
inversionSwappingTwoCircles <- function(circ1, circ2, positive = TRUE){
  stopifnot(is(circ1, "Circle"), is(circ2, "Circle"))
  c1 <- circ1$center; r1 <- circ1$radius
  c2 <- circ2$center; r2 <- circ2$radius
  ok <- TRUE
  if(positive && r1 == r2){
    warning("`positive = TRUE` not possible; switching to `FALSE`")
    ok <- FALSE
  }
  c1c2 <- .distance(c1, c2)
  inside <- FALSE
  if(max(r1,r2) > c1c2 + min(r1,r2)){
    inside <- TRUE
  }
  if(!positive && !inside){
    circlesIntersect <- c1c2 <= r1+r2
    if(circlesIntersect){
      warning("`positive = FALSE` not possible; switching to `TRUE`")
      positive <- TRUE
    }
  }
  a <- r1/r2
  if(positive && ok){
    #O <- -r2/(r1-r2) * c1 + r1/(r1-r2) * c2
    O <- if(inside){
      c1 + a/(1+a) * (c2-c1)
    }else{
      c1 - a/(1-a) * (c2-c1)
    }
    Inversion$new(O, a * abs(c(crossprod(O - c2)) - r2*r2))
  }else{
    #O <- r2/(r1+r2) * c1 + r1/(r1+r2) * c2
    O <- if(inside){
      c1 - a/(1-a) * (c2-c1)
    }else{
      c1 + a/(1+a) * (c2-c1)
    }
    Inversion$new(O, -a * abs(c(crossprod(O - c2)) - r2*r2))
  }
}

#' Inversion fixing two circles
#' @description Return the inversion which lets invariant two given circles.
#'
#' @param circ1,circ2 \code{Circle} objects
#'
#' @return An \code{Inversion} object, which maps \code{circ1} to \code{circ2}
#' and \code{circ2} to \code{circ2}.
#' @export
inversionFixingTwoCircles <- function(circ1, circ2){
  stopifnot(is(circ1, "Circle"), is(circ2, "Circle"))
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
  stopifnot(is(circ1, "Circle"), is(circ2, "Circle"), is(circ3, "Circle"))
  Rc <- radicalCenter(circ1, circ2, circ3)
  Inversion$new(Rc, circ1$power(Rc))
}

#' Inversion keeping a circle unchanged
#' @description Return an inversion with a given pole which keeps a given
#' circle unchanged.
#'
#' @param pole inversion pole, a point
#' @param circ a \code{Circle} object
#'
#' @return An \code{Inversion} object.
#' @export
#'
#' @examples circ <- Circle$new(c(4,3), 2)
#' iota <- inversionKeepingCircle(c(1,2), circ)
#' iota$transformCircle(circ)
inversionKeepingCircle <- function(pole, circ){
  stopifnot(is(circ, "Circle"))
  Inversion$new(pole, circ$power(pole))
}
