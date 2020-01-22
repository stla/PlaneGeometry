#' @title R6 class representing a Möbius transformation.
#'
#' @description A Möbius transformation is given by a matrix of complex numbers
#' with non-null determinant.
#'
#' @seealso \code{\link{MobiusMappingThreePoints}} to create a Möbius
#' transformation, and also the \code{compose} method of the
#' \code{\link{Inversion}} R6 class.
#'
#' @export
#' @importFrom R6 R6Class
Mobius <- R6Class(

  "Mobius",

  private = list(
    .a = NA_complex_,
    .b = NA_complex_,
    .c = NA_complex_,
    .d = NA_complex_
  ),

  active = list(
    #' @field a get or set \code{a}
    a = function(value) {
      if (missing(value)) {
        private[[".a"]]
      } else {
        a <- as.vector(value)
        stopifnot(
          is.numeric(a) || is.complex(a),
          length(a) == 1L,
          !is.na(a),
          is.finite(a)
        )
        private[[".a"]] <- as.complex(a)
      }
    },

    #' @field b get or set \code{b}
    b = function(value) {
      if (missing(value)) {
        private[[".b"]]
      } else {
        b <- as.vector(value)
        stopifnot(
          is.numeric(b) || is.complex(b),
          length(b) == 1L,
          !is.na(b),
          is.finite(b)
        )
        private[[".b"]] <- as.complex(b)
      }
    },

    #' @field c get or set \code{c}
    c = function(value) {
      if (missing(value)) {
        private[[".c"]]
      } else {
        c <- as.vector(value)
        stopifnot(
          is.numeric(c) || is.complex(c),
          length(c) == 1L,
          !is.na(c),
          is.finite(c)
        )
        private[[".c"]] <- as.complex(c)
      }
    },

    #' @field d get or set \code{d}
    d = function(value) {
      if (missing(value)) {
        private[[".d"]]
      } else {
        d <- as.vector(value)
        stopifnot(
          is.numeric(d) || is.complex(d),
          length(d) == 1L,
          !is.na(d),
          is.finite(d)
        )
        private[[".d"]] <- as.complex(d)
      }
    }

  ),

  public = list(
    #' @description Create a new \code{Mobius} object.
    #' @param M the matrix corresponding to the Möbius transformation
    #' @return A new \code{Mobius} object.
    initialize = function(M) {
      stopifnot(
        is.matrix(M),
        nrow(M) == 2L,
        ncol(M) == 2L,
        is.numeric(M) || is.complex(M),
        !any(is.na(M)),
        all(is.finite(M))
      )
      M[] <- as.complex(M)
      if(M[1L,1L]*M[2L,2L]-M[1L,2L]*M[2L,1L] == 0){
        stop("The determinant of `M` is zero.")
      }
      private[[".a"]] <- M[1L,1L]
      private[[".b"]] <- M[1L,2L]
      private[[".c"]] <- M[2L,1L]
      private[[".d"]] <- M[2L,2L]
    },

    #' @description Show instance of a \code{Mobius} object.
    #' @param ... ignored
    #' @examples Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
    print = function(...) {
      cat("Möbius transformation (az+b)/(cz+d)\n")
      cat(" a: ", toString(private[[".a"]]), "\n", sep = "")
      cat(" b: ", toString(private[[".b"]]), "\n", sep = "")
      cat(" c: ", toString(private[[".c"]]), "\n", sep = "")
      cat(" d: ", toString(private[[".d"]]), "\n", sep = "")
    },

    #' @description Get the matrix corresponding to the Möbius transformation.
    getM = function() {
      M <- matrix(NA_complex_, 2L, 2L)
      private[[".a"]] -> M[1L,1L]
      private[[".b"]] -> M[1L,2L]
      private[[".c"]] -> M[2L,1L]
      private[[".d"]] -> M[2L,2L]
      M
    },

    #' @description Compose the reference Möbius transformation with another
    #' Möbius transformation
    #' @param M1 a \code{Mobius} object
    #' @param left logical, whether to compose at left or at right (i.e.
    #' returns \code{M1 o M0} or \code{M0 o M1})
    #' @return A \code{Mobius} object.
    compose = function(M1, left = TRUE) {
      A <- self$getM(); B <- M1$getM()
      if(left) Mobius$new(B %*% A) else Mobius$new(A %*% B)
    },

    #' @description Inverse the reference Möbius transformation.
    #' @return A \code{Mobius} object.
    inverse = function() {
      M <- matrix(NA_complex_, 2L, 2L)
      private[[".a"]] -> M[2L,2L]
      -private[[".b"]] -> M[1L,2L]
      -private[[".c"]] -> M[2L,1L]
      private[[".d"]] -> M[1L,1L]
      Mobius$new(M)
    },

    #' @description Transformation of a point by the reference Möbius transformation.
    #' @param M a point or \code{Inf}
    #' @return A point or \code{Inf}, the image of \code{M}.
    #' @examples Mob <- Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
    #' Mob$transform(c(1,1))
    #' Mob$transform(Inf)
    transform = function(M) {
      M <- as.vector(M)
      if(!(isinf <- isTRUE(all.equal(M, Inf)))){
        stopifnot(
          is.numeric(M),
          length(M) == 2L,
          !any(is.na(M)),
          all(is.finite(M))
        )
      }
      private[[".a"]] -> a
      private[[".b"]] -> b
      private[[".c"]] -> c
      private[[".d"]] -> d
      if(isinf){
        if(c == 0) Inf else .fromCplx(a/c)
      }else{
        z <- .toCplx(M)
        if(c != 0 && z == -d/c){
          Inf
        }else{
          .fromCplx((a*z+b)/(c*z+d))
        }
      }
    },

    #' @description Transformation of a circle by the reference Möbius transformation.
    #' @param circ a \code{Circle} object
    #' @return A \code{Circle} object or a \code{Line} object.
    transformCircle = function(circ) {
      private[[".c"]] -> c
      private[[".d"]] -> d
      R <- circ$radius
      z0 <- .toCplx(circ$center)
      x1 <- .Mod2(d+c*z0)
      x2 <- R*R*.Mod2(c)
      if(x1 != x2){ # we are in this case if c=0
        z <- z0 - R^2/Conj(d/c+z0)
        w0 <- self$transform(.fromCplx(z))
        Circle$new(w0, Mod(.toCplx(w0 - self$transform(.fromCplx(z0+R)))))
      }else{
        if(circ$includes(M <- .fromCplx(-d/c))){
          alpha <- 0
          while(TRUE){
            P <- circ$pointFromAngle(alpha, degrees = FALSE)
            if(!all(P == M)){
              break
            }
            alpha <- alpha + 0.1
          }
          while(TRUE){
            Q <- circ$pointFromAngle(alpha+3)
            if(!all(Q == M)){
              break
            }
            alpha <- alpha + 0.1
          }
          A <- self$transform(P); B <- self$transform(Q)
        }else{
          A <- self$transform(circ$pointFromAngle(0))
          B <- self$transform(circ$pointFromAngle(180))
        }
        Line$new(A, B)
      }
    },

    #' @description Transformation of a line by the reference Möbius transformation.
    #' @param line a \code{Line} object
    #' @return A \code{Circle} object or a \code{Line} object.
    transformLine = function(line) {
      private[[".a"]] -> a
      private[[".b"]] -> b
      private[[".c"]] -> c
      private[[".d"]] -> d
      do <- line$directionAndOffset()
      theta <- do$direction
      gamma0 <- cos(theta) + 1i*sin(theta)
      D0 <- 2 * do$offset
      A <- -2 * Re(gamma0*c*Conj(d)) - D0*.Mod2(c)
      gamma <- Conj(gamma0)*b*Conj(c) + gamma0*Conj(d)*a + D0*Conj(c)*a
      D <- D0*.Mod2(a) + 2*Re(gamma0*Conj(b)*a)
      if(A != 0){
        Circle$new(.fromCplx(-gamma/A), sqrt(.Mod2(gamma)/A/A + D/A))
      }else{
        P <- self$transform(line$A)
        Q <- self$transform(line$B)
        Line$new(P, Q)
      }
    }

  )
)

#' Möbius transformation mapping three given points to three given points
#' @description Return a Möbius transformation which sends
#' \code{P1} to \code{Q1}, \code{P2} to \code{Q2} and \code{P3} to \code{Q3}.
#'
#' @param P1,P2,P3 three distinct points, \code{Inf} allowed
#' @param Q1,Q2,Q3 three distinct points, \code{Inf} allowed
#'
#' @return A \code{Mobius} object.
#' @export
MobiusMappingThreePoints <- function(P1, P2, P3, Q1, Q2, Q3){
  z1 <- .toCplx(P1); z2 <- .toCplx(P2); z3 <- .toCplx(P3) # Inf allowed !
  if(z1 == z2 || z1 == z3 || z2 == z3){
    stop("`P1`, `P2` and `P3` must be distinct.")
  }
  Mob1 <- .MobiusMappingThreePoints2ZeroOneInf(z1, z2, z3)
  w1 <- .toCplx(Q1); w2 <- .toCplx(Q2); w3 <- .toCplx(Q3)
  if(w1 == w2 || w1 == w3 || w2 == w3){
    stop("`Q1`, `Q2` and `Q3` must be distinct.")
  }
  Mob2 <- .MobiusMappingThreePoints2ZeroOneInf(w1, w2, w3)
  Mob1$compose(Mob2$inverse())
}
