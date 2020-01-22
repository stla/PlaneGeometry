#' @title R6 class representing a Möbius transformation.
#'
#' @description A Möbius transformation is given by a matrix of complex numbers
#' with non-null determinant.
#'
#' @seealso \code{\link{MobiusFixingThreePoints}} to create some Möbius
#' transformations.
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

    #' @description Transformation of a point by the reference Möbius transformation.
    #' @param M a point or \code{Inf}
    #' @return A point or \code{Inf}, the image of \code{M}.
    #' @examples Mob <- Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
    #' Mob$transform(c(1,1))
    transform = function(M) {
      private[[".a"]] -> a
      private[[".b"]] -> b
      private[[".c"]] -> c
      private[[".d"]] -> d
      if(isTRUE(all.equal(M, Inf))){
        if(c == 0) Inf else .fromCplx(a/c)
      }else{
        z <- .toCplx(M)
        if(z == -d/c){
          Inf
        }else{
          .fromCplx((a*z+b)/(c*z+d))
        }
      }
    }

  )
)
