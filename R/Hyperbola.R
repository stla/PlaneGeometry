#' @title R6 class representing a hyperbola
#'
#' @description A hyperbola is given by two intersecting asymptotes, named
#'   \code{L1} and \code{L2}, and a point on this hyperbola, named \code{M}.
#'
#' @export
#' @importFrom R6 R6Class
Hyperbola <- R6Class(

  "Hyperbola",

  private = list(
    .L1 = NULL,
    .L2 = NULL,
    .M = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field L1 get or set the asymptote \code{L1}
    L1 = function(value) {
      if(missing(value)) {
        private[[".L1"]]
      } else {
        L1 <- value
        stopifnot(
          inherits(L1, "Line")
        )
        private[[".L1"]] <- L1
      }
    },

    #' @field L2 get or set the asymptote \code{L2}
    L2 = function(value) {
      if(missing(value)) {
        private[[".L2"]]
      } else {
        L2 <- value
        stopifnot(
          inherits(L2, "Line")
        )
        private[[".L2"]] <- L2
      }
    },

    #' @field M get or set the point \code{M}
    M = function(value) {
      if(missing(value)) {
        private[[".M"]]
      } else {
        M <- as.vector(value)
        stopifnot(
          is.numeric(M),
          length(M) == 2L,
          !any(is.na(M)),
          all(is.finite(M))
        )
        private[[".M"]] <- M
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Hyperbola} object.
    #' @param L1,L2 two intersecting lines given as \code{Line} objects, the
    #'   asymptotes
    #' @param M a point on the hyperbola
    #' @return A new \code{Hyperbola} object.
    initialize = function(L1, L2, M) {
      stopifnot(
        inherits(L1, "Line")
      )
      stopifnot(
        inherits(L2, "Line")
      )
      M <- as.vector(M)
      stopifnot(
        is.numeric(M),
        length(M) == 2L,
        !any(is.na(M)),
        all(is.finite(M))
      )
      if(L1$isEqual(L2)) {
        stop("`L1` and `L2` are equal.")
      }
      if(L1$isParallel(L2)) {
        stop("`L1` and `L2` are parallel.")
      }
      if(L1$includes(M)) {
        stop("`M` is on `L1`.")
      }
      if(L2$includes(M)) {
        stop("`M` is on `L2`.")
      }
      private[[".L1"]] <- L1
      private[[".L2"]] <- L2
      private[[".M"]] <- M
    },

    #' @description Center of the hyperbola.
    #' @return The center of the hyperbola, i.e. the point where
    #'   the two asymptotes meet each other.
    "center" = function() {
      intersectionLineLine(self$L1, self$L2)
    },

    "Oab" = function() {

    }
  )
)
