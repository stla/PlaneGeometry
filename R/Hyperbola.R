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
      intersectionLineLine(private[[".L1"]], private[[".L2"]])
    },

    #' @description Parametric equation \eqn{O \pm cosh(t) A + sinh(t) B}
    #'   representing the hyperbola.
    #' @return The point \code{O} and the two vectors \code{A} and \code{B}
    #'   in a list.
    #' @examples
    #' L1 <- LineFromInterceptAndSlope(0, 2)
    #' L2 <- LineFromInterceptAndSlope(-2, -0.5)
    #' M <- c(4, 3)
    #' hyperbola <- Hyperbola$new(L1, L2, M)
    #' hyperbola$OAB()
    "OAB" = function() {
      O <- self$center()
      # equation O + t f1 + 1/t f2
      theta1 <- self$L1$directionAndOffset()$direction
      theta2 <- self$L2$directionAndOffset()$direction
      f10 <- c(sin(theta1), -cos(theta1))
      f20 <- c(sin(theta2), -cos(theta2))
      invMAT <- rbind(
        c(-cos(theta2), -sin(theta2)),
        c(cos(theta1), sin(theta1))
      ) / sin(theta2 - theta1)
      lambdas <- invMAT %*% (self$M - O)
      lambda1 <- lambdas[1L]
      lambda2 <- lambdas[2L]
      f1 <- lambda1 * f10
      f2 <- lambda2 * f20
      # first equation O +/- g1 cosh(t) + g2 sinh(t)
      g1 <- self$M - O
      g2 <- f1 - f2
      # vertex V1 = O + A
      t0 <- log(c(crossprod(g1-g2)) / c(crossprod(g1+g2))) / 4
      A <- cosh(t0) * g1 + sinh(t0) * g2
      # |f1|=|f2|
      lambdaEq <- c(crossprod(invMAT[1L, ], A))
      f1eq <- lambdaEq * f10
      f2eq <- lambdaEq * f20
      # tangent at v1
      tgV1 <- lambdaEq*(f10 - f20)
      # parametric representation  O +/- cosh(t) A + sinh(t) B
      B <- tgV1
      #
      list("O" = O, "A" = A, "B" = B)
    }
  )
)
