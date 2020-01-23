#' @title R6 class representing an ellipse
#'
#' @description An ellipse is given by a center, two radii (\code{rmajor} and \code{rminor}),
#' and the angle (\code{alpha}) between the major axis and the horizontal direction.
#'
#' @export
#' @importFrom R6 R6Class
Ellipse <- R6Class(

  "Ellipse",

  private = list(
    .center = c(NA_real_, NA_real_),
    .rmajor = c(NA_real_, NA_real_),
    .rminor = c(NA_real_, NA_real_),
    .alpha = NA_real_,
    .degrees = NA
  ),

  active = list(
    #' @field center get or set the center
    center = function(value) {
      if (missing(value)) {
        private[[".center"]]
      } else {
        center <- as.vector(value)
        stopifnot(
          is.numeric(center),
          length(center) == 2L,
          !any(is.na(center)),
          all(is.finite(center))
        )
        private[[".center"]] <- center
      }
    },

    #' @field rmajor get or set the major radius of the ellipse
    rmajor = function(value) {
      if (missing(value)) {
        private[[".rmajor"]]
      } else {
        rmajor <- as.vector(value)
        stopifnot(
          is.numeric(rmajor),
          length(rmajor) == 1L,
          !is.na(rmajor),
          is.finite(rmajor),
          rmajor > 0
        )
        private[[".rmajor"]] <- rmajor
      }
    },

    #' @field rminor get or set the minor radius of the ellipse
    rminor = function(value) {
      if (missing(value)) {
        private[[".rminor"]]
      } else {
        rminor <- as.vector(value)
        stopifnot(
          is.numeric(rminor),
          length(rminor) == 1L,
          !is.na(rminor),
          is.finite(rminor),
          rminor > 0
        )
        private[[".rminor"]] <- rminor
      }
    },

    #' @field alpha get or set the angle of the ellipse
    alpha = function(value) {
      if (missing(value)) {
        private[[".alpha"]]
      } else {
        alpha <- as.vector(value)
        stopifnot(
          is.numeric(alpha),
          length(alpha) == 1L,
          !is.na(alpha),
          is.finite(alpha)
        )
        private[[".alpha"]] <- alpha
      }
    },

    #' @field degrees get or set the \code{degrees} field
    degrees = function(value) {
      if (missing(value)) {
        private[[".degrees"]]
      } else {
        degrees <- as.vector(value)
        stopifnot(
          is.logical(degrees),
          length(degrees) == 1L,
          !is.na(degrees)
        )
        private[[".degrees"]] <- degrees
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Ellipse} object.
    #' @param center a point, the center of the rotation
    #' @param rmajor positive number, the major radius
    #' @param rminor positive number, the minor radius
    #' @param alpha a number, the angle between the major axis and the horizontal direction
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return A new \code{Ellipse} object.
    #' @examples Ellipse$new(c(1,1), 3, 2, 30)
    initialize = function(center, rmajor, rminor, alpha, degrees = TRUE) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      rmajor <- as.vector(rmajor)
      stopifnot(
        is.numeric(rmajor),
        length(rmajor) == 1L,
        !is.na(rmajor),
        is.finite(rmajor),
        rmajor > 0
      )
      rminor <- as.vector(rminor)
      stopifnot(
        is.numeric(rminor),
        length(rminor) == 1L,
        !is.na(rminor),
        is.finite(rminor),
        rminor > 0
      )
      alpha <- as.vector(alpha)
      stopifnot(
        is.numeric(alpha),
        length(alpha) == 1L,
        !is.na(alpha),
        is.finite(alpha)
      )
      degrees <- as.vector(degrees)
      stopifnot(
        is.logical(degrees),
        length(degrees) == 1L,
        !is.na(degrees)
      )
      private[[".center"]] <- center
      private[[".rmajor"]] <- rmajor
      private[[".rminor"]] <- rminor
      private[[".alpha"]] <- alpha
      private[[".degrees"]] <- degrees
    },

    #' @description Show instance of an \code{Ellipse} object.
    #' @param ... ignored
    print = function(...) {
      private[[".center"]] -> center
      private[[".rmajor"]] -> rmajor
      private[[".rminor"]] -> rminor
      private[[".alpha"]] -> alpha
      private[[".degrees"]] -> degrees
      cat("Ellipse:\n")
      cat("       center: ", toString(center), "\n", sep = "")
      cat(" major radius: ", toString(rmajor), "\n", sep = "")
      cat(" minor radius: ", toString(rminor), "\n", sep = "")
      cat("        angle: ",
          sprintf("%s %s", alpha,
                  ifelse(degrees,
                         ifelse(theta %in% c(0,1), "degree", "degrees"),
                         ifelse(theta %in% c(0,1), "radian", "radians"))
          ), "\n", sep = "")
    }
  )
)
