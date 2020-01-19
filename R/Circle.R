#' @title R6 class representing a circle
#'
#' @description A circle is given by a center and a radius,
#' named \code{center} and \code{radius}.
#'
#' @export
#' @importFrom R6 R6Class
Circle <- R6Class(

  "Circle",

  private = list(
    .center = c(NA_real_, NA_real_),
    .radius = NA_real_
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
          !any(is.na(center))
        )
        private[[".center"]] <- center
      }
    },

    #' @field radius get or set the radius
    radius = function(value) {
      if (missing(value)) {
        private[[".radius"]]
      } else {
        radius <- as.vector(value)
        stopifnot(
          is.numeric(radius),
          length(radius) == 1L,
          !is.na(radius)
        )
        private[[".radius"]] <- radius
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Circle} object.
    #' @param center the center
    #' @param radius the radius
    #' @return A new \code{Circle} object.
    #' @examples circ <- Circle$new(c(1,1), 1)
    #' circ
    #' circ$center
    #' circ$center <- c(0,0)
    #' circ
    initialize = function(center, radius) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center))
      )
      radius <- as.vector(radius)
      stopifnot(
        is.numeric(radius),
        length(radius) == 1L,
        radius >= 0,
        !is.na(radius)
      )
      private[[".center"]] <- center
      private[[".radius"]] <- radius
    },

    #' @description Show instance of a circle object
    #' @param ... ignored
    #' @examples Circle$new(c(0,0), 2)
    print = function(...) {
      cat("Circle:\n")
      cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
    }


  )
)
