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

    #' @description Show instance of a circle object
    #' @param ... ignored
    #' @examples Circle$new(c(0,0), 2)
    print = function(...) {
      cat("Inversion:\n")
      cat("      pole: ", toString(private[[".pole"]]), "\n", sep = "")
      cat("     power: ", toString(private[[".power"]]), "\n", sep = "")
    }
  )
)
