#' @title R6 class representing a circular arc
#'
#' @description An arc is given by a center, a radius, a starting angle and
#' an ending angle. They are respectively named \code{center}, \code{radius},
#' \code{alpha1} and \code{alpha2}.
#'
#' @export
#' @importFrom R6 R6Class
Arc <- R6Class(

  "Arc",

  private = list(
    .center = c(NA_real_, NA_real_),
    .radius = NA_real_,
    .alpha1 = NA_real_,
    .alpha2 = NA_real_
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

    #' @field radius get or set the radius
    radius = function(value) {
      if (missing(value)) {
        private[[".radius"]]
      } else {
        radius <- as.vector(value)
        stopifnot(
          is.numeric(radius),
          length(radius) == 1L,
          radius >= 0,
          !is.na(radius),
          is.finite(radius)
        )
        private[[".radius"]] <- radius
      }
    },

    #' @field alpha1 get or set the starting angle
    alpha1 = function(value) {
      if (missing(value)) {
        private[[".alpha1"]]
      } else {
        alpha1 <- as.vector(value)
        stopifnot(
          is.numeric(alpha1),
          length(alpha1) == 1L,
          !is.na(alpha1),
          is.finite(alpha1)
        )
        private[[".alpha1"]] <- alpha1
      }
    },

    #' @field alpha2 get or set the ending angle
    alpha2 = function(value) {
      if (missing(value)) {
        private[[".alpha2"]]
      } else {
        alpha2 <- as.vector(value)
        stopifnot(
          is.numeric(alpha2),
          length(alpha2) == 1L,
          !is.na(alpha2),
          is.finite(alpha2)
        )
        private[[".alpha2"]] <- alpha2
      }
    }


  ),

  public = list(
    #' @description Create a new \code{Arc} object.
    #' @param center the center
    #' @param radius the radius
    #' @param alpha1 the starting angle
    #' @param alpha2 the ending angle
    #' @return A new \code{Arc} object.
    #' @examples arc <- Arc$new(c(1,1), 1, pi/4, pi/2)
    #' arc
    #' arc$center
    #' arc$center <- c(0,0)
    #' arc
    initialize = function(center, radius, alpha1, alpha2) {
      center <- as.vector(center)
      stopifnot(
        is.numeric(center),
        length(center) == 2L,
        !any(is.na(center)),
        all(is.finite(center))
      )
      radius <- as.vector(radius)
      stopifnot(
        is.numeric(radius),
        length(radius) == 1L,
        radius >= 0,
        !is.na(radius),
        is.finite(radius)
      )
      alpha1 <- as.vector(alpha1)
      stopifnot(
        is.numeric(alpha1),
        length(alpha1) == 1L,
        !is.na(alpha1),
        is.finite(alpha1)
      )
      alpha2 <- as.vector(alpha2)
      stopifnot(
        is.numeric(alpha2),
        length(alpha2) == 1L,
        !is.na(alpha2),
        is.finite(alpha2)
      )
      private[[".center"]] <- center
      private[[".radius"]] <- radius
      private[[".alpha1"]] <- alpha1
      private[[".alpha2"]] <- alpha2
    },

    #' @description Show instance of a circle object.
    #' @param ... ignored
    #' @examples Arc$new(c(0,0), 2, pi/4, pi/2)
    print = function(...) {
      cat("Arc:\n")
      cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
      cat(" alpha1: ", toString(private[[".alpha1"]]), "\n", sep = "")
      cat(" alpha2: ", toString(private[[".alpha2"]]), "\n", sep = "")
      # if((private[[".alpha1"]]-private[[".alpha2"]]) %% pi == 0){
      #   message("")
      # }
    },

    #' @description Check whether the reference arc equals another arc.
    #' @param arc an \code{Arc} object
    isEqual = function(arc){
      c0 <- private[[".center"]]; r0 <- private[[".radius"]]
      alpha10 <- private[[".alpha1"]] %% (2*pi)
      alpha20 <- private[[".alpha2"]] %% (2*pi)
      c1 <- arc$center; r1 <- arc$radius
      alpha11 <- arc$alpha1 %% (2*pi); alpha21 <- arc$alpha2 %% (2*pi)
      isTRUE(all.equal(
        c(c0[1L],c0[2L],r0, alpha10, alpha20),
        c(c1[1L],c1[2L],r1, alpha11, alpha21)
      ))
    },

    #' @description Complementary arc of the reference arc.
    complementaryArc = function() {
      Arc$new(private[[".center"]], private[[".radius"]],
              private[[".alpha2"]], private[[".alpha1"]])
    }

  )

)
