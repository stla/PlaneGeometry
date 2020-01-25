#' @title R6 class representing a circular arc
#'
#' @description An arc is given by a center, a radius, a starting angle and
#' an ending angle. They are respectively named \code{center}, \code{radius},
#' \code{alpha1} and \code{alpha2}.
#'
#' @export
#' @importFrom R6 R6Class
# #' @importFrom DescTools DrawArc
Arc <- R6Class(

  "Arc",

  private = list(
    .center = c(NA_real_, NA_real_),
    .radius = NA_real_,
    .alpha1 = NA_real_,
    .alpha2 = NA_real_,
    .degrees = TRUE
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
    #' @description Create a new \code{Arc} object.
    #' @param center the center
    #' @param radius the radius
    #' @param alpha1 the starting angle
    #' @param alpha2 the ending angle
    #' @param degrees logical, whether \code{alpha1} and \code{alpha2} are
    #' given in degrees
    #' @return A new \code{Arc} object.
    #' @examples arc <- Arc$new(c(1,1), 1, 45, 90)
    #' arc
    #' arc$center
    #' arc$center <- c(0,0)
    #' arc
    initialize = function(center, radius, alpha1, alpha2, degrees = TRUE) {
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
      degrees <- as.vector(degrees)
      stopifnot(
        is.logical(degrees),
        length(degrees) == 1L,
        !is.na(degrees)
      )
      private[[".center"]] <- center
      private[[".radius"]] <- radius
      private[[".alpha1"]] <- alpha1
      private[[".alpha2"]] <- alpha2
      private[[".degrees"]] <- degrees
    },

    #' @description Show instance of an \code{Arc} object.
    #' @param ... ignored
    #' @examples Arc$new(c(0,0), 2, pi/4, pi/2, FALSE)
    print = function(...) {
      alpha1 <- private[[".alpha1"]]
      alpha2 <- private[[".alpha2"]]
      unit <-
        ifelse(
          private[[".degrees"]],
          "degree",
          "radian"
        )
      s1 <- ifelse(alpha1 %in% c(-1,0,1), "", "s")
      s2 <- ifelse(alpha2 %in% c(-1,0,1), "", "s")
      cat("Arc:\n")
      cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
      cat(" alpha1: ", sprintf("%s %s%s", alpha1, unit, s1), "\n", sep = "")
      cat(" alpha2: ", sprintf("%s %s%s", alpha2, unit, s2), "\n", sep = "")
    },

    #' @description Starting point of the reference arc.
    startingPoint = function() {
      private[[".alpha1"]] -> alpha
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      private[[".center"]] + private[[".radius"]] * c(cos(alpha), sin(alpha))
    },

    #' @description Ending point of the reference arc.
    endingPoint = function() {
      private[[".alpha2"]] -> alpha
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      private[[".center"]] + private[[".radius"]] * c(cos(alpha), sin(alpha))
    },

    #' @description Check whether the reference arc equals another arc.
    #' @param arc an \code{Arc} object
    isEqual = function(arc){
      c0 <- private[[".center"]]; r0 <- private[[".radius"]]
      modulo <- ifelse(private[[".degrees"]], 360, 2*pi)
      alpha10 <- private[[".alpha1"]] %% modulo
      alpha20 <- private[[".alpha2"]] %% modulo
      c1 <- arc$center; r1 <- arc$radius
      alpha11 <- arc$alpha1 %% modulo; alpha21 <- arc$alpha2 %% modulo
      isTRUE(all.equal(
        c(c0, r0, alpha10, alpha20),
        c(c1, r1, alpha11, alpha21)
      ))
    },

    #' @description Complementary arc of the reference arc.
    #' @examples arc <- Arc$new(c(0,0), 1, 30, 60)
    #' plot(NULL, type = "n", asp = 1, xlim = c(-1,1), ylim = c(-1,1),
    #'      xlab = NA, ylab = NA)
    #' draw(arc, lwd = 3, col = "red")
    #' draw(arc$complementaryArc(), lwd = 3, col = "green")
    complementaryArc = function() {
      Arc$new(private[[".center"]], private[[".radius"]],
              private[[".alpha2"]],
              private[[".alpha1"]] + ifelse(private[[".degrees"]], 360, 2*pi),
              private[[".degrees"]])
    },

    #' @description The reference arc as a path.
    #' @param npoints number of points of the path
    #' @return A matrix with two columns \code{x} and \code{y} of length
    #' \code{npoints}. See "Filling the lapping area of two circles" in the
    #' vignette for an example.
    path = function(npoints = 100L) {
      k <- ifelse(private[[".degrees"]], pi/180, 1)
      alpha1 <- (private[[".alpha1"]]*k) %% (2*pi)
      alpha2 <- (private[[".alpha2"]]*k) %% (2*pi)
      dalpha <- alpha2 - alpha1
      theta <- alpha1 +
        seq(
          from = 0,
          to = ifelse(dalpha < 0, dalpha + 2*pi, dalpha),
          length.out = npoints
        )
      .circlePoints(
        theta,
        private[[".center"]], private[[".radius"]]
      )
    }
  )
)
