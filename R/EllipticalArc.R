#' @title R6 class representing an elliptical arc
#'
#' @description An arc is given by an ellipse (\code{Ellipse} object),
#' a starting angle and an ending angle. They are respectively named
#' \code{ell}, \code{alpha1} and \code{alpha2}.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom gsl ellint_Ecomp
EllipticalArc <- R6Class(

  "EllipticalArc",

  private = list(
    .ell = NULL,
    .alpha1 = NA_real_,
    .alpha2 = NA_real_,
    .degrees = TRUE
  ),

  active = list(
    #' @field ell get or set the ellipse
    ell = function(value) {
      if (missing(value)) {
        private[[".ell"]]
      } else {
        stopifnot(
          is(value, "Ellipse")
        )
        private[[".ell"]] <- value
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
    #' @description Create a new \code{EllipticalArc} object.
    #' @param ell the ellipse
    #' @param alpha1 the starting angle
    #' @param alpha2 the ending angle
    #' @param degrees logical, whether \code{alpha1} and \code{alpha2} are
    #' given in degrees
    #' @return A new \code{EllipticalArc} object.
    #' @examples ell <- Ellipse$new(c(-4,0), 4, 2.5, 140)
    #' EllipticalArc$new(ell, 45, 90)
    initialize = function(ell, alpha1, alpha2, degrees = TRUE) {
      stopifnot(
        is(ell, "Ellipse")
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
      private[[".ell"]] <- ell
      private[[".alpha1"]] <- alpha1
      private[[".alpha2"]] <- alpha2
      private[[".degrees"]] <- degrees
    },

    #' @description Show instance of an \code{EllipticalArc} object.
    #' @param ... ignored
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
      capt <- sapply(capture.output(ell), function(x) paste0(" ", x))
      cat("EllipticalArc:\n")
      cat(capt, "\n", sep = "\n")
      cat(" alpha1: ", sprintf("%s %s%s", alpha1, unit, s1), "\n", sep = "")
      cat(" alpha2: ", sprintf("%s %s%s", alpha2, unit, s2), "\n", sep = "")
    },

    #' @description Starting point of the reference elliptical arc.
    startingPoint = function() {
      private[[".ell"]] -> ell
      private[[".alpha1"]] -> alpha
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      ell$pointFromAngle(alpha, FALSE)
    },

    #' @description Ending point of the reference elliptical arc.
    endingPoint = function() {
      private[[".ell"]] -> ell
      private[[".alpha2"]] -> alpha
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      ell$pointFromAngle(alpha, FALSE)
    },

    #' @description Check whether the reference elliptical arc equals
    #' another elliptical arc.
    #' @param arc an \code{EllipticalArc} object
    isEqual = function(arc){
      ell0 <- private[[".ell"]]
      alpha10 <- private[[".alpha1"]]; alpha20 <- private[[".alpha2"]]
      if(!private[[".degrees"]]){
        alpha10 <- alpha10 * 180/pi
        alpha20 <- alpha20 * 180/pi
      }
      ell1 <- arc$ell
      alpha11 <- arc$alpha1; alpha21 <- arc$alpha2
      if(!arc$degrees){
        alpha11 <- alpha11 * 180/pi
        alpha21 <- alpha21 * 180/pi
      }
      ell0$isEqual(ell1) && isTRUE(all.equal(
        c(alpha10 %% 360, alpha20 %% 360),
        c(alpha11 %% 360, alpha21 %% 360)
      ))
    },

    #' @description Complementary elliptical arc of the reference elliptical arc.
    #' @examples ell <- Ellipse$new(c(-4,0), 4, 2.5, 140)
    #' arc <- EllipticalArc$new(ell, 30, 60)
    #' plot(NULL, type = "n", asp = 1, xlim = c(-8,0), ylim = c(-3.2,3.2),
    #'      xlab = NA, ylab = NA)
    #' draw(arc, lwd = 3, col = "red")
    #' draw(arc$complementaryArc(), lwd = 3, col = "green")
    complementaryArc = function() {
      EllipticalArc$new(
        private[[".ell"]],
        private[[".alpha2"]],
        private[[".alpha1"]] + ifelse(private[[".degrees"]], 360, 2*pi),
        private[[".degrees"]]
      )
    },

    #' @description The reference elliptical arc as a path.
    #' @param npoints number of points of the path
    #' @return A matrix with two columns \code{x} and \code{y} of length
    #' \code{npoints}.
    path = function(npoints = 100L) {
      k <- ifelse(private[[".degrees"]], pi/180, 1)
      alpha1 <- (private[[".alpha1"]]*k) #%% (2*pi)
      alpha2 <- (private[[".alpha2"]]*k) #%% (2*pi)
      if((alpha2 - alpha1) %% (2*pi) < sqrt(.Machine$double.eps)){
        path <- private[[".ell"]]$path(npoints-1L)
        path <- rbind(path, path[1L,])
        return(path)
      }
      dalpha <- alpha2 %% (2*pi) - alpha1 %% (2*pi)
      theta <- alpha1 +
        seq(
          from = 0,
          to = ifelse(dalpha <= 0, dalpha + 2*pi, dalpha),
          length.out = npoints
        )
      private[[".ell"]]$pointFromAngle(theta, FALSE)
    },

    #' @description The length of the elliptical arc.
    #' @return A number, the arc length.
    length = function(){
      a <- private[[".ell"]]$rmajor
      b <- private[[".ell"]]$rminor
      alpha1 <- private[[".alpha1"]]
      alpha2 <- private[[".alpha2"]]
      degrees <- private[[".degrees"]]
      if(degrees){
        if(abs(alpha1-alpha2) == 180){
          return(2*a*gsl::ellint_Ecomp(sqrt(1-b^2/a^2)))
        }
        if(abs(alpha1-alpha2) == 360){
          return(4*a*gsl::ellint_Ecomp(sqrt(1-b^2/a^2)))
        }
        flatAngle <- 180
        k <- pi/180
      }else{
        if(abs(alpha1-alpha2) == pi){
          return(2*a*gsl::ellint_Ecomp(sqrt(1-b^2/a^2)))
        }
        if(abs(alpha1-alpha2) == 2*pi){
          return(4*a*gsl::ellint_Ecomp(sqrt(1-b^2/a^2)))
        }
        flatAngle <- pi
        k <- 1
      }
      if(alpha1 > alpha2) alpha1 <- alpha1 - 2*flatAngle
      delta <- alpha1 - alpha1 %% flatAngle
      alpha1 <- alpha1 + delta
      alpha2 <- alpha2 + delta
      theta1 <- alpha1*k
      theta2 <- alpha2*k
      t1 <- atan2(a/b, 1/tan(theta1)) + theta1 - theta1 %% pi
      t2 <- atan2(a/b, 1/tan(theta2)) + theta2 - theta2 %% pi
      m <- 1 - a*a/b/b
#      integrate(function(t) sqrt(a^2*sin(t)^2+b^2*cos(t)^2), t1, t2, ...)
      b * (ellint2(t2, m) - ellint2(t1,m))
    }
  )
)
