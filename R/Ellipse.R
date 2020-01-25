#' @title R6 class representing an ellipse
#'
#' @description An ellipse is given by a center, two radii (\code{rmajor} and \code{rminor}),
#' and the angle (\code{alpha}) between the major axis and the horizontal direction.
#'
#' @export
#' @importFrom R6 R6Class
# #' @importFrom DescTools DrawEllipse
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
        rminor <- private[[".rminor"]]
        stopifnot(
          is.numeric(rmajor),
          length(rmajor) == 1L,
          !is.na(rmajor),
          is.finite(rmajor),
          rmajor > 0,
          rmajor >= rminor
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
        rmajor <- private[[".rmajor"]]
        stopifnot(
          is.numeric(rminor),
          length(rminor) == 1L,
          !is.na(rminor),
          is.finite(rminor),
          rminor > 0,
          rminor <= rmajor
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
        rminor > 0,
        rminor <= rmajor
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
                         ifelse(alpha %in% c(0,1), "degree", "degrees"),
                         ifelse(alpha %in% c(0,1), "radian", "radians"))
          ), "\n", sep = "")
    },

    #' @description The coefficients of the implicit equation of the ellipse.
    #' @return A named numeric vector.
    #' @details The implicit equation of the ellipse is
    #' \code{Ax² + Bxy + Cy² + Dx + Ey + F = 0}. This method returns
    #' A, B, C, D, E and F.
    equation = function(){
      private[[".center"]] -> center
      private[[".rmajor"]]^2 -> a2
      private[[".rminor"]]^2 -> b2
      private[[".alpha"]] -> alpha
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      x <- center[1L]; y <- center[2L]
      sine <- sin(alpha); cosine <- cos(alpha)
      sine2 <- sine*sine; cosine2 <- 1-sine2
      A <- a2*sine2 + b2*cosine2
      B <- 2*(b2-a2)*sine*cosine
      C <- a2*cosine2 + b2*sine2
      D <- -2*A*x - B*y
      E <- -B*x - 2*C*y
      F <- A*x*x + B*x*y + C*y*y - a2*b2
      c(A = A, B = B, C = C, D = D, E = E, F = F)
    },

    #' @description Check whether a point lies on the reference ellipse.
    #' @param M a point
    includes = function(M){
      ABCDEF <- as.list(self$equation())
      x <- M[1L]; y <- M[2L]
      zero <- with(ABCDEF, A*x*x + B*x*y + C*y*y + D*x + E*y + F)
      isTRUE(all.equal(0, zero))
    },

    #' @description Check whether a point is contained in the reference ellipse.
    #' @param M a point
    contains = function(M){
      ABCDEF <- as.list(self$equation())
      with(ABCDEF, A*x*x + B*x*y + C*y*y + D*x + E*y + F) <= 0
    },

    #' @description Returns the 2x2 matrix \code{S} associated to the reference
    #' ellipse. The equation of the ellipse is \code{t(M-O) \%*\% S \%*\% (M-O) = 1}.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 1, 30)
    #' S <- ell$matrix()
    #' O <- ell$center
    #' pts <- ell$path(4L) # four points on the ellipse
    #' apply(pts, 1L, function(M) t(M-O) %*% S %*% (M-O))
    matrix = function(){
      ABCDEF <- as.list(self$equation())
      X <- with(ABCDEF, cbind(
        c(A, B/2, D/2),
        c(B/2, C, E/2),
        c(D/2, E/2, F)))
      K <- -det(X) / with(ABCDEF, A*C - B*B/4)
      X[-3L,-3L] / K
    },

    #' @description Path that forms the reference ellipse.
    #' @param npoints number of points of the path
    #' @return A matrix with two columns \code{x} and \code{y} of
    #' length \code{npoints}.
    path = function(npoints = 100L){
      center <- private[[".center"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      .ellipsePoints(
        seq(0, 2*pi, length.out = npoints+1L)[-1L],
        private[[".center"]],
        private[[".rmajor"]],
        private[[".rminor"]],
        alpha
      )
      # DrawEllipse(center[1L], center[2L],
      #             radius.x = private[[".rmajor"]],
      #             radius.y = private[[".rminor"]],
      #             rot = alpha, plot = FALSE)
    },

    #' @description Diameter and conjugate diameter of the reference ellipse.
    #' @param t a number, the diameter only depends on \code{t} modulo
    #' \code{pi}; the axes correspond to \code{t=0} and \code{t=pi/2}
    #' @param conjugate logical, whether to return the conjugate diameter as well
    #' @return A \code{Line} object or a list of two \code{Line} objects if
    #' \code{conjugate = TRUE}.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' diameters <- lapply(c(0, pi/3, 2*pi/3), ell$diameter)
    #' plot(NULL, type="n", asp=1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell)
    #' invisible(lapply(diameters, draw))
    diameter = function(t, conjugate = FALSE){
      center <- private[[".center"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      ts <- if(conjugate){
        c(t, t+pi, t+pi/2, t-pi/2)
      }else{
        c(t, t+pi)
      }
      pts <- .ellipsePoints(
        ts,
        private[[".center"]],
        private[[".rmajor"]],
        private[[".rminor"]],
        alpha
      )
      if(conjugate){
        list(
          Line$new(pts[1L,], pts[2L,], FALSE, FALSE),
          Line$new(pts[3L,], pts[4L,], FALSE, FALSE)
        )
      }else{
        Line$new(pts[1L,], pts[2L,], FALSE, FALSE)
      }
    },

    #' @description Foci of the reference ellipse.
    #' @return A list with the two foci.
    foci = function(){
      O <- private[[".center"]]
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      e <- sqrt(1 - b*b/a/a)
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      O_A <- a * c(cos(alpha), sin(alpha))
      list(F1 = O + e*O_A, F2 = O - e*O_A)
    },

    #' @description Tangents of the reference ellipse.
    #' @param t an angle, there is one tangent for each value of \code{t}
    #' modulo \code{2*pi}; for \code{t = 0, pi/2, pi, -pi/2}, these are the
    #' tangents at the vertices of the ellipse.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' tangents <- lapply(c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3), ell$tangent)
    #' plot(NULL, type="n", asp=1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, col = "yellow")
    #' invisible(lapply(tangents, draw, col = "blue"))
    tangent = function(t){
      O <- private[[".center"]]
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      x <- a*cos(t); y <- b*sin(t)
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      T <- c(
        O[1L] + cosalpha*x - sinalpha*y,
        O[2L] + sinalpha*x + cosalpha*y
      )
      x <- -a*sin(t); y <- b*cos(t)
      v <- c(
        cosalpha*x - sinalpha*y,
        sinalpha*x + cosalpha*y
      )
      Line$new(T, T+v)
    }
  )
)

#' Ellipse from center and matrix
#'
#' @description Returns the ellipse of equation
#' \code{t(X-center) \%*\% S \%*\% (X-center) = 1}.
#'
#' @param center a point, the center of the ellipse
#' @param S a positive symmetric matrix
#'
#' @return An \code{Ellipse} object.
#' @export
#'
#' @examples ell <- Ellipse$new(c(2,3), 4, 2, 20)
#' S <- ell$matrix()
#' EllipseFromCenterAndMatrix(ell$center, S)
EllipseFromCenterAndMatrix <- function(center, S){
  stopifnot(isSymmetric(S))
  e <- eigen(chol2inv(chol(S)), symmetric = TRUE)
  if(any(e$values <= 0)) stop("`S` is not positive.")
  v <- e$vectors[,1L]
  alpha <- (atan2(v[2L],v[1L]) * 180/pi) %% 180
  a <- .vlength(v/sqrt(c(t(v) %*% S %*% v)))
  b <- a * sqrt(e$values[2L]/e$values[1L])
  Ellipse$new(center, a, b, alpha)
}
