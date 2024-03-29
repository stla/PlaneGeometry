#' @title R6 class representing an ellipse
#'
#' @description An ellipse is given by a center, two radii (\code{rmajor}
#' and \code{rminor}), and the angle (\code{alpha}) between the major axis and
#' the horizontal direction.
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom uniformly runif_in_ellipsoid runif_on_ellipsoid
#' @importFrom Carlson elliptic_E
Ellipse <- R6Class(

  "Ellipse",

  private = list(
    .center = c(NA_real_, NA_real_),
    .rmajor = NA_real_,
    .rminor = NA_real_,
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
    #' @param alpha a number, the angle between the major axis and the
    #' horizontal direction
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
                         ifelse(alpha %in% c(0,1,-1), "degree", "degrees"),
                         ifelse(alpha %in% c(0,1,-1), "radian", "radians"))
          ), "\n", sep = "")
    },

    #' @description Check whether the reference ellipse equals an ellipse.
    #' @param ell An \code{Ellipse} object.
    isEqual = function(ell){
      if(is(ell, "Circle")) ell <- .circleAsEllipse(ell)
      private[[".center"]] -> center0
      private[[".rmajor"]] -> rmajor0
      private[[".rminor"]] -> rminor0
      private[[".alpha"]] -> alpha0
      private[[".degrees"]] -> degrees
      if(!degrees) alpha0 <- (alpha0 * 180/pi)
      alpha1 <- ell$alpha
      if(!ell$degrees) alpha1 <- (alpha1 * 180/pi)
      isTRUE(all.equal(
        c(center0, rmajor0, rminor0, alpha0 %% 180),
        c(ell$center, ell$rmajor, ell$rminor, alpha1 %% 180)
      ))
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
      isTRUE(all.equal(0, zero, check.attributes=FALSE))
    },

    #' @description Check whether a point is contained in the reference ellipse.
    #' @param M a point
    contains = function(M){
      x <- M[1L]; y <- M[2L]
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
    #' @param closed Boolean, whether to return a closed path; you don't need
    #'   a closed path if you want to plot it with
    #'   \code{\link[graphics:polygon]{polygon}}
    #' @param outer Boolean; if \code{TRUE}, the ellipse will be contained
    #'   inside the path, otherwise it will contain the path
    #' @return A matrix with two columns \code{x} and \code{y} of
    #'   length \code{npoints}.
    #' @examples
    #' library(PlaneGeometry)
    #' ell <- Ellipse$new(c(1, -1), rmajor = 3, rminor = 2, alpha = 30)
    #' innerPath <- ell$path(npoints = 10)
    #' outerPath <- ell$path(npoints = 10, outer = TRUE)
    #' bbox <- ell$boundingbox()
    #' plot(NULL, asp = 1, xlim = bbox$x, ylim = bbox$y, xlab = NA, ylab = NA)
    #' draw(ell, border = "red", lty = "dashed")
    #' polygon(innerPath, border = "blue", lwd = 2)
    #' polygon(outerPath, border = "green", lwd = 2)
    path = function(npoints = 100L, closed = FALSE, outer = FALSE){
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      if(outer) {
        .ellipsePointsOuter(
          closed,
          private[[".center"]],
          private[[".rmajor"]],
          private[[".rminor"]],
          alpha,
          as.integer(npoints)
        )
      } else {
        if(closed) {
          t_ <- seq(0, 2*pi, length.out = npoints)
        } else {
          t_ <- seq(0, 2*pi, length.out = npoints+1L)[-1L]
        }
        .ellipsePoints(
          t_,
          private[[".center"]],
          private[[".rmajor"]],
          private[[".rminor"]],
          alpha
        )
      }
    },

    #' @description Diameter and conjugate diameter of the reference ellipse.
    #' @param t a number, the diameter only depends on \code{t} modulo
    #' \code{pi}; the axes correspond to \code{t=0} and \code{t=pi/2}
    #' @param conjugate logical, whether to return the conjugate diameter as well
    #' @return A \code{Line} object or a list of two \code{Line} objects if
    #' \code{conjugate = TRUE}.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' diameters <- lapply(c(0, pi/3, 2*pi/3), ell$diameter)
    #' plot(NULL, asp = 1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell)
    #' invisible(lapply(diameters, draw))
    diameter = function(t, conjugate = FALSE){
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

    #' @description Perimeter of the reference ellipse.
    perimeter = function() {
      a <- private[[".rmajor"]]
      b <- private[[".rminor"]]
      4 * a * Re(elliptic_E(pi/2, 1-b^2/a^2, minerror = 1e-12))
    },

    #' @description Intersection point of the ellipse with the half-line
    #' starting at the ellipse center and forming angle \code{theta} with
    #' the major axis.
    #' @param theta a number, the angle, or a numeric vector
    #' @param degrees logical, whether \code{theta} is given in degrees
    #' @return A point of the ellipse if \code{length(theta)==1} or a
    #' two-column matrix of points of the ellipse if
    #' \code{length(theta) > 1} (one point per row).
    pointFromAngle = function(theta, degrees = TRUE){
      theta <- as.vector(theta)
      stopifnot(
        is.numeric(theta),
        length(theta) >= 1L,
        !any(is.na(theta)),
        all(is.finite(theta))
      )
      O <- private[[".center"]]
      a <- private[[".rmajor"]]
      b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      if(degrees) theta <- theta * pi/180
      #t <- sort(atan2(a * tan(theta), b) %% (2*pi))
      sgn <- ifelse(theta %% (2*pi) <= sqrt(.Machine$double.eps), 1, -1)
      t <- atan2(a/b, 1/tan(theta %% (2*pi))) +
        theta + sgn*sqrt(.Machine$double.eps) -
        (theta + sgn*sqrt(.Machine$double.eps)) %% pi
      # t <- ifelse(theta <= pi,
      #             atan2(a/b,1/tan(theta)),
      #             ifelse(theta <= 2*pi,
      #                    atan2(a/b,1/tan(theta)) + pi,
      #                    atan2(a/b,1/tan(theta)) + 2*pi))
      out <- .ellipsePoints(t, O, a, b, alpha)
      if(length(theta) == 1L) out <- c(out)
      out
    },

    #' @description Point of the ellipse with given eccentric angle.
    #' @param t a number, the eccentric angle in radians, or a numeric vector
    #' @return A point of the ellipse if \code{length(t)==1} or a
    #' two-column matrix of points of the ellipse if
    #' \code{length(t) > 1} (one point per row).
    pointFromEccentricAngle = function(t){
      t <- as.vector(t)
      stopifnot(
        is.numeric(t),
        length(t) >= 1L,
        !any(is.na(t)),
        all(is.finite(t))
      )
      O <- private[[".center"]]
      a <- private[[".rmajor"]]
      b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      out <- .ellipsePoints(t, O, a, b, alpha)
      if(length(t) == 1L) out <- c(out)
      out
    },

    #' @description Semi-major axis of the ellipse.
    #' @return A segment (\code{Line} object).
    semiMajorAxis = function(){
      O <- private[[".center"]]
      a <- private[[".rmajor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      O_A <- a * c(cos(alpha), sin(alpha))
      Line$new(O, O + O_A, FALSE, FALSE)
    },

    #' @description Semi-minor axis of the ellipse.
    #' @return A segment (\code{Line} object).
    semiMinorAxis = function(){
      O <- private[[".center"]]
      b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      O_B <- b * c(-sin(alpha), cos(alpha))
      Line$new(O, O + O_B, FALSE, FALSE)
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

    #' @description Tangents of the reference ellipse at a point given by
    #' its eccentric angle.
    #' @param t eccentric angle, there is one tangent for each value of \code{t}
    #' modulo \code{2*pi}; for \code{t = 0, pi/2, pi, -pi/2}, these are the
    #' tangents at the vertices of the ellipse
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' tangents <- lapply(c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3), ell$tangent)
    #' plot(NULL, asp = 1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, col = "yellow")
    #' invisible(lapply(tangents, draw, col = "blue"))
    tangent = function(t){
      t <- as.vector(t)
      stopifnot(
        is.numeric(t),
        length(t) == 1L,
        !is.na(t),
        is.finite(t)
      )
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
    },

    #' @description Normal unit vector to the ellipse.
    #' @param t a number, the eccentric angle in radians of the point of the
    #' ellipse at which we want the normal unit vector
    #' @return The normal unit vector to the ellipse at the point given by
    #' eccentric angle \code{t}.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' t_ <- seq(0, 2*pi, length.out = 13)[-1]
    #' plot(NULL, asp = 1, xlim = c(-5,7), ylim = c(-3,5),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, col = "magenta")
    #' for(i in 1:length(t_)){
    #'   t <- t_[i]
    #'   P <- ell$pointFromEccentricAngle(t)
    #'   v <- ell$normal(t)
    #'   draw(Line$new(P, P+v, FALSE, FALSE))
    #' }
    normal = function(t){
      t <- as.vector(t)
      stopifnot(
        is.numeric(t),
        length(t) == 1L,
        !is.na(t),
        is.finite(t)
      )
      O <- private[[".center"]]
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      x <- -a*sin(t); y <- b*cos(t)
      v <- c(
        sinalpha*x + cosalpha*y,
        -cosalpha*x + sinalpha*y
      )
      v / .vlength(v)
    },

    #' @description Convert angle to eccentric angle.
    #' @param theta angle between the major axis and the half-line starting
    #' at the center of the ellipse and passing through the point of interest
    #' on the ellipse
    #' @param degrees logical, whether \code{theta} is given in degrees
    #' @return The eccentric angle of the point of interest on the ellipse,
    #' in radians.
    #' @examples O <- c(1, 1)
    #' ell <- Ellipse$new(O, 5, 2, 30)
    #' theta <- 20
    #' P <- ell$pointFromAngle(theta)
    #' t <- ell$theta2t(theta)
    #' tg <- ell$tangent(t)
    #' OP <- Line$new(O, P, FALSE, FALSE)
    #' plot(NULL, asp = 1, xlim = c(-4,6), ylim = c(-2,5),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, col = "antiquewhite")
    #' points(P[1], P[2], pch = 19)
    #' draw(tg, col = "red")
    #' draw(OP)
    #' draw(ell$semiMajorAxis())
    #' text(t(O+c(1,0.9)), expression(theta))
    theta2t = function(theta, degrees = TRUE){
      theta <- as.vector(theta)
      stopifnot(
        is.numeric(theta),
        length(theta) == 1L,
        !is.na(theta),
        is.finite(theta)
      )
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      if(degrees) theta <- theta * pi/180
      sgn <- ifelse(theta %% (2*pi) <= sqrt(.Machine$double.eps), 1, -1)
      atan2(a/b, 1/tan(theta %% (2*pi))) +
        theta + sgn*sqrt(.Machine$double.eps) -
        (theta + sgn*sqrt(.Machine$double.eps)) %% pi
    },

    #' @description Regression lines. The regression line of y on x intersects
    #' the ellipse at its rightmost point and its leftmost point.
    #' The tangents at these points are vertical.
    #' The regression line of x on y intersects the ellipse at its
    #' topmost point and its bottommost point.
    #' The tangents at these points are horizontal.
    #' @return A list with two \code{Line} objects:
    #' the regression line of y on x and the regression line of x on y.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' reglines <- ell$regressionLines()
    #' plot(NULL, asp = 1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, lwd = 2)
    #' draw(reglines$YonX, lwd = 2, col = "blue")
    #' draw(reglines$XonY, lwd = 2, col = "green")
    regressionLines = function(){
      O <- private[[".center"]]
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      A <- -b*sinalpha; B <- -a*cosalpha
      t1 <- .solveTrigonometricEquation(A, B)
      A <- b*cosalpha; B <- -a*sinalpha
      t2 <- .solveTrigonometricEquation(A, B)
      pts <- .ellipsePoints(c(t1,t2), O, a, b, alpha)
      list(
        YonX = Line$new(pts[1L,], pts[2L,], FALSE, FALSE),
        XonY = Line$new(pts[3L,], pts[4L,], FALSE, FALSE)
      )
    },

    #' @description Return the smallest rectangle parallel to the axes
    #' which contains the reference ellipse.
    #' @return A list with two components: the x-limits in \code{x} and the
    #' y-limits in \code{y}.
    #' @examples ell <- Ellipse$new(c(2,2), 5, 3, 40)
    #' box <- ell$boundingbox()
    #' plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
    #' draw(ell, col = "seaShell", border = "blue")
    #' abline(v = box$x, lty = 2); abline(h = box$y, lty = 2)
    boundingbox = function(){
      O <- private[[".center"]]
      a <- private[[".rmajor"]]; b <- private[[".rminor"]]
      alpha <- private[[".alpha"]]
      if(private[[".degrees"]]) alpha <- alpha * pi/180
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      A <- -b*sinalpha; B <- -a*cosalpha
      t1 <- .solveTrigonometricEquation(A, B)
      A <- b*cosalpha; B <- -a*sinalpha
      t2 <- .solveTrigonometricEquation(A, B)
      pts <- .ellipsePoints(c(t1,t2), O, a, b, alpha)
      list(
        x = sort(c(pts[1L,1L], pts[2L,1L])),
        y = sort(c(pts[3L,2L], pts[4L,2L]))
      )
    },

    #' @description Random points on or in the reference ellipse.
    #' @param n an integer, the desired number of points
    #' @param where \code{"in"} to generate inside the ellipse,
    #' \code{"on"} to generate on the ellipse
    #' @return The generated points in a two columns matrix with \code{n} rows.
    #' @examples ell <- Ellipse$new(c(1,1), 5, 2, 30)
    #' pts <- ell$randomPoints(100)
    #' plot(NULL, type="n", asp=1, xlim = c(-4,6), ylim = c(-2,4),
    #'      xlab = NA, ylab = NA)
    #' draw(ell, lwd = 2)
    #' points(pts, pch = 19, col = "blue")
    randomPoints = function(n, where = "in"){
      where <- match.arg(where, c("in", "on"))
      S <- self$matrix()
      if(where == "in"){
        sims <- runif_in_ellipsoid(n, S, 1)
        sweep(sims, 2L, private[[".center"]], "+")
      }else{
        sims <- runif_on_ellipsoid(n, S, 1)
        sweep(sims, 2L, private[[".center"]], "+")
      }
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
  e <- eigen(S, symmetric = TRUE)
  if(any(e$values <= 0)) stop("`S` is not positive.")
  .EllipseFromCenterAndEigen(center, e)
}


#' Gaussian ellipse
#' @description Return the ellipse equal to the highest \emph{pdf} region of
#' a bivariate Gaussian distribution with a given probability.
#'
#' @param mean numeric vector of length 2, the mean of the bivariate Gaussian
#' distribution; this is the center of the ellipse
#' @param Sigma covariance matrix of the bivariate Gaussian distribution
#' @param p desired probability level, a number between 0 and 1 (strictly)
#'
#' @return An \code{Ellipse} object.
#' @export
GaussianEllipse <- function(mean, Sigma, p){
  stopifnot(
    isSymmetric(Sigma),
    p > 0, p < 1
  )
  e <- eigen(Sigma, symmetric = TRUE)
  if(any(e$values <= 0)) stop("`Sigma` is not positive.")
  r <- -2 * log1p(-p)
  e <- list(
    values = rev(1/e$values)/r,
    vectors = e$vectors %*% cbind(c(0,1),c(-1,0))
  )
  .EllipseFromCenterAndEigen(mean, e)
}

#' Ellipse equation from five points
#' @description The coefficients of the implicit equation of an ellipse from
#' five points on this ellipse.
#' @param P1,P2,P3,P4,P5 the five points
#' @return A named numeric vector.
#' @export
#' @details The implicit equation of the ellipse is
#' \code{Ax² + Bxy + Cy² + Dx + Ey + F = 0}. This function returns
#' A, B, C, D, E and F.
#' @examples ell <- Ellipse$new(c(2,3), 5, 4, 30)
#' set.seed(666)
#' pts <- ell$randomPoints(5, "on")
#' cf1 <- EllipseEquationFromFivePoints(pts[1,],pts[2,],pts[3,],pts[4,],pts[5,])
#' cf2 <- ell$equation() # should be the same up to a multiplicative factor
#' all.equal(cf1/cf1["F"], cf2/cf2["F"])
EllipseEquationFromFivePoints <- function(P1, P2, P3, P4, P5){
  P <- rbind(P1, P2, P3, P4, P5)
  if(anyDuplicated(P)) stop("The five points are not distinct.")
  x <- P[,1L]; y <- P[,2L]
  M <- cbind(x*x, x*y, y*y, x, y, 1)
  A <- det(M[,-1L])
  B <- -det(M[,-2L])
  C <- det(M[,-3L])
  if(B*B-4*A*C >= 0) stop("The five points do not lie on an ellipse.")
  D <- -det(M[,-4L])
  E <- det(M[,-5L])
  F <- -det(M[,-6L])
  c(A = A, B = B, C = C, D = D, E = E, F = F)
}


#' Ellipse from its implicit equation
#' @description Return an ellipse from the coefficients of its implicit equation.
#' @param A,B,C,D,E,F the coefficients of the equation
#' @return An \code{Ellipse} object.
#' @export
#' @details The implicit equation of the ellipse is
#' \code{Ax² + Bxy + Cy² + Dx + Ey + F = 0}. This function returns the ellipse
#' given A, B, C, D, E and F.
#' @examples ell <- Ellipse$new(c(2,3), 5, 4, 30)
#' cf <- ell$equation()
#' ell2 <- EllipseFromEquation(cf[1], cf[2], cf[3], cf[4], cf[5], cf[6])
#' ell$isEqual(ell2)
EllipseFromEquation <- function(A, B, C, D, E, F){
  stopifnot(A*C > 0)
  if(B*B-4*A*C >= 0) stop("These parameters do not define an ellipse.")
  #if(D*D + E*E <= 4*(A+C)*F) stop("These parameters do not define an ellipse.")
  Q <- rbind(c(2*A, B, D), c(B, 2*C, E), c(D, E, 2*F))
  if(det(Q) == 0) stop("These parameters do not define an ellipse.")
  M0 <- matrix(c(F, D/2, E/2, D/2, A, B/2, E/2, B/2, C), 3L, 3L)
  M <- matrix(c(A, B/2, B/2, C), 2L, 2L)
  lambda <- eigen(M, symmetric = TRUE)$values
  #if(abs(lambda[1L] - A) >= abs(lambda[1L] - C)) lambda <- rev(lambda)
  detM0 <- det(M0); detM <- det(M)
  a <- sqrt(-detM0 / (detM*lambda[1L]))
  b <- sqrt(-detM0 / (detM*lambda[2L]))
  x <- B*E - 2*C*D
  y <- B*D - 2*A*E
  phi <- if(is.nan(B/(A-C))){
    0
  }else{
    if(abs(C) > abs(A)) atan(B/(A-C))/2 else (pi/2 - atan(-B/(A-C))/2)
  }
  Ellipse$new(c(x,y)/(4*A*C - B*B), max(a,b), min(a,b), (phi*180/pi) %% 180)
}

#' Ellipse from five points
#' @description Return an ellipse from five given points on this ellipse.
#' @param P1,P2,P3,P4,P5 the five points
#' @return An \code{Ellipse} object.
#' @export
#' @examples ell <- Ellipse$new(c(2,3), 5, 4, 30)
#' set.seed(666)
#' pts <- ell$randomPoints(5, "on")
#' ell2 <- EllipseFromFivePoints(pts[1,],pts[2,],pts[3,],pts[4,],pts[5,])
#' ell$isEqual(ell2)
EllipseFromFivePoints <- function(P1, P2, P3, P4, P5){
  cf <- EllipseEquationFromFivePoints(P1, P2, P3, P4, P5)
  EllipseFromEquation(cf[1L], cf[2L], cf[3L], cf[4L], cf[5L], cf[6L])
}


#' Löwner-John ellipse (ellipse hull)
#' @description Minimum area ellipse containing a set of points.
#' @param pts the points in a two-columns matrix (one point per row); at least
#' three distinct points
#' @return An \code{Ellipse} object.
#' @export
#' @examples \donttest{pts <- cbind(rnorm(30, sd=2), rnorm(30))
#' ell <- LownerJohnEllipse(pts)
#' box <- ell$boundingbox()
#' plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
#' draw(ell, col = "seaShell")
#' points(pts, pch = 19)
#' all(apply(pts, 1, ell$contains)) # should be TRUE}
LownerJohnEllipse <- function(pts){
  stopifnot(
    is.matrix(pts),
    is.numeric(pts),
    ncol(pts) == 2L,
    nrow(pts[!duplicated(pts),]) >= 3L,
    !any(is.na(pts)),
    all(is.finite(pts))
  )
  y <- sdpt3r::minelips(pts)$y
  B <- diag(y[c(1L,2L)])
  B[2L,1L] <- B[1L,2L] <- y[3L]
  EllipseFromCenterAndMatrix(-c(chol2inv(chol(B)) %*% y[c(4L,5L)]),
                             tcrossprod(B))
}

#' @title Smallest ellipse that passes through three boundary points
#' @description Returns the smallest area ellipse which passes through
#'   three given boundary points.
#'
#' @param P1,P2,P3 three non-collinear points
#'
#' @return An \code{Ellipse} object.
#' @export
#'
#' @examples
#' P1 <- c(-1,0); P2 <- c(0, 2); P3 <- c(3,0)
#' ell <- EllipseFromThreeBoundaryPoints(P1, P2, P3)
#' ell$includes(P1); ell$includes(P2); ell$includes(P3)
EllipseFromThreeBoundaryPoints <- function(P1, P2, P3){
  if(.collinear(P1, P2, P3)){
    stop("The three points are collinear.")
  }
  points <- rbind(P1, P2, P3)
  means <- colMeans(points)
  cpoints <- sweep(points, 2L, means)
  S <- 1.5 * solve(crossprod(cpoints))
  EllipseFromCenterAndMatrix(means, S)
}

#' @title Ellipse from foci and one point
#' @description Derive the ellipse with given foci and one point on the boundary.
#'
#' @param F1,F2 points, the foci
#' @param P a point on the boundary of the ellipse
#'
#' @return An \code{Ellipse} object.
#' @export
EllipseFromFociAndOnePoint <- function(F1, F2, P){
  k <- .distance(P, F1) + .distance(P, F2)
  a <- k/2
  center <- (F1 + F2) / 2
  d <- .distance(center, F1)
  b <- sqrt(a*a - d*d)
  alpha <- atan(abs(F1[2]-F2[2])/abs(F1[1]-F2[1]))
  Ellipse$new(center, a, b, alpha, degrees = FALSE)
}


#' @title Fit an ellipse
#' @description Fit an ellipse to a set of points.
#'
#' @param points numeric matrix with two columns, one point per row
#'
#' @return An \code{Ellipse} object representing the fitted ellipse. The
#'   residual sum of squares is given in the \code{RSS} attribute.
#' @export
#' @importFrom fitConic fitConic
#'
#' @examples library(PlaneGeometry)
#' # We add some noise to 30 points on an ellipse:
#' ell <- Ellipse$new(c(1, 1), 3, 2, 30)
#' set.seed(666L)
#' points <- ell$randomPoints(30, "on") + matrix(rnorm(30*2, sd = 0.2), ncol = 2)
#' # Now we fit an ellipse to these points:
#' ellFitted <- fitEllipse(points)
#' # let's draw all this stuff:
#' box <- ell$boundingbox()
#' plot(NULL, asp = 1, xlim = box$x, ylim = box$y, xlab = NA, ylab = NA)
#' draw(ell, border = "blue", lwd = 2)
#' points(points, pch = 19)
#' draw(ellFitted, border = "green", lwd = 2)
fitEllipse <- function(points){
  if(!is.matrix(points) || !is.numeric(points)){
    stop("The `points` argument must be a numeric matrix.", call. = TRUE)
  }
  if(ncol(points) != 2L){
    stop("The `points` matrix must have two columns.", call. = TRUE)
  }
  if(any(is.na(points))){
    stop("Points with missing values are not allowed.", call. = TRUE)
  }
  fit <- fitConic(points, conicType = "e")
  if(fit[["exitCode"]] != 1){
    stop("The ellipse fitting has failed.", call. = TRUE)
  }
  cfs <- fit[["parA"]]
  fittedEllipse <- EllipseFromEquation(
    cfs[1L], cfs[2L], cfs[3L], cfs[4L], cfs[5L], cfs[6L]
  )
  attr(fittedEllipse, "RSS") <- fit[["RSS"]]
  fittedEllipse
}

#' @title Maximum area ellipse inscribed in a convex polygon
#' @description Computes the ellipse inscribed in a convex polygon with
#'   maximum area.
#'
#' @param points the vertices of the polygon in a two-columns matrix; their
#'   order has no importance, since the procedure takes the convex hull of
#'   these points (and does not check the convexity)
#' @param verbose argument passed to \code{\link[CVXR:psolve]{psolve}}
#'
#' @return An \code{Ellipse} object. The status of the optimization problem
#'   is given as an attribute of this ellipse. A warning is thrown if it is
#'   not optimal.
#' @export
#' @seealso \code{\link{maxAreaInscribedCircle}}
#' @importFrom rcdd makeV scdd
#' @importFrom CVXR Variable Minimize log_det norm2 Problem psolve
#' @examples
#' hexagon <- rbind(
#'   c(-1.7, -1),
#'   c(-1.4, 0.4),
#'   c(0.3, 1.3),
#'   c(1.7, 0.6),
#'   c(1.3, -0.3),
#'   c(-0.4, -1.8)
#' )
#' opar <- par(mar = c(2, 2, 1, 1))
#' plot(NULL, xlim=c(-2, 2), ylim=c(-2, 2), xlab = NA, ylab = NA, asp = 1)
#' points(hexagon, pch = 19)
#' polygon(hexagon)
#' ell <- maxAreaInscribedEllipse(hexagon)
#' draw(ell, col = "yellow2", border = "blue", lwd = 2)
#' par(opar)
#' # check optimization status:
#' attr(ell, "status")
maxAreaInscribedEllipse <- function(points, verbose = FALSE) {
  if(!is.matrix(points) || !is.numeric(points)){
    stop("The `points` argument must be a numeric matrix.", call. = TRUE)
  }
  if(ncol(points) != 2L){
    stop("The `points` matrix must have two columns.", call. = TRUE)
  }
  if(nrow(points) < 3L){
    stop("The `points` matrix must have at least three rows.", call. = TRUE)
  }
  if(any(is.na(points))){
    stop("Points with missing values are not allowed.", call. = TRUE)
  }
  # linear inequalities
  V <- makeV(points)
  H <- scdd(V)[["output"]]
  A <- - H[, -c(1L, 2L)]
  b <- H[, 2L]
  # problem variables
  Bvar <- Variable(2L, 2L, symmetric = TRUE)
  dvar <- Variable(2L)
  # objective
  objective <- Minimize(-log_det(Bvar))
  #constraints
  constraints <- list()
  for(i in 1L:nrow(A)) {
    constraints <- append(
      constraints, list(norm2(Bvar %*% A[i, ]) + sum(A[i, ]*dvar) <= b[i])
    )
  }
  # solve the problem
  program <- Problem(objective, constraints)
  solution <- psolve(program, solver = "SCS", verbose = verbose)
  status <- solution[["status"]]
  if(status != "optimal") {
    warning("Non-optimal solution.")
  }
  # get solutions
  B <- solution$getValue(Bvar)
  d <- c(solution$getValue(dvar))
  # get ellipse
  aff <- Affine$new(B, d)
  unitcircle <- CircleOA(c(0, 0), c(1, 0))
  ell <- aff$transformEllipse(unitcircle)
  attr(ell, "status") <- status
  ell
}
