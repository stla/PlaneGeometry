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
          radius >= 0,
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

    #' @description Show instance of a circle object.
    #' @param ... ignored
    #' @examples Circle$new(c(0,0), 2)
    print = function(...) {
      cat("Circle:\n")
      cat(" center: ", toString(private[[".center"]]), "\n", sep = "")
      cat(" radius: ", toString(private[[".radius"]]), "\n", sep = "")
    },

    #' @description Get a point on the reference circle from its polar angle.
    #' @param alpha a number, the angle
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return The point on the circle with polar angle \code{alpha}.
    pointFromAngle = function(alpha, degrees = TRUE) {
      if(degrees) alpha <- alpha * pi/180
      private[[".center"]] + private[[".radius"]] * c(cos(alpha), sin(alpha))
    },

    #' @description Diameter of the reference circle for a given polar angle.
    #' @param alpha an angle in radians, there is one diameter for each value of
    #' \code{alpha} modulo \code{pi}
    #' @return A segment (\code{Line} object).
    #' @examples circ <- Circle$new(c(1,1), 5)
    #' diams <- lapply(c(0, pi/3, 2*pi/3), circ$diameter)
    #' plot(NULL, type="n", asp=1, xlim = c(-4,6), ylim = c(-5,7),
    #'      xlab = NA, ylab = NA)
    #' draw(circ, lwd = 2, col = "yellow")
    #' invisible(lapply(diams, draw, col = "blue"))
    diameter = function(alpha){
      t <- as.vector(alpha)
      stopifnot(
        is.numeric(t),
        length(t) == 1L,
        !is.na(t),
        is.finite(t)
      )
      O <- private[[".center"]]
      v <- private[[".radius"]] * c(cos(t), sin(t))
      Line$new(O+v, O-v, FALSE, FALSE)
    },

    #' @description Tangent of the reference circle at a given polar angle.
    #' @param alpha an angle in radians, there is one tangent for each value of
    #' \code{alpha} modulo \code{2*pi}
    #' @examples circ <- Circle$new(c(1,1), 5)
    #' tangents <- lapply(c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3), circ$tangent)
    #' plot(NULL, type="n", asp=1, xlim = c(-4,6), ylim = c(-5,7),
    #'      xlab = NA, ylab = NA)
    #' draw(circ, lwd = 2, col = "yellow")
    #' invisible(lapply(tangents, draw, col = "blue"))
    tangent = function(alpha){
      t <- as.vector(alpha)
      stopifnot(
        is.numeric(t),
        length(t) == 1L,
        !is.na(t),
        is.finite(t)
      )
      r <- private[[".radius"]]
      cost <- cos(t); sint <- sin(t)
      T <- private[[".center"]] + r*c(cost, sint)
      Line$new(T, T + c(-sint,cost))
    },

    #' @description Return the two tangents of the reference circle passing
    #' through an external point.
    #' @param P a point external to the reference circle
    #' @return A list of two \code{Line} objects, the two tangents; the
    #' tangency points are in the \code{B} field of the lines.
    tangentsThroughExternalPoint = function(P){
      P <- as.vector(P)
      stopifnot(
        is.numeric(P),
        length(P) == 2L,
        all(is.finite(P)),
        !any(is.na(P))
      )
      O <- private[[".center"]]
      if(.distance(O,P) <= private[[".radius"]]){
        stop("`P` is not external to the circle.")
      }
      M <- (O+P)/2
      circ <- Circle$new(M, .distance(O, M))
      Is <- intersectionCircleCircle(self, circ)
      list(T1 = Line$new(P, Is[[1L]]), T2 = Line$new(P, Is[[2L]]))
    },

    #' @description Check whether the reference circle equals another circle.
    #' @param circ a \code{Circle} object
    isEqual = function(circ){
      c0 <- private[[".center"]]; r0 <- private[[".radius"]]
      c1 <- circ$center; r1 <- circ$radius
      isTRUE(all.equal(c(c0[1L],c0[2L],r0), c(c1[1L],c1[2L],r1)))
    },

    #' @description Check whether the reference circle is orthogonal to a
    #' given circle
    #' @param circ a \code{Circle} object
    isOrthogonal = function(circ){
      stopifnot(is(circ, "Circle"))
      d2 <- c(crossprod(private[[".center"]]-circ$center))
      R <- private[[".radius"]]
      isTRUE(all.equal(d2, R*R + circ$radius*circ$radius))
    },

    #' @description Check whether a point belongs to the reference circle.
    #' @param M a point
    includes = function(M){
      isTRUE(all.equal(private[[".radius"]]^2,
                       c(crossprod(M-private[[".center"]]))))
    },

    #' @description Orthogonal circle passing through two points on the reference circle.
    #' @param alpha1,alpha2 two angles defining two points on the reference circle
    #' @param arc logical, whether to return only the arc at the interior of the
    #' reference circle
    #' @return A \code{Circle} object if \code{arc=FALSE}, an \code{Arc} object
    #' if \code{arc=TRUE}, or a \code{Line} object: the diameter
    #' of the reference circle defined by the two points in case when the two
    #' angles differ by \code{pi}.
    #' @examples # hyperbolic triangle
    #' circ <- Circle$new(c(5,5), 3)
    #' arc1 <- circ$orthogonalThroughTwoPointsOnCircle(0, 2*pi/3, arc = TRUE)
    #' arc2 <- circ$orthogonalThroughTwoPointsOnCircle(2*pi/3, 4*pi/3, arc = TRUE)
    #' arc3 <- circ$orthogonalThroughTwoPointsOnCircle(4*pi/3, 0, arc = TRUE)
    #' opar <- par(mar = c(0,0,0,0))
    #' plot(0, 0, type = "n", asp = 1, xlim = c(2,8), ylim = c(2,8))
    #' draw(circ)
    #' draw(arc1, col = "red", lwd = 2)
    #' draw(arc2, col = "green", lwd = 2)
    #' draw(arc3, col = "blue", lwd = 2)
    #' par(opar)
    orthogonalThroughTwoPointsOnCircle = function(alpha1, alpha2, arc = FALSE) {
      I <- private[[".center"]]; r <- private[[".radius"]]
      dalpha <- alpha1 - alpha2
      if(dalpha %% pi == 0){
        eialpha1 <- c(cos(alpha1), sin(alpha1))
        A <- I + r*eialpha1; B <- I - r*eialpha1
        return(Line$new(A, B, !arc, !arc))
      }
      r0 <- r * abs(tan(dalpha/2))
      IO <- r / cos(dalpha/2)
      center <- I + IO * c(cos((alpha1+alpha2)/2), sin((alpha1+alpha2)/2))
      # Oy <- IO * sin((alpha1+alpha2)/2)
      if(arc){
        dalpha <- (alpha2-alpha1)%%(2*pi)# - alpha1%%(2*pi)
        delta <- ifelse(dalpha >= pi, pi, 0)
        beta1 <- -pi/2 + delta
        beta2 <- beta1 - pi + dalpha
        theta1 <- beta1+alpha1 #%% (2*pi)
        theta2 <- beta2+alpha1 #%% (2*pi)
        return(
          Arc$new(center, r0, min(theta1,theta2), max(theta1,theta2), FALSE)
        )
      }
      # Circle$new(I+c(Ox,Oy), r0)
      Circle$new(center, r0)
    },

    #' @description Orthogonal circle passing through two points within the reference circle.
    #' @param P1,P2 two distinct points in the interior of the reference circle
    #' @param arc logical, whether to return the arc joining the two points
    #' instead of the circle
    #' @return A \code{Circle} object or an \code{Arc} object,
    #' or a \code{Line} object if the two points are on a diameter.
    #' @examples circ <- Circle$new(c(0,0),3)
    #' P1 <- c(1,1); P2 <- c(1, 2)
    #' ocirc <- circ$orthogonalThroughTwoPointsWithinCircle(P1, P2)
    #' arc <- circ$orthogonalThroughTwoPointsWithinCircle(P1, P2, arc = TRUE)
    #' plot(0, 0, type = "n", asp = 1, xlab = NA, ylab = NA,
    #'      xlim = c(-3, 4), ylim = c(-3, 4))
    #' draw(circ, lwd = 2)
    #' draw(ocirc, lty = "dashed", lwd = 2)
    #' draw(arc, lwd = 3, col = "blue")
    orthogonalThroughTwoPointsWithinCircle = function(P1, P2, arc = FALSE) {
      if(isTRUE(all.equal(P1, P2, check.attributes = FALSE)))
        stop("`P1` and `P2` must be distinct.")
      I <- private[[".center"]]; r <- private[[".radius"]]; r2 <- r*r
      if(.distance(P1,I) >= r2){
        stop("`P1` is not in the interior of the reference circle.")
      }
      if(.distance(P2,I) >= r2){
        stop("`P2` is not in the interior of the reference circle.")
      }
      if(.collinear(I, P1, P2)){
        return(Line$new(P1, P2, !arc, !arc))
      }
      iota <- Inversion$new(I, r2)
      P1prime <- iota$invert(P1); P2prime <- iota$invert(P2)
      line1 <- Line$new(P1,P1prime); line2 <- Line$new(P2,P2prime)
      perp1 <- suppressMessages(line1$perpendicular((P1+P1prime)/2))
      perp2 <- suppressMessages(line2$perpendicular((P2+P2prime)/2))
      O <- .LineLineIntersection(perp1$A, perp1$B, perp2$A, perp2$B)
      if(arc){
        theta1 <- atan2(P1[2L]-O[2L], P1[1L]-O[1L]) %% (2*pi)
        theta2 <- atan2(P2[2L]-O[2L], P2[1L]-O[1L]) %% (2*pi)
        Arc$new(O, sqrt(c(crossprod(O-P1))),
                min(theta1,theta2), max(theta1,theta2), FALSE)
      }else{
        Circle$new(O, sqrt(c(crossprod(O-P1))))
      }
    },

    #' @description Power of a point with respect to the reference circle.
    #' @param M point
    #' @return A number.
    power = function(M) {
      private[[".radius"]] -> radius
      c(crossprod(M - private[[".center"]])) - radius*radius
    },

    #' @description Radical center of two circles.
    #' @param circ2 a \code{Circle} object
    #' @seealso \code{\link{radicalCenter}} for the radical center of three circles.
    radicalCenter = function(circ2){
      C1 <- private[[".center"]]; C2 <- circ2$center
      k <- private[[".radius"]]^2 - circ2$radius^2;
      C1_C2 <- C2 - C1
      C1C2sqr <- c(crossprod(C1_C2))
      K <- if(C1C2sqr == 0){
        c(Inf, Inf)
      }else{
        (C1+C2)/2 + k/2 * C1_C2/C1C2sqr
      }
      K#/C1[1L] # quid if C1[1] = 0 ?
    },

    #' @description Radical axis of two circles.
    #' @param circ2 a \code{Circle} object
    #' @return A \code{Line} object.
    radicalAxis = function(circ2){
      C1 <- private[[".center"]]; C2 <- circ2$center
      if(isTRUE(all.equal(C1,C2))){
        stop("The two circles must have distinct centers.")
      }
      C1_C2 <- C2 - C1
      v <- c(-C1_C2[2L], C1_C2[1L])
      R <- self$radicalCenter(circ2)
      Line$new(R, R+v, TRUE, TRUE)
      # l <- Line$new(C1, C2, TRUE, TRUE)
      # R <- self$radicalCenter(circ2)
      # l$perpendicular(R, TRUE, TRUE)
    },

    #' @description Rotate the reference circle.
    #' @param alpha angle of rotation
    #' @param O center of rotation
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return A \code{Circle} object.
    rotate = function(alpha, O, degrees = TRUE){
      alpha <- as.vector(alpha)
      stopifnot(
        is.numeric(alpha),
        length(alpha) == 1L,
        !is.na(alpha),
        is.finite(alpha)
      )
      O <- as.vector(O)
      stopifnot(
        is.numeric(O),
        length(O) == 2L,
        !any(is.na(O)),
        all(is.finite(O))
      )
      if(degrees){
        alpha <- alpha * pi/180
      }
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      At <- private[[".center"]] - O
      RAt <- c(cosalpha*At[1L]-sinalpha*At[2L], sinalpha*At[1L]+cosalpha*At[2L])
      Circle$new(RAt + O, private[[".radius"]])
    },

    #' @description Translate the reference circle.
    #' @param v the vector of translation
    #' @return A \code{Circle} object.
    translate = function(v){
      v <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(v) == 2L,
        !any(is.na(v)),
        all(is.finite(v))
      )
      Circle$new(private[[".center"]] + v, private[[".radius"]])
    },

    #' @description Invert the reference circle.
    #' @param inversion an \code{Inversion} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invert = function(inversion){
      inversion$invertCircle(self)
    },

    #' @description Convert the reference circle to an \code{Ellipse} object.
    asEllipse = function(){
      r <- private[[".radius"]]
      Ellipse$new(private[[".center"]], r, r, 0)
    },

    #' @description Random points on or in the reference circle.
    #' @param n an integer, the desired number of points
    #' @param where \code{"in"} to generate inside the circle,
    #' \code{"on"} to generate on the circle
    #' @return The generated points in a two columns matrix with \code{n} rows.
    randomPoints = function(n, where = "in"){
      where <- match.arg(where, c("in", "on"))
      if(where == "in"){
        sims <- uniformly::runif_in_sphere(n, 2, private[[".radius"]])
        sweep(sims, 2L, private[[".center"]], "+")
      }else{
        sims <- uniformly::runif_on_sphere(n, 2, private[[".radius"]])
        sweep(sims, 2L, private[[".center"]], "+")
      }
    }
  )
)

#' Radical center
#' @description Returns the radical center of three circles.
#'
#' @param circ1,circ2,circ3 \code{Circle} objects
#'
#' @return A point.
#' @export
radicalCenter <- function(circ1, circ2, circ3){
  l1 <- circ1$radicalAxis(circ2)
  l2 <- circ1$radicalAxis(circ3)
  .LineLineIntersection(l1$A, l1$B, l2$A, l2$B)
}

#' Mid-circle(s)
#' @description Return the mid-circle(s) of two circles.
#'
#' @param circ1,circ2 \code{Circle} objects
#'
#' @return A \code{Circle} object, or a \code{Line} object, or a list of two
#' such objects.
#' @export
#'
#' @details A mid-circle of two circles is a generalized circle (i.e. a circle
#' or a line) such that the inversion on this circle swaps the two circles.
#' The case of a line appears only when the two circles have equal radii.
#'
#' @seealso \code{\link{inversionSwappingTwoCircles}}
#'
#' @examples circ1 <- Circle$new(c(5,4),2)
#' circ2 <- Circle$new(c(6,4),1)
#' midcircle <- midCircles(circ1, circ2)
#' inversionFromCircle(midcircle)
#' inversionSwappingTwoCircles(circ1, circ2)
midCircles <- function(circ1, circ2){
  stopifnot(
    is(circ1, "Circle"),
    is(circ2, "Circle")
  )

  r1 <- circ1$radius; r2 <- circ2$radius
  O1 <- circ1$center; O2 <- circ2$center

  epsilon <- sqrt(.Machine$double.eps)

  if(r1 == r2){
    if(isTRUE(all.equal(O1,O2))){ # O1=O2
      out <- list(
        C1 = circ1,
        C2 = "circles are equal; every diameter is a mid-circle"
      )
    }else{
      d2 <- c(crossprod(O1 - O2))
      sumRadii2 <- (r1+r2)^2
      I <- (O1 + O2) / 2
      O1_O2 <- O2 - O1
      v <- c(O1_O2[2L], -O1_O2[1L])
      line <- Line$new(I+v, I-v)
      if(d2 < sumRadii2){ # they intersect at two points
        out <- list(
          C1 = Circle$new(I, sqrt(abs(c(crossprod(I-O2)) - r2*r2))),
          C2 = line
        )
      }else{ # they are tangent or they do not intersect
        out <- line
      }
    }
  }else{ # r1 != r2
    d2 <- c(crossprod(O1 - O2))
    sumRadii2 <- (r1+r2)^2
    rho <- r1/r2
    if(d2 > sumRadii2 + epsilon){ # they are outside each other
      I <- O1 - rho/(1-rho)*(O2-O1)
      k <- rho * abs(c(crossprod(I-O2))-r2*r2)
      out <- Circle$new(I, sqrt(k))
    }else if(d2 < (r1-r2)^2 - epsilon){ # one contains the other
      I <- O1 + rho/(1+rho)*(O2-O1)
      k <- rho * abs(c(crossprod(I-O2))-r2*r2)
      out <- Circle$new(I, sqrt(k))
    }else if(sumRadii2 - d2 < epsilon){ # they are externally tangent
      I <- O1 - rho/(1-rho)*(O2-O1)
      k <- rho * abs(c(crossprod(I-O2))-r2*r2)
      out <- Circle$new(I, sqrt(k))
    }else if(d2 - (r1-r2)^2 < epsilon){ # they are internally tangent
      I <- O1 + rho/(1+rho)*(O2-O1)
      k <- rho * abs(c(crossprod(I-O2))-r2*r2)
      out <- Circle$new(I, sqrt(k))
    }else{ # they intersect at two points
      I1 <- O1 - rho/(1-rho)*(O2-O1)
      k1 <- rho * abs(c(crossprod(I1-O2))-r2*r2)
      I2 <- O1 + rho/(1+rho)*(O2-O1)
      k2 <- rho * abs(c(crossprod(I2-O2))-r2*r2)
      out <- list(
        C1 = Circle$new(I1, sqrt(k1)),
        C2 = Circle$new(I2, sqrt(k2))
      )
    }
  }
  out
}

#' Steiner chain
#' @description Return a Steiner chain of circles.
#'
#' @param c0 exterior circle, a \code{Circle} object
#' @param n number of circles, not including the inner circle; at least \code{3}
#' @param phi \code{-1 < phi < 1} controls the radii of the circles
#' @param shift any number; it produces a kind of rotation around the inner
#' circle; values between \code{0} and \code{n} cover all possibilities
#' @param ellipse logical; the centers of the circles of the Steiner chain lie
#' on an ellipse, and this ellipse is returned as an attribute if you set this
#' argument to \code{TRUE}
#'
#' @return A list of \code{n+1} \code{Circle} objects. The inner circle is stored at the
#' last position.
#' @export
#'
#' @examples c0 <- Circle$new(c(1,1), 3)
#' chain <- SteinerChain(c0, 5, 0.3, 0.5, ellipse = TRUE)
#' plot(0, 0, type = "n", asp = 1, xlim = c(-4,4), ylim = c(-4,4))
#' invisible(lapply(chain, draw, lwd = 2, border = "blue"))
#' draw(c0, lwd = 2)
#' draw(attr(chain, "ellipse"), lwd = 2, border = "red")
SteinerChain <- function(c0, n, phi, shift, ellipse = FALSE){
  n <- as.vector(n)
  phi <- as.vector(phi)
  shift <- as.vector(shift)
  stopifnot(
    is(c0, "Circle"),
    .isInteger(n),
    length(n) == 1L,
    n > 2,
    is.numeric(phi),
    length(phi) == 1L,
    !is.na(phi),
    -1 < phi && phi < 1,
    is.numeric(shift),
    length(shift) == 1L,
    !is.na(shift),
    is.finite(shift)
  )
  circles0 <- .SteinerChain_phi0(c0 = c0, n = n, shift = shift)
  if(phi == 0) return(circles0)
  R <- c0$radius; O <- c0$center
  invphi <- 1/phi
  I <- c(R*invphi, 0) + O
  r2 <- R*R * (invphi*invphi-1)
  iota <- Inversion$new(I, r2)
  out <- lapply(circles0, function(circle) iota$invertCircle(circle))
  if(ellipse){
    O2 <- out[[n+1L]]$center
    r <- out[[n+1L]]$radius
    c <- (O2[1L] - O[1L])/2
    a <- (r + R)/2
    b <- sqrt(a*a - c*c)
    attr(out, "ellipse") <- Ellipse$new((O+O2)/2, a, b, 0)
  }
  return(out)
}


#' Circle given by its center and a point
#' @description Return the circle given by its center and a point it
#' passes through.
#'
#' @param O the center of the circle
#' @param A a point of the circle
#'
#' @return A \code{Circle} object.
#' @export
CircleOA <- function(O, A){
  Circle$new(O, .distance(O,A))
}

#' Unit circle
#' @description Circle centered at the origin with radius 1.
#' @export
unitCircle <- Circle$new(c(0,0), 1)
