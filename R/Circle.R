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

    #' @description Check whether the reference circle equals another circle.
    #' @param circ a \code{Circle} object
    isEqual = function(circ){
      c0 <- private[[".center"]]; r0 <- private[[".radius"]]
      c1 <- circ$center; r1 <- circ$radius
      isTRUE(all.equal(c(c0[1L],c0[2L],r0), c(c1[1L],c1[2L],r1)))
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
        return(Line$new(A, B, FALSE, FALSE))
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
        return(Arc$new(center, r0, min(theta1,theta2), max(theta1,theta2)))
      }
      # Circle$new(I+c(Ox,Oy), r0)
      Circle$new(center, r0)
    },

    #' @description Orthogonal circle passing through two points within the reference circle.
    #' @param P1,P2 two points in the interior of the reference circle
    #' @return A \code{Circle} object.
    orthogonalThroughTwoPointsWithinCircle = function(P1, P2) {
      I <- private[[".center"]]; r <- private[[".radius"]]; r2 <- r*r
      if(c(crossprod(P1-I)) >= r2){
        stop("`P1` is not in the interior of the reference circle")
      }
      if(c(crossprod(P2-I)) >= r2){
        stop("`P2` is not in the interior of the reference circle")
      }
      iota <- Inversion$new(I, r2)
      P1prime <- iota$invert(P1); P2prime <- iota$invert(P2)
      line1 <- Line$new(P1,P1prime); line2 <- Line$new(P2,P2prime)
      perp1 <- suppressMessages(line1$perpendicular((P1+P1prime)/2))
      perp2 <- suppressMessages(line2$perpendicular((P2+P2prime)/2))
      O <- .LineLineIntersection(perp1$A, perp1$B, perp2$A, perp2$B)
      Circle$new(O, sqrt(c(crossprod(O-P1))))
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


#' Steiner chain
#' @description Return a Steiner chain of circles.
#'
#' @param c0 exterior circle, a \code{Circle} object
#' @param n number of circles, not including the inner circle; at least \code{3}
#' @param phi \code{-1 < phi < 1} controls the radii of the circles
#' @param shift any number; it produces a kind of rotation around the inner
#' circle; values between \code{0} and \code{n} cover all possibilities
#'
#' @return A list of \code{n+1} \code{Circle} objects. The inner circle is stored at the
#' last position.
#' @export
#'
#' @examples c0 <- Circle$new(c(1,1), 3)
#' chain <- SteinerChain(c0, 5, 0.3, 0.5)
#' plot(0, 0, type = "n", asp = 1, xlim = c(-4,4), ylim = c(-4,4))
#' invisible(lapply(chain, draw, lwd = 2, border = "blue"))
#' draw(c0, lwd = 2)
SteinerChain <- function(c0, n, phi, shift){
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
  lapply(circles0, function(circle) iota$invertCircle(circle))
}
