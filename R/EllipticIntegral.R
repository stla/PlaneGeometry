#' Extended elliptic integral of the second kind
#' @description Evaluates the extended incomplete elliptic integral of the second kind.
#' The function is vectorized in \code{m} but not in \code{phi}.
#'
#' @param phi amplitude, a number
#' @param m values of the parameter, a vector of numbers lower than \code{1/sin(phi)^2}
#' (\code{NaN} is returned if this condition is not satisfied)
#'
#' @return A numeric vector of the same length as \code{m}.
#' @export
#'
#' @details For \code{-pi/2 <= phi <= pi/2}, this is the integral of
#' \code{sqrt(1 - m*sin(t)^2)} for \code{t} between \code{0} and \code{phi}.
#' Then the function is extended to arbitrary \code{phi} by the formula
#' \code{ellint2(phi + k*pi, m) = 2*k*ellint2(pi/2, m) + ellint2(phi, m)} for
#' any integer \code{k}.
#'
#' @note This function is used to calculate the length of an elliptical arc
#' (method \code{length} of \code{EllipticalArc}).
#'
#' @examples phi <- pi/4; m <- 0.6
#' ellint2(phi, m)
#' gsl::ellint_E(phi, sqrt(m))
#' curve(ellint2(phi, x), -5, 1/sin(phi)^2)
ellint2 <- function(phi, m){
  if(phi == 0){
    0
  }else if(phi >= -pi/2 && phi <= pi/2){
    sine <- sin(phi)
    sine2 <- sine*sine
    cosine2 <- 1 - sine2
    oneminusmsine2 <- 1 - m*sine2
    sine * (gsl::ellint_RF(cosine2, oneminusmsine2, 1) -
              m * sine2 * gsl::ellint_RD(cosine2, oneminusmsine2, 1) / 3)
  }else if(phi > pi/2){
    k <- 0
    while(phi > pi/2){
      phi <- phi - pi
      k <- k + 1
    }
    2*k*ellint2(pi/2, m) + ellint2(phi, m)
  }else{
    k <- 0
    while(phi < -pi/2){
      phi <- phi + pi
      k <- k - 1
    }
    2*k*ellint2(pi/2, m) + ellint2(phi, m)
  }
}
