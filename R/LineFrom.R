#' @title Line from intercept and slope
#' @description Create a \code{Line} object representing the infinite line
#'   with given intercept and given slope.
#'
#' @param a intercept
#' @param b slope
#'
#' @return A \code{Line} object.
#' @export
LineFromInterceptAndSlope <- function(a, b) {
  A <- c(0, a)
  B <- c(1, a + b)
  Line$new(A = A, B = B)
}

#' @title Line from general equation
#' @description Create a \code{Line} object representing the infinite line
#'   with given equation \eqn{ax + by + c = 0}.
#'
#' @param a,b,c the parameters of the equation; \code{a} and \code{b} cannot
#'   be both zero
#'
#' @return A \code{Line} object.
#' @export
LineFromEquation <- function(a, b, c) {
  stopifnot(a != 0 || b != 0)
  if(b != 0) {
    LineFromInterceptAndSlope(a = -c/b, b = -a/b)
  } else {
    x <- c / a
    Line$new(A = c(x, 0), B = c(x, 1))
  }
}
