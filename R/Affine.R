#' @title R6 class representing an affine transformation.
#'
#' @description An affine transformation is given by an invertible 2x2 matrix
#' (a linear transformation) and a vector (the "intercept").
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom stringr str_trim
Affine <- R6Class(

  "Affine",

  private = list(
    .A = matrix(NA_real_, 2L, 2L),
    .b = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field A get or set the matrix \code{A}
    A = function(value) {
      if (missing(value)) {
        private[[".A"]]
      } else {
        A <- value
        stopifnot(
          is.numeric(A),
          is.matrix(A),
          nrow(A) == 2L,
          ncol(A) == 2L,
          !any(is.na(A)),
          all(is.finite(A)),
          det(A) != 0
        )
        private[[".A"]] <- A
      }
    },

    #' @field b get or set the vector \code{b}
    b = function(value) {
      if (missing(value)) {
        private[[".b"]]
      } else {
        b <- as.vector(value)
        stopifnot(
          is.numeric(b),
          length(b) == 2L,
          !any(is.na(b)),
          all(is.finite(b))
        )
        private[[".b"]] <- b
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Affine} object.
    #' @param A the 2x2 invertible matrix of the affine transformation
    #' @param b the shift vector of the affine transformation
    #' @return A new \code{Affine} object.
    initialize = function(A, b) {
      stopifnot(
        is.numeric(A),
        is.matrix(A),
        nrow(A) == 2L,
        ncol(A) == 2L,
        !any(is.na(A)),
        all(is.finite(A)),
        det(A) != 0
      )
      b <- as.vector(b)
      stopifnot(
        is.numeric(b),
        length(b) == 2L,
        !any(is.na(b)),
        all(is.finite(b))
      )
      private[[".A"]] <- A
      private[[".b"]] <- b
    },

    #' @description Show instance of an \code{Affine} object.
    #' @param ... ignored
    #' @examples Affine$new(rbind(c(3.5,2),c(0,4)), c(-1, 1.25))
    print = function(...) {
      A <- private[[".A"]]
      captA <- capture.output(A)[-1L]
      captA[1L] <- stringr::str_trim(substring(captA[1L], 6L), "left")
      captA[2L] <- stringr::str_trim(substring(captA[2L], 6L), "left")
      if(A[1L,1L] >= 0 && A[2L,1L] < 0){
        captA[1L] <- paste0(" ", captA[1L])
      }else if(A[1L,1L] < 0 && A[2L,1L] >= 0){
        captA[2L] <- paste0(" ", captA[2L])
      }
      b <- private[[".b"]]
      captB <- capture.output(cbind(b))[-1L]
      captB[1L] <- stringr::str_trim(substring(captB[1L], 6L), "left")
      captB[2L] <- stringr::str_trim(substring(captB[2L], 6L), "left")
      if(b[1L] >= 0 && b[2L] < 0){
        captB[1L] <- paste0(" ", captB[1L])
      }else if(b[1L] < 0 && b[2L] >= 0){
        captB[2L] <- paste0(" ", captB[2L])
      }
      cat("Affine transformation Ax+b\n")
      cat(" A: / ", captA[1L], " \\\n    \\ ", captA[2L], " /\n", sep = "")
      cat(" b: / ", captB[1L], " \\\n    \\ ", captB[2L], " /\n", sep = "")
    },

    #' @description The 3x3 matrix representing the affine transformation.
    get3x3matrix = function(){
      rbind(cbind(private[[".A"]],private[[".b"]]), c(0,0,1))
    },

    #' @description The inverse affine transformation.
    inverse = function(){
      M <- solve(self$get3x3matrix())
      Affine$new(M[-3L,-3L], M[-3L,3L])
    },

    #' @description Compose the reference affine transformation with another
    #' affine transformation.
    #' @param transfo an \code{Affine} object
    #' @param left logical, whether to compose at left or at right (i.e.
    #' returns \code{f1 o f0} or \code{f0 o f1})
    #' @return An \code{Affine} object.
    compose = function(transfo, left = TRUE){
      M0 <- self$get3x3matrix()
      M1 <- transfo$get3x3matrix()
      M <- if(left) M1 %*% M0 else M0 %*% M1
      Affine$new(M[-3L,-3L], M[-3L,3L])
    },

    #' @description Transform a point or several points by the reference affine transformation.
    #' @param M a point or a two-column matrix of points, one point per row
    transform = function(M){
      if(is.matrix(M)){
        stopifnot(
          ncol(M) == 2L,
          is.numeric(M)
        )
      }else{
        M <- as.vector(M)
        stopifnot(
          is.numeric(M),
          length(M) == 2L
        )
        M <- rbind(M)
      }
      stopifnot(
        !any(is.na(M)),
        all(is.finite(M))
      )
      out <- t(private[[".A"]] %*% t(M) + private[[".b"]])
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description Transform a line by the reference affine transformation.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    transformLine = function(line){
      stopifnot(is(line, "Line"))
      A <- private[[".A"]]; b <- private[[".b"]]
      Line$new(
        c(A %*% line$A) + b,
        c(A %*% line$B) + b,
        line$extendA, line$extendB
      )
    },

    #' @description Transform an ellipse by the reference affine transformation.
    #' The result is an ellipse.
    #' @param ell an \code{Ellipse} object or a \code{Circle} object
    #' @return An \code{Ellipse} object.
    transformEllipse = function(ell){
      if(is(ell, "Circle")) ell <- .circleAsEllipse(ell)
      ABCDEF <- as.list(ell$equation())
      X <- with(ABCDEF, cbind(
        c(A, B/2, D/2),
        c(B/2, C, E/2),
        c(D/2, E/2, F)))
      Mat <- solve(self$get3x3matrix())
      Y <- t(Mat) %*% X %*% Mat
      A <- Y[1L,1L]
      B <- 2*Y[1L,2L]
      C <- Y[2L,2L]
      D <- 2*Y[1L,3L]
      E <- 2*Y[2L,3L]
      F <- Y[3L,3L]
      Delta <- B*B-4*A*C
      a_and_b <-
        - sqrt(2*(A*E*E+C*D*D-B*D*E+Delta*F)*((A+C)+c(1,-1)*sqrt((A-C)^2+B*B))) /
        Delta
      a <- a_and_b[1L]; b <- a_and_b[2L]
      x0 <- (2*C*D-B*E)/Delta
      y0 <- (2*A*E-B*D)/Delta
      theta <- atan2(C-A-sqrt((A-C)^2+B*B), B)
      degrees <- ell$degrees
      theta <- if(degrees) (theta*180/pi) %% 180 else theta %% pi
      Ellipse$new(c(x0,y0), a, b, theta, degrees = degrees)
    }
  )
)


#' Affine transformation mapping three given points to three given points
#' @description Return the affine transformation which sends
#' \code{P1} to \code{Q1}, \code{P2} to \code{Q2} and \code{P3} to \code{Q3}.
#'
#' @param P1,P2,P3 three non-collinear points
#' @param Q1,Q2,Q3 three non-collinear points
#'
#' @return An \code{Affine} object.
#' @export
AffineMappingThreePoints <- function(P1, P2, P3, Q1, Q2, Q3){
  if(.collinear(P1,P2,P3)) stop("P1, P2 and P3 are collinear.")
  if(.collinear(Q1,Q2,Q3)) stop("Q1, Q2 and Q3 are collinear.")
  f1 <- Affine$new(cbind(P2-P1, P3-P1), P1)
  f2 <- Affine$new(cbind(Q2-Q1, Q3-Q1), Q1)
  f1$inverse()$compose(f2)
}

#' Affine transformation mapping a given ellipse to a given ellipse
#' @description Return the affine transformation which transforms
#' \code{ell1} to \code{ell2}.
#'
#' @param ell1,ell2 \code{Ellipse} or \code{Circle} objects
#'
#' @return An \code{Affine} object.
#' @export
#'
#' @examples ell1 <- Ellipse$new(c(1,1), 5, 1, 30)
#' ( ell2 <- Ellipse$new(c(4,-1), 3, 2, 50) )
#' f <- AffineMappingEllipse2Ellipse(ell1, ell2)
#' f$transformEllipse(ell1)
AffineMappingEllipse2Ellipse <- function(ell1, ell2){
  if(is(ell1, "Circle")){
    a <- b <- ell1$radius
    costheta <- 1; sintheta <- 0
  }else{
    a <- ell1$rmajor; b <- ell1$rminor; theta <- ell1$alpha
    if(ell1$degrees) theta <- theta * pi/180
    costheta <- cos(theta); sintheta <- sin(theta)
  }
  f1 <-
    Affine$new(cbind(a*c(costheta,sintheta), b*c(-sintheta,costheta)), ell1$center)
  #
  if(is(ell2, "Circle")){
    a <- b <- ell2$radius
    costheta <- 1; sintheta <- 0
  }else{
    a <- ell2$rmajor; b <- ell2$rminor; theta <- ell2$alpha
    if(ell2$degrees) theta <- theta * pi/180
    costheta <- cos(theta); sintheta <- sin(theta)
  }
  f2 <-
    Affine$new(cbind(a*c(costheta,sintheta), b*c(-sintheta,costheta)), ell2$center)
  #
  f1$inverse()$compose(f2)
}
