#' @title R6 class representing a line
#'
#' @description A line is given by two distinct points,
#' named \code{A} and \code{B}, and two logical values \code{extendA}
#' and \code{extendB}, indicating whether the line must be extended
#' beyond \code{A} and \code{B} respectively.
#'
#' @export
#' @importFrom R6 R6Class
Line <- R6Class(

  "Line",

  private = list(
    .A = c(NA_real_, NA_real_),
    .B = c(NA_real_, NA_real_),
    .extendA = NA,
    .extendB = NA
  ),

  active = list(
    #' @field A get or set the point A
    A = function(value) {
      if (missing(value)) {
        private[[".A"]]
      } else {
        A <- as.vector(value)
        stopifnot(
          is.numeric(A),
          length(A) == 2L,
          !any(is.na(A))
        )
        private[[".A"]] <- A
      }
    },

    #' @field B get or set the point B
    B = function(value) {
      if (missing(value)) {
        private[[".B"]]
      } else {
        B <- as.vector(value)
        stopifnot(
          is.numeric(B),
          length(B) == 2L,
          !any(is.na(B))
        )
        private[[".B"]] <- B
      }
    },

    #' @field extendA get or set \code{extendA}
    extendA = function(value){
      if (missing(value)) {
        private[[".extendA"]]
      } else {
        extendA <- as.vector(value)
        stopifnot(
          is.logical(extendA),
          length(extendA) == 1L,
          !is.na(extendA)
        )
        private[[".extendA"]] <- extendA
      }
    },

    #' @field extendB get or set \code{extendB}
    extendB = function(value){
      if (missing(value)) {
        private[[".extendB"]]
      } else {
        extendB <- as.vector(value)
        stopifnot(
          is.logical(extendB),
          length(extendB) == 1L,
          !is.na(extendB)
        )
        private[[".extendB"]] <- extendB
      }
    }

  ),

  public = list(
    #' @description Create a new \code{Line} object.
    #' @param A,B points
    #' @param extendA,extendB logical values
    #' @return A new \code{Line} object.
    #' @examples l <- Line$new(c(1,1), c(1.5,1.5), FALSE, TRUE)
    #' l
    #' l$A
    #' l$A <- c(0,0)
    #' l
    initialize = function(A, B, extendA = TRUE, extendB = TRUE) {
      A <- as.vector(A); B <- as.vector(B)
      stopifnot(
        is.numeric(A),
        length(A) == 2L,
        !any(is.na(A))
      )
      stopifnot(
        is.numeric(B),
        length(B) == 2L,
        !any(is.na(B))
      )
      stopifnot(any(A != B))
      extendA <- as.vector(extendA); extendB <- as.vector(extendB)
      stopifnot(
        is.logical(extendA),
        length(extendA) == 1L,
        !is.na(extendA)
      )
      stopifnot(
        is.logical(extendB),
        length(extendB) == 1L,
        !is.na(extendB)
      )
      private[[".A"]] <- A
      private[[".B"]] <- B
      private[[".extendA"]] <- extendA
      private[[".extendB"]] <- extendB
    },

    #' @description Show instance of a line object.
    #' @param ... ignored
    #' @examples Line$new(c(0,0), c(1,0), FALSE, TRUE)
    print = function(...) {
      extendA <- private[[".extendA"]]; extendB <- private[[".extendB"]]
      cat("Line:\n")
      cat("       A: ", toString(private[[".A"]]), "\n", sep = "")
      cat("       B: ", toString(private[[".B"]]), "\n", sep = "")
      cat(" extendA: ", toString(extendA), "\n", sep = "")
      cat(" extendB: ", toString(extendB), "\n", sep = "")
      if(extendA && extendB){
        cat("Infinite line passing through A and B.\n")
      }else if(extendA){
        cat("Half-line with origin B and passing through A.\n")
      }else if(extendB){
        cat("Half-line with origin A and passing through B.\n")
      }else{
        cat("Segment joining A and B.\n")
      }
    },

    #' @description Direction (angle between 0 and 2pi)
    #' and offset (positive number) of the reference line.
    #' @details The equation of the line is
    #' \ifelse{html}{\out{cos(&theta;)x+sin(&theta;)y=d}}{\eqn{\cos(\theta)x+\sin(\theta)y=d}{cos(theta)x+sin(theta)y=d}}
    #' where \ifelse{html}{\out{&theta;}}{\eqn{\theta}{theta}} is the direction
    #' and \ifelse{html}{\out{d}}{\eqn{\d}{d}} is the offset.
    directionAndOffset = function(){
      A <- private[[".A"]]; B <- private[[".B"]]
      if(A[1L] == B[1L]){
        if(A[1L] > 0){
          list(direction = 0, offset = A[1L])
        }else{
          list(direction = pi, offset = -A[1L])
        }
      }else{
        x <- B[1L] - A[1L]
        y <- B[2L] - A[2L]
        # sgn <- sign(x)*sign(y)
        # intercept <-
        #   retistruct::line.line.intersection(A, B, c(0,0), c(0,1))[2L]
        theta <- -atan2(x, y) # if(y >= 0) atan2(y, x) else atan(y, x)
        offset <- A[1L]*cos(theta) + A[2L]*sin(theta)
        if(offset < 0){
          theta <- theta + pi
          offset <- -offset
        }
        # theta <- if(sgn >= 0){
        #   if(x < 0){
        #     atan2(y, x)
        #   }else{
        #     atan2(y, -x)
        #   }
        # }else{
        #   if(x < 0){
        #     atan2(y, -x)
        #   }else{
        #     atan2(y, x)
        #   }
        # }
        # if(intercept > 0){
        #   theta <- theta + pi
        # }else{
        #   theta <- theta + 2*pi
        # }
        list(direction = theta %% (2*pi), offset = offset)
      }
    },

    #' @description Check whether the reference line equals a given line,
    #' without taking into account \code{extendA} and \code{extendB}.
    #' @param line a \code{Line} object
    #' @return \code{TRUE} or \code{FALSE}.
    isEqual = function(line) {
      do1 <- as.numeric(self$directionAndOffset())
      do2 <- as.numeric(line$directionAndOffset())
      do1[1L] <- do1[1L] %% pi; do2[1L] <- do2[1L] %% pi
      isTRUE(all.equal(do1, do2))
    },

    #' @description Check whether the reference line is parallel to a given line.
    #' @param line a \code{Line} object
    #' @return \code{TRUE} or \code{FALSE}.
    isParallel = function(line) {
      P1 <- private[[".A"]]; P2 <- private[[".B"]]
      Q1 <- line$A; Q2 <- line$B
      dx1 <- P1[1L] - P2[1L]; dx2 <- Q1[1L] - Q2[1L]
      dy1 <- P1[2L] - P2[2L]; dy2 <- Q1[2L] - Q2[2L]
      abs(det(rbind(c(dx1, dy1), c(dx2, dy2)))) < sqrt(.Machine$double.eps)
    },

    #' @description Whether a point belongs to the reference line.
    #' @param M the point for which we want to test whether it belongs to the line
    #' @param strict logical, whether to take into account \code{extendA} and \code{extendB}
    #' @param checkCollinear logical, whether to check the collinearity of
    #' \code{A}, \code{B}, \code{M}; set to \code{FALSE} only if you are sure that
    #' \code{M} is on the line \code{(AB)} (if you use \code{strict=TRUE})
    #' @return \code{TRUE} or \code{FALSE}.
    #' @examples A <- c(0,0); B <- c(1,2); M <- c(3,6)
    #' l <- Line$new(A, B, FALSE, FALSE)
    #' l$includes(M, strict = TRUE)
    includes = function(M, strict = FALSE, checkCollinear = TRUE){
      A <- private[[".A"]]; B <- private[[".B"]]
      if(checkCollinear){
        test <- .collinear(A, B, M)
        if(!test) return(FALSE)
      }
      extendA <- private[[".extendA"]]; extendB <- private[[".extendB"]]
      if(!strict || (extendA && extendB)) return(.collinear(A, B, M))
      if(!extendA && !extendB){
        dotprod <- c(crossprod(A-M, B-M))
        if(dotprod <= 0){
          TRUE
        } else {
          message("The point is on the line (AB), but not on the segment [AB]")
          FALSE
        }
      }else if(extendA){
        if(any((M-B)*(A-B)>0)){ #(M-B)[1L] * (A-B)[1L] > 0){
          TRUE
        }else{
          message("The point is on the line (AB), but not on the half-line (AB]")
          FALSE
        }
      }else{ # extendB
        if(any((M-A)*(B-A)>0)){#(M-A)[1L] * (B-A)[1L] >= 0){
          TRUE
        }else{
          message("The point is on the line (AB), but not on the half-line [AB)")
          FALSE
        }
      }
    },

    #' @description Perpendicular line passing through a given point.
    #' @param M the point through which the perpendicular passes.
    #' @param extendH logical, whether to extend the perpendicular line
    #' beyond the meeting point
    #' @param extendM logical, whether to extend the perpendicular line
    #' beyond the point \code{M}
    #' @return A \code{Line} object; its two points are the
    #' meeting point and the point \code{M}.
    perpendicular = function(M, extendH = FALSE, extendM = TRUE) {
      A <- private[[".A"]]; B <- private[[".B"]]
      A_B <- B - A
      v <- c(-A_B[2L], A_B[1L])
      if(self$includes(M)){
        message("M is on the line")
        return(Line$new(M, M+v, TRUE, TRUE))
      }
      H <- .LineLineIntersection(A, B, M-v, M+v)
      Line$new(H, M, extendH, extendM)
    },

    #' @description Orthogonal projection of a point to the reference line.
    #' @param M a point
    #' @return A point.
    projection = function(M) {
      A <- private[[".A"]]; B <- private[[".B"]]
      A_B <- B - A
      v <- c(-A_B[2L], A_B[1L])
      .LineLineIntersection(A, B, M, M+v)
    },

    #' @description Reflection of a point with respect to the reference line.
    #' @param M a point
    #' @return A point.
    reflection = function(M){
      R <- Reflection$new(self)
      R$reflect(M)
    },

    #' @description Rotate the reference line.
    #' @param alpha angle of rotation
    #' @param O center of rotation
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return A \code{Line} object.
    rotate = function(alpha, O, degrees = TRUE){
      alpha <- as.vector(alpha)
      stopifnot(
        is.numeric(alpha),
        length(alpha) == 1L,
        !is.na(alpha)
      )
      O <- as.vector(O)
      stopifnot(
        is.numeric(O),
        length(O) == 2L,
        !any(is.na(O))
      )
      if(degrees){
        alpha <- alpha * pi/180
      }
      cosalpha <- cos(alpha); sinalpha <- sin(alpha)
      At <- private[[".A"]] - O
      RAt <- c(cosalpha*At[1L]-sinalpha*At[2L], sinalpha*At[1L]+cosalpha*At[2L])
      Bt <- private[[".B"]] - O
      RBt <- c(cosalpha*Bt[1L]-sinalpha*Bt[2L], sinalpha*Bt[1L]+cosalpha*Bt[2L])
      Line$new(RAt + O, RBt + O, private[[".extendA"]], private[[".extendB"]])
    },

    #' @description Translate the reference line.
    #' @param v the vector of translation
    #' @return A \code{Line} object.
    translate = function(v){
      v <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(v) == 2L,
        !any(is.na(v))
      )
      Line$new(private[[".A"]] + v, private[[".B"]] + v,
               private[[".extendA"]], private[[".extendB"]])
    },

    #' @description Invert the reference line.
    #' @param inversion an \code{Inversion} object
    #' @return A \code{Circle} object or a \code{Line} object.
    invert = function(inversion){
      inversion$invertLine(self)
    }

  )
)
