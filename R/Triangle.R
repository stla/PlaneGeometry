#' @title R6 class representing a triangle
#'
#' @description A triangle has three vertices. They are named A, B, C.
#'
#' @seealso \code{\link{TriangleThreeLines}} to define a triangle by three lines.
#'
#' @examples # incircle and excircles
#' A <- c(0,0); B <- c(1,2); C <- c(3.5,1)
#' t <- Triangle$new(A, B, C)
#' incircle <- t$incircle()
#' excircles <- t$excircles()
#' JA <- excircles$A$center
#' JB <- excircles$B$center
#' JC <- excircles$C$center
#' JAJBJC <- Triangle$new(JA, JB, JC)
#' A_JA <- Line$new(A, JA, FALSE, FALSE)
#' B_JB <- Line$new(B, JB, FALSE, FALSE)
#' C_JC <- Line$new(C, JC, FALSE, FALSE)
#' opar <- par(mar = c(0,0,0,0))
#' plot(NULL, asp = 1, xlim = c(0,6), ylim = c(-4,4),
#'      xlab = NA, ylab = NA, axes = FALSE)
#' draw(t, lwd = 2)
#' draw(incircle, border = "orange")
#' draw(excircles$A); draw(excircles$B); draw(excircles$C)
#' draw(JAJBJC, col = "blue")
#' draw(A_JA, col = "green")
#' draw(B_JB, col = "green")
#' draw(C_JC, col = "green")
#' par(opar)
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom uniformly runif_in_triangle runif_on_triangle
Triangle <- R6Class(

  "Triangle",

  private = list(
    .A = c(NA_real_, NA_real_),
    .B = c(NA_real_, NA_real_),
    .C = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field A get or set the vertex \code{A}
    A = function(value) {
      if (missing(value)) {
        private[[".A"]]
      } else {
        A <- as.vector(value)
        stopifnot(
          is.numeric(A),
          length(A) == 2L,
          !any(is.na(A)),
          all(is.finite(A))
        )
        private[[".A"]] <- A
      }
    },

    #' @field B get or set the vertex \code{B}
    B = function(value) {
      if (missing(value)) {
        private[[".B"]]
      } else {
        B <- as.vector(value)
        stopifnot(
          is.numeric(B),
          length(B) == 2L,
          !any(is.na(B)),
          all(is.finite(B))
        )
        private[[".B"]] <- B
      }
    },

    #' @field C get or set the vertex \code{C}
    C = function(value) {
      if (missing(value)) {
        private[[".C"]]
      } else {
        C <- as.vector(value)
        stopifnot(
          is.numeric(C),
          length(C) == 2L,
          !any(is.na(C)),
          all(is.finite(C))
        )
        private[[".C"]] <- C
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Triangle} object.
    #' @param A,B,C vertices
    #' @return A new \code{Triangle} object.
    #' @examples t <- Triangle$new(c(0,0), c(1,0), c(1,1))
    #' t
    #' t$C
    #' t$C <- c(2,2)
    #' t
    initialize = function(A, B, C) {
      A <- as.vector(A); B <- as.vector(B); C <- as.vector(C)
      stopifnot(
        is.numeric(A),
        length(A) == 2L,
        !any(is.na(A)),
        all(is.finite(A))
      )
      stopifnot(
        is.numeric(B),
        length(B) == 2L,
        !any(is.na(B)),
        all(is.finite(B))
      )
      stopifnot(
        is.numeric(C),
        length(C) == 2L,
        !any(is.na(C)),
        all(is.finite(C))
      )
      private[[".A"]] <- A
      private[[".B"]] <- B
      private[[".C"]] <- C
    },

    #' @description Show instance of a triangle object
    #' @param ... ignored
    #' @examples Triangle$new(c(0,0), c(1,0), c(1,1))
    print = function(...) {
      cat("Triangle: \n")
      cat("       A: ", toString(private[[".A"]]), "\n", sep = "")
      cat("       B: ", toString(private[[".B"]]), "\n", sep = "")
      cat("       C: ", toString(private[[".C"]]), "\n", sep = "")
      if((flatness <- self$flatness()) == 1){
        message("The triangle is flat.")
      }else if(flatness > 0.99){
        message(
          sprintf("The triangle is almost flat (flatness: %s).", flatness)
        )
      }
    },

    #' @description Flatness of the triangle.
    #' @return A number between 0 and 1. A triangle is flat when its flatness is 1.
    flatness = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      AB <- B-A; AC <- C-A
      z <- (AB[1] - 1i*AB[2]) * (AC[1] + 1i*AC[2])
      re <- Re(z); im <- Im(z)
      1 / (1 + im*im/re/re)
    },

    #' @description Length of the side \code{BC}.
    a = function() {
      .distance(private[[".B"]], private[[".C"]])
    },

    #' @description Length of the side \code{AC}.
    b = function() {
      .distance(private[[".A"]], private[[".C"]])
    },

    #' @description Length of the side \code{AB}.
    c = function() {
      .distance(private[[".A"]], private[[".B"]])
    },

    #' @description The lengths of the sides of the triangle.
    #' @return A named numeric vector.
    edges = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      c(
        a = .distance(B,C),
        b = .distance(A,C),
        c = .distance(A,B)
      )
    },

    #' @description Determine the orientation of the triangle.
    #' @return An integer: 1 for counterclockwise, -1 for clockwise, 0 for collinear.
    orientation = function(){
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      val <-
        (B[2L] - A[2L])*(C[1L] - B[1L]) - (B[1L] - A[1L])*(C[2L] - B[2L])
      ifelse(val == 0, 0L, ifelse(val > 0, -1L, 1L))
    },

    #' @description Determine whether a point lies inside the reference triangle.
    #' @param M a point
    contains = function(M){
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      dsArea <- -B[2L]*C[1L] + A[2L]*(-B[1L] + C[1L]) + A[1L]*(B[2L] - C[2L]) +
        B[1L]*C[2L]
      s <- (A[2L]*C[1L] - A[1L]*C[2L] + (C[2L] - A[2L])*M[1L] +
              (A[1L] - C[1L])*M[2L]) / dsArea
      t <- (A[1L]*B[2L] - A[2L]*B[1L] + (A[2L] - B[2L])*M[1L] +
              (B[1L] - A[1L])*M[2L]) / dsArea
      s > 0 && t > 0 && 1-s-t > 0
    },

    #' @description Determines whether the reference triangle is acute.
    #' @return `TRUE` if the triangle is acute (or right), `FALSE` otherwise.
    isAcute = function() {
      edges <- self$edges()
      i <- match(max(edges), edges)
      sum(edges[-i]^2) >= edges[[i]]^2
    },

    #' @description Angle at the vertex A.
    #' @return The angle at the vertex A in radians.
    angleA = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      AC <- C-A; AB <- B-A
      b <- .vlength(AC)
      c <- .vlength(AB)
      acos(.dot(AC,AB) / b / c)
    },

    #' @description Angle at the vertex B.
    #' @return The angle at the vertex B in radians.
    angleB = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      BC <- C-B; BA <- A-B
      a <- .vlength(BC)
      c <- .vlength(BA)
      acos(.dot(BC,BA) / a / c)
    },

    #' @description Angle at the vertex C.
    #' @return The angle at the vertex C in radians.
    angleC = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      CA <- A-C; CB <- B-C
      b <- .vlength(CA)
      a <- .vlength(CB)
      acos(.dot(CA,CB) / a / b)
    },

    #' @description The three angles of the triangle.
    #' @return A named vector containing the values of the angles in radians.
    angles = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      c(
        A = acos(.dot(AC,AB) / b / c),
        B = acos(.dot(BC,BA) / a / c),
        C = acos(.dot(CA,CB) / a / b)
      )
    },

    #' @description The X(175) triangle center.
    X175 = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      hangleA <- acos(.dot(AC,AB) / b / c) / 2
      BC <- C-B; BA <- A-B
      hangleB <- acos(.dot(BC,BA) / a / c) / 2
      CA <- A-C; CB <- B-C
      hangleC <- acos(.dot(CA,CB) / a / b) / 2
      x <- cos(hangleB)*cos(hangleC)/cos(hangleA) - 1
      y <- cos(hangleC)*cos(hangleA)/cos(hangleB) - 1
      z <- cos(hangleA)*cos(hangleB)/cos(hangleC) - 1
      (a*x*A + b*y*B + c*z*C) / (a*x + b*y + c*z)
    },

    #' @description Isoperimetric point in the sense of Veldkamp.
    #' @return The isoperimetric point in the sense of Veldkamp, if it exists.
    #' Otherwise, returns `NULL`.
    VeldkampIsoperimetricPoint = function() {
      if(!self$isAcute()){
        private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
        a <- .distance(B,C)
        b <- .distance(A,C)
        c <- .distance(B,A)
        s <- (a + b + c) / 2
        areaABC <- sqrt(s*(s-a)*(s-b)*(s-c))
        r <- areaABC / s # inradius
        R <- a*b*c / sqrt((a+b+c)*(b+c-a)*(c+a-b)*(a+b-c)) # circumradius
        if(s <= 2*R + r/2){ # always TRUE for an acute triangle
          warning("The Veldkamp isoperimetric point does not exist.")
          return(NULL)
        }
      }
      self$X175()
    },

    #' @description Centroid.
    centroid = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      (A + B + C) / 3
    },

    #' @description Orthocenter.
    orthocenter = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      x <- 1 / (.dot(AC,AB) / b / c)
      y <- 1 / (.dot(BC,BA) / a / c)
      z <- 1 / (.dot(CA,CB) / a / b)
      den <- a*x + b*y + c*z
      (a*x*A + b*y*B + c*z*C) / den
    },

    #' @description Area of the triangle.
    area = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      s <- (a + b + c) / 2
      sqrt(s*(s-a)*(s-b)*(s-c))
    },

    #' @description Incircle of the triangle.
    #' @return A \code{Circle} object.
    incircle = function() {
      if(self$flatness() == 1){
        warning("The triangle is flat.")
      }
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      p <- (a + b + c); s <- p / 2;
      areaABC <- sqrt(s*(s-a)*(s-b)*(s-c))
      Circle$new(
        center = (A*a + B*b + C*c) / p,
        radius = areaABC / s
      )
    },

    #' @description Excircles of the triangle.
    #' @return A list with the three excircles, \code{Circle} objects.
    excircles = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      s <- (a + b + c) / 2
      JA <- (-a*A + b*B + c*C) / (-a + b + c)
      JB <- (a*A - b*B + c*C) / (a - b + c)
      JC <- (a*A + b*B - c*C) / (a + b - c)
      rA <- sqrt(s*(s-b)*(s-c)/(s-a))
      rB <- sqrt(s*(s-a)*(s-c)/(s-b))
      rC <- sqrt(s*(s-a)*(s-b)/(s-c))
      list(
        A = Circle$new(center = JA, radius = rA),
        B = Circle$new(center = JB, radius = rB),
        C = Circle$new(center = JC, radius = rC)
      )
    },

    #' @description Excentral triangle of the reference triangle.
    #' @return A \code{Triangle} object.
    excentralTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      JA <- (-a*A + b*B + c*C) / (-a + b + c)
      JB <- (a*A - b*B + c*C) / (a - b + c)
      JC <- (a*A + b*B - c*C) / (a + b - c)
      Triangle$new(JA, JB, JC)
    },

    #' @description Bevan point. This is the circumcenter of the
    #' excentral triangle.
    BevanPoint = function(){
      self$excentralTriangle()$circumcenter()
    },

    #' @description Medial triangle. Its vertices are the mid-points of the
    #' sides of the reference triangle.
    medialTriangle = function(){
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      Triangle$new((B+C)/2, (A+C)/2, (A+B)/2)
    },

    #' @description Orthic triangle. Its vertices are the feet of the altitudes
    #' of the reference triangle.
    orthicTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      x <- 1 / (.dot(AC,AB) / b / c)
      y <- 1 / (.dot(BC,BA) / a / c)
      z <- 1 / (.dot(CA,CB) / a / b)
      # HA <- (b*y*B + c*z*C) / (b*y + c*z)
      # HB <- (a*x*A + c*z*C) / (a*x + c*z)
      # HC <- (a*x*A + b*y*B) / (a*x + b*y)
      HA <- (b/z*B + c/y*C) / (b/z + c/y)
      HB <- (a/z*A + c/x*C) / (a/z + c/x)
      HC <- (a/y*A + b/x*B) / (a/y + b/x)
      Triangle$new(HA, HB, HC)
    },

    #' @description Incentral triangle.
    #' @return A \code{Triangle} object.
    #' @details It is the triangle whose vertices are the intersections of the
    #' reference triangle's angle bisectors with the respective opposite sides.
    incentralTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      Triangle$new(
        (b*B + c*C) / (b + c),
        (a*A + c*C) / (a + c),
        (a*A + b*B) / (a + b)
      )
    },

    #' @description Nagel triangle (or extouch triangle) of the reference triangle.
    #' @param NagelPoint logical, whether to return the Nagel point as attribute
    #' @return A \code{Triangle} object.
    #' @examples t <- Triangle$new(c(0,-2), c(0.5,1), c(3,0.6))
    #' lineAB <- Line$new(t$A, t$B)
    #' lineAC <- Line$new(t$A, t$C)
    #' lineBC <- Line$new(t$B, t$C)
    #' NagelTriangle <- t$NagelTriangle(NagelPoint = TRUE)
    #' NagelPoint <- attr(NagelTriangle, "Nagel point")
    #' excircles <- t$excircles()
    #' opar <- par(mar = c(0,0,0,0))
    #' plot(0, 0, type="n", asp = 1, xlim = c(-1,5), ylim = c(-3,3),
    #'      xlab = NA, ylab = NA, axes = FALSE)
    #' draw(t, lwd = 2)
    #' draw(lineAB); draw(lineAC); draw(lineBC)
    #' draw(excircles$A, border = "orange")
    #' draw(excircles$B, border = "orange")
    #' draw(excircles$C, border = "orange")
    #' draw(NagelTriangle, lwd = 2, col = "red")
    #' draw(Line$new(t$A, NagelTriangle$A, FALSE, FALSE), col = "blue")
    #' draw(Line$new(t$B, NagelTriangle$B, FALSE, FALSE), col = "blue")
    #' draw(Line$new(t$C, NagelTriangle$C, FALSE, FALSE), col = "blue")
    #' points(rbind(NagelPoint), pch = 19)
    #' par(opar)
    NagelTriangle = function(NagelPoint = FALSE) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      tA <- a / (1 - cosA)
      tB <- b / (1 - cosB)
      tC <- c / (1 - cosC)
      TA <- (tB*B + tC*C) / (tB + tC)
      TB <- (tA*A + tC*C) / (tA + tC)
      TC <- (tA*A + tB*B) / (tA + tB)
      out <- Triangle$new(TA, TB, TC)
      if(NagelPoint){
        attr(out, "Nagel point") <- (tA*A + tB*B + tC*C) / (tA + tB + tC)
      }
      out
    },

    #' @description Nagel point of the triangle.
    NagelPoint = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      tA <- a / (1 - cosA)
      tB <- b / (1 - cosB)
      tC <- c / (1 - cosC)
      (tA*A + tB*B + tC*C) / (tA + tB + tC)
    },

    #' @description Gergonne triangle of the reference triangle.
    #' @param GergonnePoint logical, whether to return the Gergonne point as an attribute
    #' @return A \code{Triangle} object.
    #' @details The Gergonne triangle is also known as the
    #' \emph{intouch triangle} or the \emph{contact triangle}.
    #' This is the triangle made of the three tangency points of the incircle.
    GergonneTriangle = function(GergonnePoint = FALSE) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      tA <- a / (1 + cosA)
      tB <- b / (1 + cosB)
      tC <- c / (1 + cosC)
      TA <- (tB*B + tC*C) / (tB + tC)
      TB <- (tA*A + tC*C) / (tA + tC)
      TC <- (tA*A + tB*B) / (tA + tB)
      out <- Triangle$new(TA, TB, TC)
      if(GergonnePoint){
        attr(out, "Gergonne point") <- (tA*A + tB*B + tC*C) / (tA + tB + tC)
      }
      out
    },

    #' @description Gergonne point of the reference triangle.
    GergonnePoint = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      tA <- a / (1 + cosA)
      tB <- b / (1 + cosB)
      tC <- c / (1 + cosC)
      (tA*A + tB*B + tC*C) / (tA + tB + tC)
    },

    #' @description Tangential triangle of the reference triangle.
    #' This is the triangle formed by the lines tangent to the circumcircle of
    #' the reference triangle at its vertices. It does not exist for a
    #' right triangle.
    #' @return A \code{Triangle} object.
    tangentialTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      i <- match(max(a2,b2,c2), c(a2,b2,c2))
      if(sum(c(a2,b2,c2)[-i]) == c(a2,b2,c2)[i]){
        stop("The triangle is right, no tangential triangle.")
      }
      TA <- (-a2*A + b2*B + c2*C) / (-a2 + b2 + c2)
      TB <- (a2*A - b2*B + c2*C) / (a2 - b2 + c2)
      TC <- (a2*A + b2*B - c2*C) / (a2 + b2 - c2)
      Triangle$new(TA, TB, TC)
    },

    #' @description Symmedial triangle of the reference triangle.
    #' @return A \code{Triangle} object.
    #' @examples t <- Triangle$new(c(0,-2), c(0.5,1), c(3,0.6))
    #' symt <- t$symmedialTriangle()
    #' symmedianA <- Line$new(t$A, symt$A, FALSE, FALSE)
    #' symmedianB <- Line$new(t$B, symt$B, FALSE, FALSE)
    #' symmedianC <- Line$new(t$C, symt$C, FALSE, FALSE)
    #' K <- t$symmedianPoint()
    #' opar <- par(mar = c(0,0,0,0))
    #' plot(NULL, asp = 1, xlim = c(-1,5), ylim = c(-3,3),
    #'      xlab = NA, ylab = NA, axes = FALSE)
    #' draw(t, lwd = 2)
    #' draw(symmedianA, lwd = 2, col = "blue")
    #' draw(symmedianB, lwd = 2, col = "blue")
    #' draw(symmedianC, lwd = 2, col = "blue")
    #' points(rbind(K), pch = 19, col = "red")
    #' par(opar)
    symmedialTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      KA <- (b2*B + c2*C) / (b2 + c2)
      KB <- (a2*A + c2*C) / (a2 + c2)
      KC <- (a2*A + b2*B) / (a2 + b2)
      Triangle$new(KA, KB, KC)
    },

    #' @description Symmedian point of the reference triangle.
    #' @return A point.
    symmedianPoint = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      (a2*A + b2*B + c2*C) / (a2 + b2 + c2)
    },

    #' @description Circumcircle of the reference triangle.
    #' @return A \code{Circle} object.
    circumcircle = function() {
      if(self$flatness() == 1){
        warning("The triangle is flat.")
        return(NULL)
      }
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      q <- c(crossprod(A), crossprod(B), crossprod(C))
      ABC <- rbind(A, B, C)
      Dx <- det(cbind(q, ABC[,2L], 1))
      Dy <- -det(cbind(q, ABC[,1L], 1))
      center <- c(Dx,Dy) / det(cbind(ABC, 1)) / 2
      Circle$new(center = center, radius = .distance(center,A))
    },

    #' @description Circumcenter of the reference triangle.
    circumcenter = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      (a*cosA*A + b*cosB*B + c*cosC*C) / (a*cosA + b*cosB + c*cosC)
    },

    #' @description Circumradius of the reference triangle.
    circumradius = function(){
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      a*b*c / sqrt((a+b+c)*(b+c-a)*(c+a-b)*(a+b-c))
    },

    #' @description The Brocard circle of the reference triangle (also known
    #' as the seven-point circle).
    #' @return A \code{Circle} object.
    BrocardCircle = function() {
      O <- self$circumcenter()
      K <- self$symmedianPoint()
      CircleAB(O, K)
    },

    #' @description Brocard points of the reference triangle.
    #' @return A list of two points, the first Brocard point and the second
    #' Brocard point.
    BrocardPoints = function(){
      if(self$flatness() == 1){
        warning("The triangle is flat.")
        return(NULL)
      }
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      Omega <- self$trilinearToPoint(c/b, a/c, b/a)
      OmegaPrime <- self$trilinearToPoint(b/c, c/a, a/b)
      if(self$orientation() == 1L){
        list(Z1 = Omega, Z2 = OmegaPrime)
      }else{
        list(Z1 = OmegaPrime, Z2 = Omega)
      }
    },

    #' @description The first Lemoine circle of the reference triangle.
    #' @return A \code{Circle} object.
    LemoineCircleI = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      a <- sqrt(a2)
      b <- sqrt(b2)
      c <- sqrt(c2)
      R <- a*b*c*sqrt(a2*b2 + b2*c2 + c2*a2) / (a2 + b2 + c2) /
        sqrt((-a+b+c)*(a-b+c)*(a+b-c)*(a+b+c))
      O <- self$circumcenter()
      K <- self$symmedianPoint()
      Circle$new((O+K)/2, R)
    },

    #' @description The second Lemoine circle of the reference triangle (also
    #' known as the cosine circle)
    #' @return A \code{Circle} object.
    LemoineCircleII = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      R <- sqrt(a2*b2*c2) / (a2 + b2 + c2)
      K <- self$symmedianPoint()
      Circle$new(K, R)
    },

    #' @description The Lemoine triangle of the reference triangle.
    #' @return A \code{Triangle} object.
    LemoineTriangle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      abc <- sqrt(a2*b2*c2)
      # t1 <- sqrt(b2*c2) / (a2-2*b2-2*c2)
      # t2 <- sqrt(a2*c2) / (-2*a2+b2-2*c2)
      # t3 <- sqrt(a2*b2) / (-2*a2-2*b2+c2)
      t1 <- abc / (a2-2*b2-2*c2)
      t2 <- abc / (-2*a2+b2-2*c2)
      t3 <- abc / (-2*a2-2*b2+c2)
      P1 <- (t2*B + t3*C) / (t2+t3)
      P2 <- (t1*A + t3*C) / (t1+t3)
      P3 <- (t1*A + t2*B) / (t1+t2)
      Triangle$new(P1, P2, P3)
    },

    #' @description The third Lemoine circle of the reference triangle.
    #' @return A \code{Circle} object.
    LemoineCircleIII = function() {
      self$LemoineTriangle()$circumcircle()
    },

    #' @description Parry circle of the reference triangle.
    #' @return A \code{Circle} object.
    ParryCircle = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a2 <- c(crossprod(B-C))
      b2 <- c(crossprod(A-C))
      c2 <- c(crossprod(B-A))
      t1 <- a2*(b2-c2)*(b2+c2-2*a2)
      t2 <- b2*(c2-a2)*(c2+a2-2*b2)
      t3 <- c2*(a2-b2)*(a2+b2-2*c2)
      O <- (t1*A+t2*B+t3*C) / (t1+t2+t3)
      R <- sqrt(a2*b2*c2)*((a2*a2+b2*b2+c2*c2) - (a2*b2+b2*c2+a2*c2)) / 3 /
        abs((a2-b2)*(b2-c2)*(c2-a2)) # quid si isocele ?
      Circle$new(O, R)
    },

    #' @description Pedal triangle of a point with respect to the reference
    #' triangle. The pedal triangle of a point \code{P} is the triangle whose
    #' vertices are the feet of the perpendiculars from \code{P} to the sides
    #' of the reference triangle.
    #' @param P a point
    #' @return A \code{Triangle} object.
    pedalTriangle = function(P){
      # P <- as.vector(P)
      # stopifnot(
      #   is.numeric(P),
      #   length(P) == 2L,
      #   !any(is.na(P)),
      #   all(is.finite(P))
      # )
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- .dot(AC,AB) / b / c
      cosB <- .dot(BC,BA) / a / c
      cosC <- .dot(CA,CB) / a / b
      Ptc <- self$pointToTrilinear(P)
      Q1 <- (b*(Ptc[2L] + Ptc[1L]*cosC)*B + c*(Ptc[3L] + Ptc[1L]*cosB)*C) /
        (b*(Ptc[2L] + Ptc[1L]*cosC) + c*(Ptc[3L] + Ptc[1L]*cosB))
      Q2 <- (a*(Ptc[1L] + Ptc[2L]*cosC)*A + c*(Ptc[3L] + Ptc[2L]*cosA)*C) /
        (a*(Ptc[1L] + Ptc[2L]*cosC) + c*(Ptc[3L] + Ptc[2L]*cosA))
      Q3 <- (a*(Ptc[1L] + Ptc[3L]*cosB)*A + b*(Ptc[2L] + Ptc[3L]*cosA)*B) /
        (a*(Ptc[1L] + Ptc[3L]*cosB) + b*(Ptc[2L] + Ptc[3L]*cosA))
      Triangle$new(Q1, Q2, Q3)
    },

    #' @description Cevian triangle of a point with respect to the reference
    #' triangle.
    #' @param P a point
    #' @return A \code{Triangle} object.
    CevianTriangle = function(P){
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      Ptc <- self$pointToTrilinear(P)
      Q1 <- (b*Ptc[2L]*B + c*Ptc[3L]*C) / (b*Ptc[2L] + c*Ptc[3L])
      Q2 <- (a*Ptc[1L]*A + c*Ptc[3L]*C) / (a*Ptc[1L] + c*Ptc[3L])
      Q3 <- (a*Ptc[1L]*A + b*Ptc[2L]*B) / (a*Ptc[1L] + b*Ptc[2L])
      Triangle$new(Q1, Q2, Q3)
    },

    #' @description Malfatti circles of the triangle.
    #' @param tangencyPoints logical, whether to retourn the tangency points of
    #' the Malfatti circles as an attribute.
    #' @return A list with the three Malfatti circles, \code{Circle} objects.
    #' @examples t <- Triangle$new(c(0,0), c(2,0.5), c(1.5,2))
    #' Mcircles <- t$MalfattiCircles(TRUE)
    #' plot(NULL, asp = 1, xlim = c(0,2.5), ylim = c(0,2.5),
    #'      xlab = NA, ylab = NA)
    #' grid()
    #' draw(t, col = "blue", lwd = 2)
    #' invisible(lapply(Mcircles, draw, col = "green", border = "red"))
    #' invisible(lapply(attr(Mcircles, "tangencyPoints"), function(P){
    #'   points(P[1], P[2], pch = 19)
    #' }))
    MalfattiCircles = function(tangencyPoints = FALSE) {
      if(self$flatness() == 1){
        warning("The triangle is flat.")
        return(NULL)
      }
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      p <- (a + b + c); s <- p / 2
      smina <- s - a; sminb <- s - b; sminc <- s - c
      areaABC <- sqrt(s*smina*sminb*sminc)
      I <- (A*a + B*b + C*c) / p # incenter
      r <- areaABC / s # inradius
      # radii of Malfatti circles ####
      IA <- .vlength(I-A)
      IB <- .vlength(I-B)
      IC <- .vlength(I-C)
      halfr <- r / 2
      sminr <- s - r
      r1 <- halfr * (sminr-(IB+IC-IA)) / smina
      r2 <- halfr * (sminr-(IC+IA-IB)) / sminb
      r3 <- halfr * (sminr-(IA+IB-IC)) / sminc
      # centers of Malfatti circles ####
      d1 <- r1 / tan(acos(.dot(C-A,B-A)/b/c)/2)
      d2 <- r2 / tan(acos(.dot(C-B,A-B)/a/c)/2)
      d3 <- r3 / tan(acos(.dot(A-C,B-C)/b/a)/2)
      w <- d1 + d2 + 2*sqrt(r1*r2)
      u <- d2 + d3 + 2*sqrt(r2*r3)
      v <- d3 + d1 + 2*sqrt(r3*r1)
      d <- sqrt((-u+v+w)*(u+v-w)*(u-v+w)*(u+v+w)) / 2
      x <- d/r1 - (v+w); y <- u; z <- u # trilinear coordinates
      O1 <- (u*x*A + v*y*B + w*z*C) / (u*x + v*y + w*z)
      x <- v; y <- d/r2 - (u+w); z <- v # trilinear coordinates
      O2 <- (u*x*A + v*y*B + w*z*C) / (u*x + v*y + w*z)
      x <- w; y <- w; z <- d/r3 - (u+v) # trilinear coordinates
      O3 <- (u*x*A + v*y*B + w*z*C) / (u*x + v*y + w*z)
      out <- list(
        cA = Circle$new(center = O1, radius = r1),
        cB = Circle$new(center = O2, radius = r2),
        cC = Circle$new(center = O3, radius = r3)
      )
      if(tangencyPoints){
        O2_O3 <- O3 - O2
        O1_O3 <- O3 - O1
        O1_O2 <- O2 - O1
        attr(out, "tangencyPoints") <- list(
          TA = O2 + r2/sqrt(c(crossprod(O2_O3))) * O2_O3,
          TB = O1 + r1/sqrt(c(crossprod(O1_O3))) * O1_O3,
          TC = O1 + r1/sqrt(c(crossprod(O1_O2))) * O1_O2
        )
      }
      out
    },

    #' @description First Ajima-Malfatti point of the triangle.
    AjimaMalfatti1 = function() {
      private[[".A"]] -> A; private[[".B"]] -> B
      Mcircles <- self$MalfattiCircles(tangencyPoints = TRUE)
      Ts <- attr(Mcircles, "tangencyPoints")
      .LineLineIntersection(A, Ts$TA, B, Ts$TB)
    },

    #' @description Second Ajima-Malfatti point of the triangle.
    AjimaMalfatti2 = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      JA <- (-a*A + b*B + c*C) / (-a + b + c)
      JB <- (a*A - b*B + c*C) / (a - b + c)
      Mcircles <- self$MalfattiCircles(tangencyPoints = TRUE)
      Ts <- attr(Mcircles, "tangencyPoints")
      .LineLineIntersection(JA, Ts$TA, JB, Ts$TB)
    },

    #' @description Equal detour point of the triangle.
    #' @param detour logical, whether to return the detour as an attribute.
    #' @details Also known as the X(176) triangle center.
    equalDetourPoint = function(detour=FALSE) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      s <- (a + b + c) / 2;
      areaABC <- sqrt(s*(s-a)*(s-b)*(s-c))
      tc <- c(a,b,c) + 2 * areaABC / c(b+c-a, c+a-b, a+b-c) # triangular coordinates
      out <- (tc[1]*A + tc[2]*B + tc[3]*C) / sum(tc)
      if(detour){
        attr(out, "detour") <- .vlength(A-out) + .vlength(B-out) - c
      }
      out
    },

    #' @description Point given by trilinear coordinates.
    #' @param x,y,z trilinear coordinates
    #' @return The point with trilinear coordinates \code{x:y:z} with respect to
    #' the reference triangle.
    #' @examples t <- Triangle$new(c(0,0), c(2,1), c(5,7))
    #' incircle <- t$incircle()
    #' t$trilinearToPoint(1, 1, 1)
    #' incircle$center
    trilinearToPoint = function(x, y, z) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      den <- a*x + b*y + c*z
      (a*x*A + b*y*B + c*z*C) / den
    },

    #' @description Give the trilinear coordinates of a point with respect to
    #' the reference triangle.
    #' @param P a point
    #' @return The trilinear coordinates, a numeric vector of length 3.
    pointToTrilinear = function(P){
      P <- as.vector(P)
      stopifnot(
        is.numeric(P),
        length(P) == 2L,
        !any(is.na(P)),
        all(is.finite(P))
      )
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- .distance(B,C)
      b <- .distance(A,C)
      c <- .distance(B,A)
      Mat <- solve(rbind(cbind(A-C,B-C,C),c(0,0,1)))
      k1k2 <- Mat %*% c(P,1)
      k1 <- k1k2[1L]
      k2 <- k1k2[2L]
      k3 <- 1-k1-k2
      c(x = k1/a, y = k2/b, z = k3/c)
    },

    #' @description Rotate the triangle.
    #' @param alpha angle of rotation
    #' @param O center of rotation
    #' @param degrees logical, whether \code{alpha} is given in degrees
    #' @return A \code{Triangle} object.
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
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      At <- A - O; Bt <- B - O; Ct <- C - O
      RAt <- c(cosalpha*At[1L]-sinalpha*At[2L], sinalpha*At[1L]+cosalpha*At[2L])
      RBt <- c(cosalpha*Bt[1L]-sinalpha*Bt[2L], sinalpha*Bt[1L]+cosalpha*Bt[2L])
      RCt <- c(cosalpha*Ct[1L]-sinalpha*Ct[2L], sinalpha*Ct[1L]+cosalpha*Ct[2L])
      Triangle$new(RAt + O, RBt + O, RCt + O)
    },

    #' @description Translate the triangle.
    #' @param v the vector of translation
    #' @return A \code{Triangle} object.
    translate = function(v){
      v <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(v) == 2L,
        !any(is.na(v)),
        all(is.finite(v))
      )
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      Triangle$new(A + v, B + v, C + v)
    },

    #' @description The Steiner ellipse (or circumellipse) of the reference
    #' triangle. This is the ellipse passing through the three vertices of
    #' the triangle and centered at the centroid of the triangle.
    #' @return An \code{Ellipse} object.
    #' @examples t <- Triangle$new(c(0,0), c(2,0.5), c(1.5,2))
    #' ell <- t$SteinerEllipse()
    #' plot(NULL, asp = 1, xlim = c(0,2.5), ylim = c(-0.7,2.4),
    #'      xlab = NA, ylab = NA)
    #' draw(t, col = "blue", lwd = 2)
    #' draw(ell, border = "red", lwd =2)
    SteinerEllipse = function(){
      if(self$flatness() == 1){
        warning("The triangle is flat.")
        return(NULL)
      }
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      P <- c(0,0); Q <- c(10,0); R <- c(5, 5*sqrt(3))
      circ <- Triangle$new(P, Q, R)$circumcircle()
      f <- AffineMappingThreePoints(P, Q, R, A, B, C)
      f$transformEllipse(circ)
    },

    #' @description Random points on or in the reference triangle.
    #' @param n an integer, the desired number of points
    #' @param where \code{"in"} to generate inside the triangle,
    #' \code{"on"} to generate on the triangle
    #' @return The generated points in a two columns matrix with \code{n} rows.
    randomPoints = function(n, where = "in"){
      where <- match.arg(where, c("in", "on"))
      if(where == "in"){
        runif_in_triangle(n, private[[".A"]], private[[".B"]],
                                     private[[".C"]])
      }else{
        runif_on_triangle(n, private[[".A"]], private[[".B"]],
                                     private[[".C"]])
      }
    }

  )
)


#' Triangle defined by three lines
#' @description Return the triangle formed by three lines.
#'
#' @param line1,line2,line3 \code{Line} objects
#'
#' @return A \code{Triangle} object.
#' @export
TriangleThreeLines <- function(line1, line2, line3){
  if(line1$isEqual(line2)) stop("`line1` equals `line2`.")
  if(line1$isEqual(line3)) stop("`line1` equals `line3`.")
  if(line3$isEqual(line2)) stop("`line2` equals `line3`.")
  A <- .LineLineIntersection(line2$A, line2$B, line3$A, line3$B)
  if(A[1L] == Inf) stop("`line2` is parallel to `line3`.")
  B <- .LineLineIntersection(line1$A, line1$B, line3$A, line3$B)
  if(B[1L] == Inf) stop("`line1` is parallel to `line3`.")
  C <- .LineLineIntersection(line1$A, line1$B, line2$A, line2$B)
  if(C[1L] == Inf) stop("`line1` is parallel to `line2`.")
  Triangle$new(A, B, C)
}
