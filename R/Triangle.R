#' @title R6 class representing a triangle
#'
#' @description A triangle has three vertices. They are named A, B, C.
#'
#' @export
#' @importFrom R6 R6Class
Triangle <- R6Class(

  "Triangle",

  private = list(
    .A = c(NA_real_, NA_real_),
    .B = c(NA_real_, NA_real_),
    .C = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field A get or set the vertex A
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

    #' @field B get or set the vertex B
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

    #' @field C get or set the vertex C
    C = function(value) {
      if (missing(value)) {
        private[[".C"]]
      } else {
        C <- as.vector(value)
        stopifnot(
          is.numeric(C),
          length(C) == 2L,
          !any(is.na(C))
        )
        private[[".C"]] <- C
      }
    }
  ),

  public = list(
    #' @description
    #' Create a new \code{Triangle} object.
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
        !any(is.na(A))
      )
      stopifnot(
        is.numeric(B),
        length(B) == 2L,
        !any(is.na(B))
      )
      stopifnot(
        is.numeric(C),
        length(C) == 2L,
        !any(is.na(C))
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

    #' @description Length of the side BC.
    a = function() {
      sqrt(c(crossprod(private[[".B"]]-private[[".C"]])))
    },

    #' @description Length of the side AC.
    b = function() {
      sqrt(c(crossprod(private[[".A"]]-private[[".C"]])))
    },

    #' @description Length of the side AB.
    c = function() {
      sqrt(c(crossprod(private[[".A"]]-private[[".B"]])))
    },

    #' @description The lengths of the sides of the triangle.
    #' @return A named numeric vector.
    edges = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      c(
        a = sqrt(c(crossprod(B-C))),
        b = sqrt(c(crossprod(A-C))),
        c = sqrt(c(crossprod(B-A)))
      )
    },

    #' @description Determines whether the triangle is acute.
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
      b <- sqrt(c(crossprod(AC)))
      c <- sqrt(c(crossprod(AB)))
      acos(c(crossprod(AC,AB)) / b / c)
    },

    #' @description Angle at the vertex B.
    #' @return The angle at the vertex B in radians.
    angleB = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      BC <- C-B; BA <- A-B
      a <- sqrt(c(crossprod(BC)))
      c <- sqrt(c(crossprod(BA)))
      acos(c(crossprod(BC,BA)) / a / c)
    },

    #' @description Angle at the vertex C.
    #' @return The angle at the vertex C in radians.
    angleC = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      CA <- A-C; CB <- B-C
      b <- sqrt(c(crossprod(CA)))
      a <- sqrt(c(crossprod(CB)))
      acos(c(crossprod(CA,CB)) / a / b)
    },

    #' @description The three angles of the triangle.
    #' @return A named vector containing the values of the angles in radians.
    angles = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      c(
        A = acos(c(crossprod(AC,AB)) / b / c),
        B = acos(c(crossprod(BC,BA)) / a / c),
        C = acos(c(crossprod(CA,CB)) / a / b)
      )
    },

    #' @description The X(175) triangle center.
    X175 = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      hangleA <- acos(c(crossprod(AC,AB)) / b / c) / 2
      BC <- C-B; BA <- A-B
      hangleB <- acos(c(crossprod(BC,BA)) / a / c) / 2
      CA <- A-C; CB <- B-C
      hangleC <- acos(c(crossprod(CA,CB)) / a / b) / 2
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
        a <- sqrt(c(crossprod(B-C)))
        b <- sqrt(c(crossprod(A-C)))
        c <- sqrt(c(crossprod(B-A)))
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

    #' @description Area of the triangle.
    area = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
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
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
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
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
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

    #' @description Nagel triangle of the triangle.
    #' @param NagelPoint logical, whether to return the Nagel point as attribute.
    #' @return A \code{Triangle} object.
    NagelTriangle = function(NagelPoint = FALSE) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- c(crossprod(AC,AB)) / b / c
      cosB <- c(crossprod(BC,BA)) / a / c
      cosC <- c(crossprod(CA,CB)) / a / b
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
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- c(crossprod(AC,AB)) / b / c
      cosB <- c(crossprod(BC,BA)) / a / c
      cosC <- c(crossprod(CA,CB)) / a / b
      tA <- a / (1 - cosA)
      tB <- b / (1 - cosB)
      tC <- c / (1 - cosC)
      (tA*A + tB*B + tC*C) / (tA + tB + tC)
    },

    #' @description Gergonne triangle of the triangle.
    #' @param GergonnePoint logical, whether to return the Gergonne point as an attribute
    #' @return A \code{Triangle} object.
    #' @details The Gergonne triangle is also known as the
    #' \emph{intouch triangle} or the \emph{contact triangle}.
    #' This is the triangle made of the three tangency points of the incircle.
    GergonneTriangle = function(GergonnePoint = FALSE) {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- c(crossprod(AC,AB)) / b / c
      cosB <- c(crossprod(BC,BA)) / a / c
      cosC <- c(crossprod(CA,CB)) / a / b
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

    #' @description Gergonne point of the triangle.
    GergonnePoint = function() {
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      AC <- C-A; AB <- B-A
      BC <- C-B; BA <- A-B
      CA <- A-C; CB <- B-C
      cosA <- c(crossprod(AC,AB)) / b / c
      cosB <- c(crossprod(BC,BA)) / a / c
      cosC <- c(crossprod(CA,CB)) / a / b
      tA <- a / (1 + cosA)
      tB <- b / (1 + cosB)
      tC <- c / (1 + cosC)
      (tA*A + tB*B + tC*C) / (tA + tB + tC)
    },

    #' @description Circumcircle of the triangle.
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
      Circle$new(center = center, radius = sqrt(c(crossprod(center-A))))
    },

    #' @description Malfatti circles of the triangle.
    #' @param tangencyPoints logical, whether to retourn the tangency points of
    #' the Malfatti circles as an attribute.
    #' @return A list with the three Malfatti circles, \code{Circle} objects.
    #' @examples t <- Triangle$new(c(0,0), c(2,0.5), c(1.5,2))
    #' Mcircles <- t$MalfattiCircles(TRUE)
    #' plot(0, 0, type="n", asp = 1, xlim = c(0,2.5), ylim = c(0,2.5),
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
      a <- sqrt(c(crossprod(B-C))) # distance BC
      b <- sqrt(c(crossprod(A-C))) # distance AC
      c <- sqrt(c(crossprod(B-A))) # distance AB
      p <- (a + b + c); s <- p / 2
      smina <- s - a; sminb <- s - b; sminc <- s - c
      areaABC <- sqrt(s*smina*sminb*sminc)
      I <- (A*a + B*b + C*c) / p # incenter
      r <- areaABC / s # inradius
      # radii of Malfatti circles ####
      IA <- sqrt(c(crossprod(I-A)))
      IB <- sqrt(c(crossprod(I-B)))
      IC <- sqrt(c(crossprod(I-C)))
      halfr <- r / 2
      sminr <- s - r
      r1 <- halfr * (sminr-(IB+IC-IA)) / smina
      r2 <- halfr * (sminr-(IC+IA-IB)) / sminb
      r3 <- halfr * (sminr-(IA+IB-IC)) / sminc
      # centers of Malfatti circles ####
      d1 <- r1 / tan(acos(c(crossprod(C-A,B-A)/b/c))/2)
      d2 <- r2 / tan(acos(c(crossprod(C-B,A-B)/a/c))/2)
      d3 <- r3 / tan(acos(c(crossprod(A-C,B-C)/b/a))/2)
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
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
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
      a <- sqrt(c(crossprod(B-C)))
      b <- sqrt(c(crossprod(A-C)))
      c <- sqrt(c(crossprod(B-A)))
      s <- (a + b + c) / 2;
      areaABC <- sqrt(s*(s-a)*(s-b)*(s-c))
      tc <- c(a,b,c) + 2 * areaABC / c(b+c-a, c+a-b, a+b-c) # triangular coordinates
      out <- (tc[1]*A + tc[2]*B + tc[3]*C) / sum(tc)
      if(detour){
        attr(out, "detour") <-
          sqrt(c(crossprod(A-out))) + sqrt(c(crossprod(B-out))) - c
      }
      out
    },

    #' @description Rotate the triangle.
    #' @param alpha angle of rotation
    #' @param O center of rotation
    #' @param degrees logical, whether `alpha` is given in degrees
    #' @return A \code{Triangle} object.
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
      O <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(M) == 2L,
        !any(is.na(v))
      )
      private[[".A"]] -> A; private[[".B"]] -> B; private[[".C"]] -> C
      Triangle$new(A + v, B + v, C + v)
    }

  )
)
