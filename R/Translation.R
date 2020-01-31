#' @title R6 class representing a translation
#'
#' @description A translation is given by a vector \code{v}.
#'
#' @export
#' @importFrom R6 R6Class
Translation <- R6Class(

  "Translation",

  private = list(
    .v = c(NA_real_, NA_real_)
  ),

  active = list(
    #' @field v get or set the vector of translation
    v = function(value) {
      if (missing(value)) {
        private[[".v"]]
      } else {
        v <- as.vector(value)
        stopifnot(
          is.numeric(v),
          length(v) == 2L,
          !any(is.na(v)),
          all(is.finite(v))
        )
        private[[".v"]] <- v
      }
    }
  ),

  public = list(
    #' @description Create a new \code{Translation} object.
    #' @param v a numeric vector of length two, the vector of translation
    #' @return A new \code{Translation} object.
    initialize = function(v) {
      v <- as.vector(v)
      stopifnot(
        is.numeric(v),
        length(v) == 2L,
        !any(is.na(v)),
        all(is.finite(v))
      )
      private[[".v"]] <- v
    },

    #' @description Show instance of a translation object.
    #' @param ... ignored
    print = function(...) {
      v <- private[[".v"]]
      cat("Translation by vector v.\n", sep = "")
      cat("       v: ", toString(v), "\n", sep = "")
    },

    #' @description Transform a point or several points by the reference
    #' translation.
    #' @param M a point or a two-column matrix of points, one point per row
    project = function(M) {
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
      out <- t(private[[".v"]] + t(M))
      if(nrow(out) == 1L) out <- c(out)
      out
    },

    #' @description An alias of \code{translate}.
    #' @param M a point or a two-column matrix of points, one point per row
    transform = function(M){
      self$translate(M)
    },

    #' @description Translate a line.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    translateLine = function(line){
      stopifnot(is(line, "Line"))
      v <- private[[".v"]]
      Line$new(line$A+v, line$B+v, line$extendA, line$extendB)
    },

    #' @description An alias of \code{translateLine}.
    #' @param line a \code{Line} object
    #' @return A \code{Line} object.
    transformLine = function(line){
      self$translateLine(line)
    },

    #' @description Translate a circle or an ellipse.
    #' @param ell an \code{Ellipse} object or a \code{Circle} object
    #' @return An \code{Ellipse} object or a \code{Circle} object.
    translateEllipse = function(ell){
      stopifnot(is(ell, "Ellipse") || is(ell, "Circle"))
      ellcopy <- ell$clone(deep = TRUE)
      ellcopy$center <- ellcopy$center + private[[".v"]]
      ellcopy
    },

    #' @description An alias of \code{translateEllipse}.
    #' @param ell an \code{Ellipse} object or a \code{Circle} object
    #' @return An \code{Ellipse} object or a \code{Circle} object.
    transformEllipse = function(ell){
      self$translateEllipse(ell)
    },

    #' @description Augmented matrix of the translation.
    #' @return A 3x3 matrix.
    getMatrix = function(){
      cbind(rbind(diag(2L),0), c(private[[".v"]],1))
    },

    #' @description Convert the reference translation to an \code{Affine}
    #' object.
    asAffine = function(){
      Affine$new(diag(2L), private[[".v"]])
    }

  )
)
