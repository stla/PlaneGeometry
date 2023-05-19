library(rcdd)
library(CVXR)
library(PlaneGeometry)

maxVolumeInscribedCircle <- function(points, verbose = FALSE) {
  if(!is.matrix(points) || !is.numeric(points)){
    stop("The `points` argument must be a numeric matrix.", call. = TRUE)
  }
  if(ncol(points) != 2L){
    stop("The `points` matrix must have two columns.", call. = TRUE)
  }
  if(nrow(points) < 3L){
    stop("The `points` matrix must have at least three rows.", call. = TRUE)
  }
  if(anyNA(points)){
    stop("Points with missing values are not allowed.", call. = TRUE)
  }
  # linear inequalities
  V <- makeV(points)
  H <- scdd(V)[["output"]]
  A <- - H[, -c(1L, 2L)]
  b <- H[, 2L]
  # problem variables
  Rvar <- Variable(1L, nonneg = TRUE)
  Ovar <- Variable(2L)
  # objective
  objective <- Maximize(Rvar)
  #constraints
  constraints <- list()
  for(i in 1L:nrow(A)) {
    constraints <- append(
      constraints, list(sum(A[i, ]*Ovar) + Rvar*norm2(A[i, ]) <= b[i])
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
  R <- solution$getValue(Rvar)
  O <- c(solution$getValue(Ovar))
  # return circle
  circle <- Circle$new(O, R)
  attr(circle, "status") <- status
  circle
}

library(PlaneGeometry)
hexagon <- rbind(
  c(-1.7, -1),
  c(-1.4, 0.4),
  c(0.3, 1.3),
  c(1.7, 0.6),
  c(1.3, -0.3),
  c(-0.4, -1.8)
)
opar <- par(mar = c(2, 2, 1, 1))
plot(NULL, xlim=c(-2, 2), ylim=c(-2, 2), xlab = NA, ylab = NA, asp = 1)
points(hexagon, pch = 19)
polygon(hexagon)
circ <- maxVolumeInscribedCircle(hexagon)
draw(circ, col = "yellow2", border = "blue", lwd = 2)
par(opar)
# check optimization status:
attr(circ, "status")