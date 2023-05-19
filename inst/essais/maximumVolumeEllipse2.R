library(rcdd)
library(CVXR)

plg <- rbind(
  c(0, -2),
  c(-1, -1),
  c(-1, 1),
  c(0, 2),
  c(1, 1),
  c(1, -1)
)

Rot <- cbind(c(cos(pi/3), -sin(pi/3)), c(sin(pi/3), cos(pi/3))) 
plg <- t(Rot %*% t(plg))

plg <- rbind(
  c(-1.7, -1),
  c(-1.4, 0.4),
  c(0.3, 1.3),
  c(1.7, 0.6),
  c(1.3, -0.3),
  c(-0.4, -1.8)
)
plot(NULL, xlim=c(-2,2), ylim=c(-2,2))
polygon(plg)


V <- makeV(plg)
H <- scdd(V)$output
A <- - H[, -c(1L, 2L)]
b <- H[, 2L]


B <- Variable(2, 2)
d <- Variable(2)

objective <- Minimize(-log_det(B))

constraints <- list()
for(i in 1L:nrow(A)) {
  constraints <- append(
    constraints, list(norm2(B %*% A[i,]) + sum(A[i,]*d) <= b[i])
  )
}

# solve
program <- Problem(objective, constraints)
solution <- solve(program, solver = "SCS")#, FEASTOL = 1e-4, RELTOL = 1e-3, verbose = TRUE)

# get solutions
Bval <- solution$getValue(B)
dval <- c(solution$getValue(d))


plot(NULL, xlim=c(-2, 2), ylim=c(-2, 2), xlab = NA, ylab = NA, asp = 1)
points(plg, pch = 19)
polygon(plg)
theta_ <- seq(0, 2*pi, length.out = 100)
#lines(t(dval + Bval %*% rbind(cos(theta_), sin(theta_))))

library(PlaneGeometry)
aff <- Affine$new(Bval, dval)
crcl <- CircleOA(c(0,0), c(1,0))
ell <- aff$transformEllipse(crcl)
draw(ell, col = "yellow2", border = "blue", lwd = 2)
