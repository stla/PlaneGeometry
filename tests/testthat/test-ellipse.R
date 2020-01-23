context("Ellipse")

test_that("Scaling$scaleCircle", {
  circ <- Circle$new(c(2,3), 4)
  P <- circ$pointFromAngle(45)
  S <- Scaling$new(c(1,2), direction = c(2,1), scale = 2)
  ell <- S$scaleCircle(circ)
  Q <- S$transform(P)
  x <- Q[1L]; y <- Q[2L]
  ABCDEF <- ell$equation()
  zero <- with(as.list(ABCDEF), A*x*x + B*x*y + C*y*y + D*x + E*y + F)
  expect_equal(0, zero)
})
