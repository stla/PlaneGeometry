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

test_that("Affine$transformEllipse", {
  ell0 <- Ellipse$new(c(1,1), 5, 2, 30)
  path0 <- ell0$path()
  f <- Affine$new(rbind(c(3.5,2),c(0,4)), c(-1, 1.25))
  ell1 <- f$transformEllipse(ell0)
  Q <- f$transform(c(path0$x[70L], path0$y[70L]))
  expect_true(ell1$includes(Q))
  Q <- f$transform(c(path0$x[10L], path0$y[10L]))
  expect_true(ell1$includes(Q))
  Q <- f$transform(c(path0$x[40L], path0$y[40L]))
  expect_true(ell1$includes(Q))
})
