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
  f <- Affine$new(rbind(c(3.5,2),c(0,4)), c(-1, 1.25))
  ell1 <- f$transformEllipse(ell0)
  path0 <- ell0$path(3L)
  Q <- f$transform(path0[1L,])
  expect_true(ell1$includes(Q))
  Q <- f$transform(path0[2L,])
  expect_true(ell1$includes(Q))
  Q <- f$transform(path0[3L,])
  expect_true(ell1$includes(Q))
})

test_that("LownerJohnEllipse", {
  set.seed(666)
  pts <- cbind(rnorm(30, sd=2), rnorm(30))
  ell <- LownerJohnEllipse(pts)
  expect_true(all(apply(pts, 1L, ell$contains)))
})

test_that("EllipseFromFivePoints", {
  ell <- Ellipse$new(c(2,3), 5, 4, 50)
  set.seed(314)
  pts <- ell$randomPoints(5, "on")
  ell2 <- EllipseFromFivePoints(pts[1,],pts[2,],pts[3,],pts[4,],pts[5,])
  expect_true(ell$isEqual(ell2))
})

test_that("EllipseFromEquation", {
  ell <- Ellipse$new(c(4,3), 5, 1, 100)
  cf <- ell$equation()
  ell2 <- EllipseFromEquation(cf[1], cf[2], cf[3], cf[4], cf[5], cf[6])
  expect_true(ell$isEqual(ell2))
})
