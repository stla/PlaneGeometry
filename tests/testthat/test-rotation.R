context("Rotation")

test_that("Transform several points", {
  R <- Rotation$new(30, c(1,3))
  Ms <- matrix(rpois(6, 10), 3L, 2L)
  Ps <- R$transform(Ms)
  expect_equal(Ps[1L,], R$transform(Ms[1L,]))
  expect_equal(Ps[2L,], R$transform(Ms[2L,]))
  expect_equal(Ps[3L,], R$transform(Ms[3L,]))
})

test_that("Rotate ellipse", {
  ell <- Ellipse$new(c(5,4), 3, 2, pi/6, FALSE)
  R <- Rotation$new(40, c(1,2))
  f <- R$asAffine()
  ell1 <- R$rotateEllipse(ell)
  ell2 <- f$transformEllipse(ell)
  expect_true(ell1$isEqual(ell2))
  #
  ell <- Ellipse$new(c(5,4), 3, 2, 30)
  R <- Rotation$new(pi/5, c(1,2), FALSE)
  f <- R$asAffine()
  ell1 <- R$rotateEllipse(ell)
  ell2 <- f$transformEllipse(ell)
  expect_true(ell1$isEqual(ell2))
})
