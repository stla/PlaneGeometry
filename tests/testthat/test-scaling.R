context("Scaling")

test_that("Scale circle", {
  S <- Scaling$new(c(1,1), c(2,3), 2)
  f <- S$asAffine()
  circ <- Circle$new(c(-1,2), 4)
  ell1 <- S$scaleCircle(circ)
  ell2 <- f$transformEllipse(circ)
  expect_true(ell1$isEqual(ell2))
})

