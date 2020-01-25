context("Shear")

test_that("Transform several points", {
  S <- Shear$new(c(1,1), c(1,3), 0.5, 30)
  Ms <- matrix(rpois(6, 10), 3L, 2L)
  Ps <- S$transform(Ms)
  expect_equal(Ps[1L,], S$transform(Ms[1L,]))
  expect_equal(Ps[2L,], S$transform(Ms[2L,]))
  expect_equal(Ps[3L,], S$transform(Ms[3L,]))
})
