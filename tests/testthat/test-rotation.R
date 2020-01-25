context("Rotation")

test_that("Transform several points", {
  R <- Rotation$new(30, c(1,3))
  Ms <- matrix(rpois(6, 10), 3L, 2L)
  Ps <- R$transform(Ms)
  expect_equal(Ps[1L,], R$transform(Ms[1L,]))
  expect_equal(Ps[2L,], R$transform(Ms[2L,]))
  expect_equal(Ps[3L,], R$transform(Ms[3L,]))
})
