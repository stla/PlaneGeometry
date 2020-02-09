context("Inversions")

test_that("invertCircle", {
  circ0 <- Circle$new(c(0,2), 3)
  iota <- Inversion$new(c(5,5), 6)
  circ1 <- iota$invertCircle(circ0)
  A <- iota$invert(c(3,2))
  B <- iota$invert(c(0,5))
  C <- iota$invert(c(-3,2))
  expect_true(circ1$includes(A) && circ1$includes(B) && circ1$includes(C))
})

test_that("inversionSwappingTwoCircles", {
  ## non-intersecting circles, external
  circ1 <- Circle$new(c(0,2), 3)
  circ2 <- Circle$new(c(4,5), 1)
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  ## non-intersecting circles, internal
  circ1 <- Circle$new(c(0,0), 3)
  circ2 <- Circle$new(c(0.7,1), 0.7)
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  ## intersecting circles
  circ1 <- Circle$new(c(5,4), 2)
  circ2 <- Circle$new(c(8,5), 3)
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  ## tangent circles
  circ1 <- Circle$new(c(5,4), 2)
  circ2 <- Circle$new(c(8,4), 1)
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ1, circ2, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, TRUE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
  #
  iota <- inversionSwappingTwoCircles(circ2, circ1, FALSE)
  expect_true(circ1$isEqual(iota$invertCircle(circ2)))
  expect_true(circ2$isEqual(iota$invertCircle(circ1)))
})

test_that("inversionFixingTwoCircles", {
  circ1 <- Circle$new(c(1,0), 3)
  circ2 <- Circle$new(c(2,2), 1)
  iota <- inversionFixingTwoCircles(circ1, circ2)
  newcirc1 <- iota$invertCircle(circ1)
  newcirc2 <- iota$invertCircle(circ2)
  expect_true(circ1$isEqual(newcirc1))
  expect_true(circ2$isEqual(newcirc2))
})

test_that("inversionFixingThreeCircles", {
  circ1 <- Circle$new(c(1,0), 3)
  circ2 <- Circle$new(c(2,2), 1)
  circ3 <- Circle$new(c(3,5), 2)
  iota <- inversionFixingThreeCircles(circ1, circ2, circ3)
  newcirc1 <- iota$invertCircle(circ1)
  newcirc2 <- iota$invertCircle(circ2)
  newcirc3 <- iota$invertCircle(circ3)
  expect_true(circ1$isEqual(newcirc1))
  expect_true(circ2$isEqual(newcirc2))
  expect_true(circ3$isEqual(newcirc3))
})

test_that("Compose inversions", {
  iota1 <- Inversion$new(c(1,1), 2)
  iota2 <- Inversion$new(c(3,2), 4)
  M <- c(4,5)
  P <- iota1$invert(iota2$invert(M))
  Mob <- iota2$compose(iota1)
  Q <- Mob$transform(M)
  expect_equal(P, Q)
  # with a negative power
  iota2 <- Inversion$new(c(3,2), -4)
  P <- iota1$invert(iota2$invert(M))
  Mob <- iota2$compose(iota1)
  Q <- Mob$transform(M)
  expect_equal(P, Q)
})
