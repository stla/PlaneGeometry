context("Inversions")

test_that("inversionMappingCircle2Circle", {
  circ1 <- Circle$new(c(0,0), 3)
  circ2 <- Circle$new(c(2,2), 1)
  iota <- inversionMappingCircle2Circle(circ1, circ2)
  circ3 <- iota$invertCircle(circ1)
  expect_true(circ2$isEqual(circ3))
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

