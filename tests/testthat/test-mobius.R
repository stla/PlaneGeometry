context("Mobius transformations")

test_that("Image of a circle", {
  circ <- Circle$new(c(2,3), 4)
  Mob <- Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
  circ1 <- Mob$transformCircle(circ)
  P <- Mob$transform(circ$pointFromAngle(0))
  Q <- Mob$transform(circ$pointFromAngle(60))
  R <- Mob$transform(circ$pointFromAngle(120))
  circ2 <- Triangle$new(P,Q,R)$circumcircle()
  expect_true(circ1$isEqual(circ2))
})
