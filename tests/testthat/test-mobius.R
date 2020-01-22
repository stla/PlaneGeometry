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

test_that("Image of a line", {
  line <- Line$new(c(2,3), c(1,5))
  # in case c=0, the image is a line
  Mob <- Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
  expect_is(Mob$transformLine(line), "Line")
  # case of a circle image
  Mob <- Mobius$new(rbind(c(1+1i,2),c(2-3i,3-2i)))
  circ1 <- Mob$transformLine(line)
  P <- Mob$transform(line$A)
  Q <- Mob$transform(line$B)
  R <- Mob$transform((line$A + line$B)/2)
  circ2 <- Triangle$new(P,Q,R)$circumcircle()
  expect_true(circ1$isEqual(circ2))
  # expect_true(circ1$includes(P) && circ1$includes(Q))
})
