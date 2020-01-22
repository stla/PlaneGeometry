context("Mobius transformations")

test_that("Image of a circle", {
  circ <- Circle$new(c(2,3), 4)
  # a case with c = 0
  Mob <- Mobius$new(rbind(c(1+1i,2),c(0,3-2i)))
  circ1 <- Mob$transformCircle(circ)
  P <- Mob$transform(circ$pointFromAngle(0))
  Q <- Mob$transform(circ$pointFromAngle(60))
  R <- Mob$transform(circ$pointFromAngle(120))
  circ2 <- Triangle$new(P,Q,R)$circumcircle()
  expect_true(circ1$isEqual(circ2))
  # a case with c != 0
  Mob <- Mobius$new(rbind(c(1+1i,2),c(2-3i,3-2i)))
  circ1 <- Mob$transformCircle(circ)
  P <- Mob$transform(circ$pointFromAngle(0))
  Q <- Mob$transform(circ$pointFromAngle(60))
  R <- Mob$transform(circ$pointFromAngle(120))
  circ2 <- Triangle$new(P,Q,R)$circumcircle()
  expect_true(circ1$isEqual(circ2))
  # a case with -d/c on the circle
  Mob <- Mobius$new(rbind(c(1+1i,2),c(1,-6-3i)))
  line1 <- Mob$transformCircle(circ)
  P <- Mob$transform(circ$pointFromAngle(30))
  Q <- Mob$transform(circ$pointFromAngle(60))
  line2 <- Line$new(P,Q)
  expect_true(line1$isEqual(line2))
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

test_that("MobiusMappingThreePoints", {
  P1 <- c(0,0); P2 <- c(1,1); P3 <- c(5,2)
  Q1 <- c(2,0); Q2 <- c(1,5); Q3 <- c(5,-2)
  Mob <- MobiusMappingThreePoints(P1, P2, P3, Q1, Q2, Q3)
  R1 <- Mob$transform(P1); R2 <- Mob$transform(P2); R3 <- Mob$transform(P3)
  expect_equal(Q1,R1); expect_equal(Q2,R2); expect_equal(Q3,R3)
})

