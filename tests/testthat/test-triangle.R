context("Triangle")

test_that("Triangle$contains", {
  t <- Triangle$new(c(0,0), c(1,5), c(3,3))
  set.seed(666)
  pts <- t$randomPoints(3, "in")
  expect_true(all(apply(pts, 1L, t$contains)))
})

test_that("Malfatii circles are tangent", {
  t <- Triangle$new(c(0,0), c(1,5), c(3,3))
  Mcircles <- t$MalfattiCircles(tangencyPoints = TRUE)
  C1 <- Mcircles[[1L]]; C2 <- Mcircles[[2L]]; C3 <- Mcircles[[3L]]
  I1 <- intersectionCircleCircle(C1, C2)
  expect_true(is.numeric(I1) && length(I1) == 2L)
  I2 <- intersectionCircleCircle(C1, C3)
  expect_true(is.numeric(I2) && length(I2) == 2L)
  I3 <- intersectionCircleCircle(C2, C3)
  expect_true(is.numeric(I3) && length(I3) == 2L)
  tpoints <- attr(Mcircles, "tangencyPoints")
  expect_equal(tpoints$TA, I3)
  expect_equal(tpoints$TB, I2)
  expect_equal(tpoints$TC, I1)
})

test_that("Gergonne point is the symmedian point of the Gergonne triangle", {
  t <- Triangle$new(c(0,0), c(1,5), c(4,3))
  gpoint <- t$GergonnePoint()
  intouchTriangle <- t$GergonneTriangle()
  sympoint <- intouchTriangle$symmedianPoint()
  expect_equal(gpoint, sympoint)
})

test_that("Symmedian point of a right triangle", {
  t <- Triangle$new(c(0,0), c(1,5), c(3,3))
  sympoint <- t$symmedianPoint()
  orthic <- t$orthicTriangle()
  H <- orthic$C
  expect_equal(sympoint, (t$C+H)/2)
})

test_that("Gergonne triangle of tangential triangle is reference triangle", {
  tref <- Triangle$new(c(0,0), c(1,5), c(4,3))
  ttref <- tref$tangentialTriangle()
  gergonnettref <- ttref$GergonneTriangle()
  expect_equal(
    cbind(tref$A, tref$B, tref$C),
    cbind(gergonnettref$A, gergonnettref$B, gergonnettref$C)
  )
})

test_that("Orthogonality Parry circle", {
  t <- Triangle$new(c(0,0), c(1,5), c(4,3))
  parry <- t$ParryCircle()
  brocard <- t$BrocardCircle()
  circum <- t$circumcircle()
  expect_true(parry$isOrthogonal(brocard))
  expect_true(parry$isOrthogonal(circum))
})

test_that("Brocard points - concurrency", {
  t <- Triangle$new(c(0,0), c(1,5), c(5,2))
  bpoints <- t$BrocardPoints()
  bline1 <- Line$new(t$A, bpoints$Z1)
  bline2 <- Line$new(t$A, bpoints$Z2)
  median <- Line$new(t$B, t$centroid())
  symmedian <- Line$new(t$C, t$symmedianPoint())
  P <- intersectionLineLine(bline1, median)
  expect_true(symmedian$includes(P))
  # Q <- intersectionLineLine(bline2, median)
  # expect_true(symmedian$includes(Q)) ???
})

test_that("Brocard points - distance from circumcenter", {
  t <- Triangle$new(c(0,0), c(1,5), c(5,2))
  bpoints <- t$BrocardPoints()
  O <- t$circumcenter()
  R <- t$circumradius()
  edges <- as.list(t$edges())
  d <- R * with(edges, sqrt((a^4+b^4+c^4)/(a^2*b^2+a^2*c^2+b^2*c^2)-1))
  expect_equal(d, .distance(bpoints$Z1, O))
  expect_equal(d, .distance(bpoints$Z2, O))
})
