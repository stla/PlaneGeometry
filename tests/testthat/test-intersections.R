context("Intersections")

test_that("intersectionCircleCircle", {
  # two intersection points
  A <- c(0,0); B <- c(2,0); C <- c(3,2); D <- c(3,-3)
  ABC <- Triangle$new(A,B,C); ABD <- Triangle$new(A,B,D)
  c1 <- ABC$circumcircle(); c2 <- ABD$circumcircle()
  Is <- intersectionCircleCircle(c1, c2)
  expect_equal(A, Is[[1L]]); expect_equal(B, Is[[2L]])
  # one intersection point
  c1 <- Circle$new(c(0,0), 2); c2 <- Circle$new(c(5,0), 3)
  I <- intersectionCircleCircle(c1, c2)
  expect_equal(c(2,0), I)
  # no intersection
  ## c1 and c2 external
  c1 <- Circle$new(c(0,0), 2); c2 <- Circle$new(c(5,0), 1)
  expect_null(intersectionCircleCircle(c1, c2))
  ## c1 included in c2
  c1 <- Circle$new(c(4,0), 2); c2 <- Circle$new(c(5,0), 5)
  expect_null(intersectionCircleCircle(c1, c2))
})
