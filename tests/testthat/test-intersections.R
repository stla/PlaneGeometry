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

test_that("Intersection collinear segments", {
  library(sets)
  #
  A <- c(1,1); B <- c(8,8); C <- c(3,3); D <- c(5,5)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(S2$A, S2$B))
  #
  S1 <- Line$new(B, A, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(S2$A, S2$B))
  #
  A <- c(1,1); B <- c(5,5); C <- c(3,3); D <- c(8,8)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(C, B))
  #
  A <- c(3,3); B <- c(8,8); C <- c(1,1); D <- c(5,5)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(A, D))
  #
  A <- c(1,8); B <- c(8,1); C <- c(3,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(C, D))
  #
  C <- c(1,8); B <- c(8,1); A <- c(3,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(A, D))
  #
  A <- c(5,8); B <- c(5,1); C <- c(5,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(C, D))
  #
  A <- c(5,6); B <- c(5,1); C <- c(5,8); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  S2 <- Line$new(C, D, FALSE, FALSE)
  S <- intersectionLineLine(S1, S2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(S$A, S$B) == set(A, D))
})

test_that("Intersection collinear segment and half-line", {
  library(sets)
  #
  A <- c(1,1); B <- c(8,8); C <- c(3,3); D <- c(5,5)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, FALSE, TRUE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(C, B))
  #
  I <- intersectionLineLine(D2, S1, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(C, B))
  #
  S1 <- Line$new(B, A, FALSE, FALSE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(C, B))
  #
  A <- c(1,1); B <- c(5,5); C <- c(3,3); D <- c(8,8)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, FALSE, TRUE)
  S <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(S$extendA || S$extendB)
  expect_true(set(I$A, I$B) == set(C, D))
  #
  D2 <- Line$new(C, D, TRUE, FALSE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(A, B))
  #
  A <- c(3,3); B <- c(8,8); C <- c(1,1); D <- c(5,5)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, FALSE, TRUE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(A, B))
  #
  A <- c(1,8); B <- c(8,1); C <- c(3,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, FALSE, TRUE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(C, B))
  #
  C <- c(1,8); B <- c(8,1); A <- c(3,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, FALSE, TRUE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(A, B))
  #
  A <- c(5,8); B <- c(5,1); C <- c(5,6); D <- c(5,4)
  S1 <- Line$new(A, B, FALSE, FALSE)
  D2 <- Line$new(C, D, TRUE, FALSE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_false(I$extendA || I$extendB)
  expect_true(set(I$A, I$B) == set(D, A))
  # case of one intersection point
  S1 <- Line$new(D, B, FALSE, FALSE)
  D2 <- Line$new(C, D, TRUE, FALSE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_equal(I, D)
  # case of no intersection
  S1 <- Line$new(D, B, FALSE, FALSE)
  D2 <- Line$new(C, A, FALSE, TRUE)
  I <- intersectionLineLine(S1, D2, strict = TRUE)
  expect_null(I)
})
