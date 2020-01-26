context("Elliptical arc")

test_that("Length", {
  ell <- Ellipse$new(c(1,1), 5, 2, 50)
  #
  alpha1 <- 0; alpha2 <- 359
  arc <- EllipticalArc$new(ell, alpha1, alpha2)
  path <- arc$path(500L)
  perim <- 0
  for(i in 2L:nrow(path)){
    perim <- perim + sqrt(c(crossprod(path[i-1,]-path[i,])))
  }
  expect_equal(perim, arc$length()$value, tolerance = 1e-3)
  #
  alpha1 <- 330; alpha2 <- 60
  arc <- EllipticalArc$new(ell, alpha1, alpha2)
  path <- arc$path(500L)
  perim <- 0
  for(i in 2L:nrow(path)){
    perim <- perim + sqrt(c(crossprod(path[i-1,]-path[i,])))
  }
  expect_equal(perim, arc$length()$value, tolerance = 1e-3)
  #
  alpha1 <- -30; alpha2 <- 60
  arc <- EllipticalArc$new(ell, alpha1, alpha2)
  path <- arc$path(500L)
  perim <- 0
  for(i in 2L:nrow(path)){
    perim <- perim + sqrt(c(crossprod(path[i-1,]-path[i,])))
  }
  expect_equal(perim, arc$length()$value, tolerance = 1e-3)
  #
  arc <- EllipticalArc$new(ell, 0, 180)
  expect_equal(
    2*ell$rmajor*gsl::ellint_Ecomp(sqrt(1-ell$rminor^2/ell$rmajor^2)),
    arc$length()
  )
})
