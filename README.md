# PlaneGeometry: plane geometry with R

<!-- badges: start -->
[![R-CMD-check](https://github.com/stla/PlaneGeometry/workflows/R-CMD-check/badge.svg)](https://github.com/stla/PlaneGeometry/actions)
<!-- badges: end -->


```r
library(PlaneGeometry)

# starting triangle
tr <- Triangle$new(c(0,0), c(4,1), c(3,3))
# excircles
excircles <- tr$excircles()
JA <- excircles[["A"]]$center
JB <- excircles[["B"]]$center
JC <- excircles[["C"]]$center
# hexyl triangle
ht <- tr$hexylTriangle()
A <- ht$A; B <- ht$B; C <- ht$C

opar <- par(mar = c(0, 0, 0, 0))
plot(NULL, type = "n", asp = 1, xlim = c(-3, 6), ylim = c(-4, 6),
     xlab = NA, ylab = NA, axes = FALSE)
# draw reference triangle
draw(tr, lwd = 2, col = "blue")
# draw excircles and their center
draw(excircles[["A"]]); draw(excircles[["B"]]); draw(excircles[["C"]])
points(rbind(JA, JB, JC), pch = 19, col = "green")
# draw hexyl triangle
draw(ht, lwd = 2, col = "red")
## the vertices of the hexyl triangle and the excenters form an hexagon
## whose opposite sides are parallel
polygon(rbind(A, JC, B, JA, C, JB), border = "green", lwd = 2)
one_side <- Line$new(A, JC)
opposite_side <- Line$new(C, JA)
one_side$isParallel(opposite_side)
# TRUE
## the vertices of this hexagon lie on an ellipse
ell <- EllipseFromFivePoints(A, JC, B, JA, C)
draw(ell, lwd = 2)
par(opar)
```

![](https://github.com/stla/PlaneGeometry/raw/master/inst/imgs/hexyl.png)

___

See more examples in [the vignette](https://cran.r-project.org/web/packages/PlaneGeometry/vignettes/examples.html).
