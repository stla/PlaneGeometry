library(PlaneGeometry)
library(elliptic) # for the 'unimodular' function

# Möbius transformations
T <- Mobius$new(rbind(c(0,-1), c(1,0)))
U <- Mobius$new(rbind(c(1,1), c(0,1)))
Phi <- Mobius$new(rbind(c(1i,1), c(1,1i)))

c0 <- Circle$new(c(0, 1.5), 0.5)
l0 <- Line$new(c(0.5,0), c(0.5,1))
l1 <- Line$new(c(-0.5,0), c(-0.5,1))

par(mar = c(0, 0, 0, 0))
plot(NULL, type = "n", xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), asp = 1,
     xlab = NA, ylab = NA)
draw(Circle$new(c(0, 0), 1), border = "black", lwd = 2, col = "yellow")
draw(Phi$transformCircle(c0), border = "black", lwd = 2)
draw(Phi$transformLine(l0), border = "black", lwd = 2)
draw(Phi$transformLine(l1), border = "black", lwd = 2)
draw(U$compose(Phi)$transformCircle(c0), border = "black", lwd = 2)
draw(U$compose(Phi)$transformLine(l0), border = "black", lwd = 2)
draw(U$compose(Phi)$transformLine(l1), border = "black", lwd = 2)

