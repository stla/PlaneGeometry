library(PlaneGeometry)
library(freegroup)

uniqueWith <- function(v, f){
  size <- length(v)
  for(i in seq_len(size-1L)){
    j <- i + 1L
    while(j <= size){
      if(f(v[[j]], v[[i]])){
        v <- v[-j]
        size <- size - 1L
      }else{
        j <- j + 1L
      }
    }
  }
  v[1L:size]
}

a <- alpha(1)
b <- alpha(2)
B <- inverse(b)

# words of size n
n <- 4L
G4 <- do.call(expand.grid, rep(list(c("a", "b", "B")), n))
G4 <- split(as.matrix(G4), 1:nrow(G4))
n <- 5L
G <- do.call(expand.grid, rep(list(c("a", "b", "B")), n))
G <- c(G4, split(as.matrix(G), 1:nrow(G)))
G <- lapply(G, function(w){
  sum(do.call(c.free, lapply(w, function(x) switch(x, a=a, b=b, B=B))))
})
G <- uniqueWith(G, free_equal)
sizes <- vapply(G, total, numeric(1L))
Gn <- G[sizes > 0]

transfo2seq <- function(g){
  seq <- c()
  gr <- reduce(g)[[1L]]
  for(j in 1L:ncol(gr)){
    monomial <- gr[, j]
    t <- c("a", "b")[monomial[1L]]
    i <- monomial[2L]
    if(i < 0L){
      i <- -i
      t <- c(a = "a", b = "B")[[t]]
    }
    seq <- c(seq, rep(t, i))
  }
  seq <- gsub("aa", "", paste0(seq, collapse = ""))
  strsplit(seq, "")[[1]]
}

allseqs <- uniqueWith(lapply(Gn, transfo2seq), function(s1, s2) paste0(s1, collapse = "") == paste0(s2, collapse = ""))
keep <- lengths(allseqs) > 0L
allseqs <- allseqs[keep]

# Möbius transformations
T <- Mobius$new(rbind(c(0,-1), c(1,0)))
Tinv <- T
U <- Mobius$new(rbind(c(1,1), c(0,1)))
Uinv <- Mobius$new(rbind(c(1,-1), c(0,1)))
Phi <- Mobius$new(rbind(c(1i,1), c(1,1i)))

c0 <- Circle$new(c(0, 1.5), 0.5)
l0 <- Line$new(c(0.5,0), c(0.5,1))
l1 <- Line$new(c(-0.5,0), c(-0.5,1))

MOBS <- list(a = T, b = U, B = Uinv)

mobius <- function(seq){
  if(length(seq) == 1L){
    return(MOBS[[seq]])
  }
  mobs <- MOBS[seq]
  Reduce(function(M1, M2) M1$compose(M2), mobs)
}

allcircles <- vector("list", 3*length(allseqs))
for(i in seq_along(allseqs)){
  M <- mobius(allseqs[[i]])$compose(Phi)
  allcircles[[3*(i-1)+1]] <- M$transformCircle(c0)
  allcircles[[3*(i-1)+2]] <- M$transformLine(l0)
  allcircles[[3*(i-1)+3]] <- M$transformLine(l1)
}
allcircles <- c(allcircles, list(Phi$transformLine(l0)), list(Phi$transformLine(l1)))

allcircles <- uniqueWith(allcircles, function(c1, c2){
  if(is(c1, "Circle") && is(c2, "Circle") || is(c1, "Line") && is(c2, "Line")) c1$isEqual(c2) else FALSE
})
circles <- arcs <- lines <- list()
for(circ in allcircles){
  if(is(circ, "Circle")){
    pts <- intersectionCircleCircle(circ, unitCircle)
    if(length(pts) == 2){
      alpha1 <- atan2(pts[[1]][2], pts[[1]][1])
      alpha2 <- atan2(pts[[2]][2], pts[[2]][1])
      arc <- unitCircle$orthogonalThroughTwoPointsOnCircle(alpha1, alpha2, arc = TRUE)
      arcs <- append(arcs, arc)
    }else{
      circles <- append(circles, circ)
    }
  }else{
    lines <- append(lines, circ)
  }
}

unitCircle <- Circle$new(c(0, 0), 1)
par(mar = c(0, 0, 0, 0))
plot(NULL, type = "n", xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), asp = 1,
     xlab = NA, ylab = NA)
draw(unitCircle, border = "black", lwd = 2, col = "seashell")
for(arc in arcs){
  draw(arc, col = "yellow", lwd = 2)
}
for(circle in circles){
  draw(circle, border = "black", lwd = 2, col = "midnightblue")
}
for(line in lines){
  draw(line, col = "black", lwd = 2)
}


library(rgl)

unitSphere <- subdivision3d(icosahedron3d(), depth = 4)
unitSphere$vb[4, ] <-
  apply(unitSphere$vb[1:3, ], 2, function(x) sqrt(sum(x * x)))
unitSphere$normals <- unitSphere$vb
drawSphere <- function(circle, ...){
  center <- circle$center
  radius <- circle$radius
  sphere <- scale3d(unitSphere, radius, radius, radius)
  shade3d(translate3d(sphere, center[1], center[2], 0), ...)
}

Psi <- Phi$inverse()
R <- U$compose(T)
Rt <- function(t) R$gpower(t)

open3d(windowRect = c(50, 50, 562, 562))
view3d(0, 0, zoom = 0.8)
bg3d(rgb(54, 57, 64, maxColorValue = 255))
for(circ in circles){
  M <- Psi$compose(Rt(1))$compose(Phi)
  drawSphere(M$transformCircle(circ), color = "midnightblue")
}
