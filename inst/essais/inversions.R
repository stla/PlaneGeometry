library(PlaneGeometry)

# generation 0
angles <- c(0, pi/2, pi, 3*pi/2)
gen0 <- c(
  lapply(seq_along(angles), function(i){
    beta <- angles[i]
    list(
      gcircle = Circle$new(center = c(cos(beta), sin(beta)), radius = 1),
      gen = 0L,
      base = i
    )
  }),
  list(list(
    gcircle = Circle$new(center = c(0, 0), radius = 2),
    gen = 0L,
    base = length(angles) + 1L
  ))
)
n0 <- length(gen0)

# generations 1, 2, 3
generations <- vector("list", length = 4L)
generations[[1L]] <- gen0
for(g in 2L:4L){
  gen <- generations[[g-1L]]
  n <- length(gen)
  n1 <- n*(n0 - 1L)
  gen_new <- vector("list", length = n1)
  k <- 0L
  while(k < n1){
    for(j in 1L:n){
      gcircle_j <- gen[[j]]
      for(i in 1L:n0){
        if(i != gcircle_j[["base"]]){
          k <- k + 1L
          circ <- gen0[[i]][["gcircle"]]
          iota <- Inversion$new(pole = circ$center, power = circ$radius^2)
          gen_new[[k]] <- list(
            gcircle = iota$invertGcircle(gcircle_j[["gcircle"]]),
            gen = g - 1L,
            base = i
          )
        }
      }
    }
  }
  generations[[g]] <- gen_new
}


gcircles <- c(
  generations[[1L]], generations[[2L]], generations[[3L]], generations[[4L]]
)

uniqueWith <- function(v, f){
  size <- length(v)
  for(i in seq_len(size-1L)){
    j <- i + 1L
    while(j <= size){
      if(f(v[[i]], v[[j]])){
        v <- v[-j]
        size <- size - 1L
      }else{
        j <- j + 1L
      }
    }
  }
  v[1L:size]
}

gcircles <- uniqueWith(
  gcircles,
  function(g1, g2){
    g1 <- g1[["gcircle"]]; g2 <- g2[["gcircle"]]
    class(g1)[1L] == class(g2)[1L] && g1$isEqual(g2)
  }
)


drawGcircle <- function(gcircle, colors = rainbow(4L), ...){
  gen <- gcircle[["gen"]]
  gcircle <- gcircle[["gcircle"]]
  if(is(gcircle, "Circle")){
    draw(gcircle, border = colors[1L + gen], ...)
  }else{
    draw(gcircle, col = colors[1L + gen], ...)
  }
}

par(mar = c(0,0,0,0), bg = "black")
plot(0, 0, type = "n", xlim = c(-2.3, 2.3), ylim = c(-2.3, 2.3),
     asp = 1, axes = FALSE, xlab = NA, ylab = NA)
invisible(lapply(gcircles, drawGcircle, lwd=2))
