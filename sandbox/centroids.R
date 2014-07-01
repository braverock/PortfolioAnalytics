
# Numerically compute the centroid for different cases as described in
# the Almgren and Chriss paper.

# These replicate the paper, now I just need to functionalize them


# Complete sort
nsim <- 1000
nassets <- 50
out <- matrix(0, nsim, nassets)
for(i in 1:nsim){
  out[i,] <- sort(rnorm(nassets), decreasing=TRUE)
}

barplot(colMeans(out))

# Complete sort with multiple sectors
sectors <- list()
sectors[[1]] <- 1:10
sectors[[2]] <- 11:40
nassets <- length(unlist(sectors))
nsectors <- length(sectors)

sim.list <- vector("list", nsectors)
for(j in 1:nsectors){
  nassets <- length(sectors[[j]])
  out <- matrix(0, nsim, nassets)
  for(i in 1:nsim){
    out[i,] <- sort(rnorm(nassets), decreasing=TRUE)
  }
  sim.list[[j]] <- out
}
barplot(unlist(lapply(sim.list, colMeans)))

# Complete sort with comparison to 0
my.list <- list()
my.list$pos <- c(1, 2, 3, 4)
my.list$neg <- c(5, 6, 7, 8 , 9, 10)
pos <- length(my.list$pos)
neg <- length(my.list$neg)

nsim <- 1000
nassets <- pos + neg

out <- matrix(0, nsim, nassets)
for(i in 1:nsim){
  tmp <- rnorm(nassets)
  tmp.pos <- tmp[1:pos]
  tmp.neg <- tmp[(pos+1):(pos+neg)]
  
  # Sign correct the pos assets
  idx <- which(tmp.pos < 0)
  if(length(idx) != 0){
    tmp.pos[idx] <- -1 * tmp.pos[idx]
  }
  
  # Sign correct the neg assets
  idx <- which(tmp.neg > 0)
  if(length(idx) != 0){
    tmp.neg[idx] <- -1 * tmp.neg[idx]
  }
  out[i,] <- sort(c(tmp.pos, tmp.neg), decreasing=TRUE)
}

barplot(colMeans(out))


# Complete sort with "buckets"
qlist <- list()
qlist[[1]] <- c(1, 2, 3, 4)
qlist[[2]] <- c(5, 6, 7, 8)
qlist[[3]] <- c(9, 10, 11, 12)
qlist[[4]] <- c(13, 14, 15, 16)

nsim <- 1000
nassets <- length(unlist(qlist))
nbuckets <- length(qlist)
out <- matrix(0, nsim, nassets)
for(i in 1:nsim){
  tmp <- sort(rnorm(nbuckets), decreasing=TRUE)
  vec <- c()
  for(j in 1:nbuckets){
    vec <- c(vec, rep(tmp[j], length(qlist[[j]])))
  }
  out[i,] <- vec
}

barplot(colMeans(out))
