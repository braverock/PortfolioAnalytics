

# Complete cases centroid computed numerically
centroid.complete.mc(order = c(3, 1, 2, 4))
barplot(centroid.complete.mc(50:1))

# Express a view on the assets in two sectors
# Sector 1 View: R_2 < R_1 < R_4
# Sector 2 View: R_5 < R_3
x <- list()
x[[1]] <- c(2, 1, 4)
x[[2]] <- c(5, 3)
barplot(centroid.sectors(x))

y <- list()
y[[1]] <- 10:1
y[[2]] <- 40:11
barplot(centroid.sectors(y))

# Express a view that
# R_1 < R_2 < 0 < R_3 < R_4
centroid.sign(c(1, 2), c(4, 3))

# The centroid values of 16:50 are negative
barplot(centroid.sign(15:1, 50:16))

z <- list()
z[[1]] <- c(1, 3)
z[[2]] <- c(2, 4)
barplot(centroid.buckets(z))

zz <- list()
zz[[1]] <- 10:1
zz[[2]] <- 20:11
zz[[3]] <- 30:21
zz[[4]] <- 40:31
zz[[5]] <- 50:41
barplot(centroid.buckets(zz))

