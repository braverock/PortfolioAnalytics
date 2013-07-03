library(PortfolioAnalytics)

# Testing to see how rp_transform handles group constraints

##### EX1 #####
# first group exceeds cUP
weights <- c(0.15, 0.35, 0.50)
sum(weights)

groups <- c(2, 1)
cLO <- c(0.1, 0.10)
cUP <- c(0.45, 0.8)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))

group_fail(weights, groups, cLO, cUP)

w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 200)
w
group_fail(w, groups, cLO, cUP)

##### EX2 #####
# The assets are grouped into 3 groups of 2
# The sum of the weights for the first group assets must be between 0.05 and 0.35
# The sum of the weights for the second group of assets must be between 0.10 and 0.45
# The sum of the weights for the last group of assets must be between 0.05 and 0.25

# first group exceeds cUP
weights <- c(0.15, 0.30, 0.15, 0.25, 0.05, 0.10)
sum(weights)

groups <- c(2, 2, 2)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.35)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))


group_fail(weights, groups, cLO, cUP)

# groups is NULL and box and leverage constraints are satisfied so this should
# just return the original weights vector
w <- rp_transform(weights, min_sum, max_sum, min, max, groups=NULL, cLO, cUP, 500)
w

# The first group exceeds cUP so the weights vector should be modified
w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 1000)
w
group_fail(w, groups, cLO, cUP)

##### Ex3 #####
# The second group is below cLO and the third weight is below min
weights <- c(0.15, 0.25, 0.08, 0.2, 0.22, 0.10)
sum(weights)

groups <- c(2, 1, 3)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.65)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.1, length(weights))
max <- rep(0.65, length(weights))


group_fail(weights, groups, cLO, cUP)

w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 500)
w
group_fail(w, groups, cLO, cUP)

##### Ex4 #####
# The second group is above cUP and the fourth group is below cLO
weights <- c(0.06, 0.1, 0.07, 0.2, 0.22, 0.10, 0.05, 0.08, 0.05, 0.04, 0.03)
sum(weights[1:2])
sum(weights[3:6])
sum(weights[7:10])
sum(weights[10:11])
sum(weights)

groups <- c(2, 4, 3, 2)
cLO <- c(0.05, 0.10, 0.05, 0.08)
cUP <- c(0.4, 0.55, 0.65, 0.45)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))

group_fail(weights, groups, cLO, cUP)

# Note that this was typically not working with max_permutations=200
# Relax constraints or increase max_permutations
rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 1000)
