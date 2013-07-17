library(PortfolioAnalytics)

# Testing to see how rp_transform handles group constraints

##### EX1 #####
# first group exceeds cUP
weights <- c(0.15, 0.35, 0.50)
sum(weights)

groups <- c(2, 1)
group_pos <- c(1, 1)
cLO <- c(0.1, 0.10)
cUP <- c(0.45, 0.8)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))

group_fail(weights, groups, cLO, cUP, group_pos)

w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 2, group_pos, 200)
w
group_fail(w, groups, cLO, cUP, group_pos)

##### EX2 #####
# The assets are grouped into 3 groups of 2
# The sum of the weights for the first group assets must be between 0.05 and 0.35
# The sum of the weights for the second group of assets must be between 0.10 and 0.45
# The sum of the weights for the last group of assets must be between 0.05 and 0.25

# first group exceeds cUP
weights <- c(0.15, 0.30, 0.15, 0.25, 0.05, 0.10)
sum(weights)

groups <- c(2, 2, 2)
group_pos <- c(2, 2, 1)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.35)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))


group_fail(weights, groups, cLO, cUP, group_pos)

# groups and max_pos are NULL and box and leverage constraints are satisfied so this should
# just return the original weights vector
w <- rp_transform(weights, min_sum, max_sum, min, max, groups=NULL, cLO, cUP, max_pos=NULL, group_pos, 500)
w

# The first group exceeds cUP so the weights vector should be modified
w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, max_pos=NULL, group_pos, 1000)
w
group_fail(w, groups, cLO, cUP, group_pos)

##### Ex3 #####
# The second group is below cLO and the third weight is below min
weights <- c(0.15, 0.25, 0.08, 0.2, 0.22, 0.10)
sum(weights)

groups <- c(2, 1, 3)
group_pos <- c(1, 1, 3)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.65)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.1, length(weights))
max <- rep(0.65, length(weights))


group_fail(weights, groups, cLO, cUP, group_pos)

w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 5, group_pos, 500)
w
group_fail(w, groups, cLO, cUP, group_pos)

##### Ex4 #####
# The second group is above cUP and the fourth group is below cLO
weights <- c(0.06, 0.1, 0.07, 0.2, 0.22, 0.10, 0.05, 0.08, 0.05, 0.04, 0.03)
sum(weights[1:2])
sum(weights[3:6])
sum(weights[7:10])
sum(weights[10:11])
sum(weights)

groups <- c(2, 4, 3, 2)
group_pos <- c(2, 3, 2, 2)
cLO <- c(0.05, 0.10, 0.05, 0.08)
cUP <- c(0.4, 0.55, 0.65, 0.45)
min_sum <- 0.99
max_sum <- 1.01
min <- rep(0.05, length(weights))
max <- rep(0.65, length(weights))

group_fail(weights, groups, cLO, cUP, group_pos)

# Note that this was typically not working with max_permutations=200
# Relax constraints or increase max_permutations
w <- rp_transform(weights, min_sum, max_sum, min, max, groups, cLO, cUP, 7, group_pos, 500)
w
group_fail(w, groups, cLO, cUP, group_pos)