

##### EX1 #####
# first group exceeds cUP
groups <- c(2, 1)
cLO <- c(0.2, 0.10)
cUP <- c(0.4, 0.55)

weights <- c(0.15, 0.35, 0.50)
sum(weights)

(w <- constrained_group_tmp(groups, cLO, cUP, weights, 1, 1, TRUE))
sum(w[1:2])
sum(w)

(w <- constrained_group_tmp(groups, cLO, cUP, weights, 1, 1, FALSE))
# The group 1 cUP is met exactly, but the sume of weights are not equal to 1
sum(w[1:2])
sum(w)


##### EX2 #####
# The assets are grouped into 3 groups of 2
# The sum of the weights for the first group assets must be between 0.05 and 0.35
# The sum of the weights for the second group of assets must be between 0.10 and 0.45
# The sum of the weights for the last group of assets must be between 0.05 and 0.25

# first group exceeds cUP
groups <- c(2, 2, 2)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.25)

weights <- c(0.15, 0.30, 0.15, 0.25, 0.05, 0.10)
sum(weights)

(w <- constrained_group_tmp(groups, cLO, cUP, weights, 1, 1, TRUE))
sum(w)

##### Ex3 #####
# The second group is below cLO
groups <- c(2, 1, 3)
cLO <- c(0.05, 0.10, 0.05)
cUP <- c(0.4, 0.45, 0.65)

weights <- c(0.15, 0.25, 0.08, 0.2, 0.22, 0.10)
sum(weights)

(w <- constrained_group_tmp(groups, cLO, cUP, weights, 1, 1, TRUE))
sum(w)

##### Ex4 #####
# The second group is above cUP and the fourth group is below cLO
groups <- c(2, 4, 3, 2)
cLO <- c(0.05, 0.10, 0.05, 0.08)
cUP <- c(0.4, 0.5, 0.65, 0.45)

weights <- c(0.05, 0.1, 0.07, 0.2, 0.22, 0.10, 0.05, 0.08, 0.05, 0.04, 0.04)
sum(weights[1:2])
sum(weights[3:6])
sum(weights[7:10])
sum(weights[10:11])
sum(weights)

(w <- constrained_group_tmp(groups, cLO, cUP, weights, 1, 1, TRUE))
sum(w[1:2])
sum(w[3:6])
sum(w[7:10])
sum(w[10:11])

# Group 2 cUP is being violated. Appears that normalizing at the end of the 
# function is causing some of the group constraints to be violated
