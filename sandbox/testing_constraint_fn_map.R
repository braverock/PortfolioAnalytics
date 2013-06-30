# testing for constraint_fnMap functions

##### test txfrm_box_constraint #####
# transform weights that violate min and max box constraints

min <- c(0.05, 0.08, 0.15, 0.1, 0.15)
max <- c(0.45, 0.55, 0.45, 0.6, 0.45)

# elements 2 and 5 violate min
# element 3 violates max
w <- c(0.1, 0.06, 0.48, 0.25, 0.11)
which(w < min)
which(w > max)

w.box <- txfrm_box_constraint(w, min, max)
sum(w.box) # max_sum is violated
w.box
any(w.box < min)
any(w.box > max)

###### test txfrm_group_constraint #####
# transform weights that violate group constraints
groups <- c(2,2)
cLO <- c(0.1, 0.2)
cUP <- c(0.3, 0.8)

# cUP is violated for the first group
w <- c(0.15, 0.2, 0.15, 0.5)

w.grp <- txfrm_group_constraint(weights=w, groups=groups, cLO=cLO, cUP=cUP)
w.grp
sum(w.grp) # min_sum  is violated

##### test txfrm_weight_sum_constraint #####
# normalizes the weights to min_sum or max_sum by transforming the entire vector
sum(w.box)
w1 <- txfrm_weight_sum_constraint(weights=w.box, min_sum=0.99, max_sum=1.01)
w1
min
# elements 2 and 5 barely violate min

sum(w.grp)
w2 <- txfrm_weight_sum_constraint(weights=w.grp, min_sum=0.99, max_sum=1.01)
cLO
cUP
# the first group barely violates cUP
sum(w2[1:2])
sum(w2[3:4])






