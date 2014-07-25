
min_sum <- 0.99
max_sum <- 1.01
min_box <- rep(-0.15, length(weights))
max_box <- rep(0.6, length(weights))

# violate min_sum and box constraint
weights <- c(0.2, -0.2, 0.4, 0.5)
sum(weights)

rp_transform(w=weights,
             min_sum=min_sum,
             max_sum=max_sum,
             min_box=min_box,
             max_box=max_box)


# violate max_sum and box constraints
weights <- c(0.35, 0.05, 0.7, 0.1)
sum(weights)

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box)


# violate box constraints and leverage
weights <- c(-0.45, 0.45, 0.55, 0.45)
sum(weights)
sum(abs(weights))
leverage <- 1.6

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              leverage=leverage)


# violate max position limit constraint
weights <- c(0.15, 0.25, 0.4, 0.2)
sum(weights)
max_pos <- 3

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              max_pos=max_pos)

# violate position limit constraint
weights <- c(-0.05, -0.05, 0.4, 0.7)
sum(weights)

max_pos_short <- 1

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              max_pos_short=max_pos_short)

# violate position limit constraint
weights <- c(-0.05, -0.05, 0.4, 0.7)
sum(weights)

max_pos_long <- 3
max_pos_short <- 1

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              max_pos_long=max_pos_long,
              max_pos_short=max_pos_short)


# violate position limit constraint
weights <- c(-0.05, -0.05, 0.4, 0.7)
sum(weights)

max_pos_long <- 3
max_pos_short <- 1
max_pos <- 3

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              max_pos=max_pos,
              max_pos_long=max_pos_long,
              max_pos_short=max_pos_short)

# violate position limit and leverage constraint
weights <- c(-0.25, -0.15, 0.4, 0.7)
sum(weights)
sum(abs(weights))

max_pos_long <- 3
max_pos_short <- 1
max_pos <- 3
leverage <- 1.3

rp_transform(w=weights,
              min_sum=min_sum,
              max_sum=max_sum,
              min_box=min_box,
              max_box=max_box,
              max_pos=max_pos,
              max_pos_long=max_pos_long,
              max_pos_short=max_pos_short,
              leverage=leverage)

# The second group is above cUP and the fourth group is below cLO
weights <- c(0.06, 0.1, 0.07, 0.2, 0.22, 0.10, 0.05, 0.08, 0.05, 0.04, 0.03)
sum(weights[1:2])
sum(weights[3:6])
sum(weights[7:10])
sum(weights[10:11])
sum(weights)

groups <- list(1:2,
               3:6,
               7:10,
               10:11)
# group_pos <- c(2, 3, 2, 2)
group_pos <- NULL
cLO <- c(0.05, 0.10, 0.05, 0.08)
cUP <- c(0.4, 0.55, 0.65, 0.45)
min_sum <- 0.99
max_sum <- 1.01
min_box <- rep(0.05, length(weights))
max_box <- rep(0.65, length(weights))

group_fail(weights, groups, cLO, cUP, group_pos)

w <- rp_transform(weights, min_sum, max_sum, min_box, max_box, groups, cLO, cUP)
PortfolioAnalytics:::group_fail(w, groups, cLO, cUP, group_pos)


# Note that this was typically not working with max_permutations=200
# Relax constraints or increase max_permutations

# max_pos <- 3
# max_pos_long <- 4
# max_pos_short <- 4
# leverage <- Inf
# max_permutations <- 200
# 
# rp_transform2(weights=weights, 
#               min_sum=min_sum, 
#               max_sum=max_sum, 
#               min_box=min_box, 
#               max_box=max_box, 
#               max_pos=max_pos, 
#               max_pos_long=max_pos_long,
#               max_pos_short=max_pos_short,
#               leverage=leverage, 
#               max_permutations=max_permutations)


