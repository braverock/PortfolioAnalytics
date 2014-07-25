
require(testthat)
require(PortfolioAnalytics)

context("random portfolios sample method")

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

init.portf <- portfolio.spec(assets=funds)
init.portf <- add.constraint(init.portf, type="weight_sum", 
                             min_sum=0.99, max_sum=1.01)
init.portf <- add.constraint(init.portf, type="box", 
                             min=-0.3, max=0.65)

# generate portfolios to satisfy weight_sum and box constraints
rp1 <- random_portfolios(init.portf, 1000, eliminate=FALSE)
test_that("we have created at least 1 feasible portfolio to satisfy weight_sum and box constraints", {
  expect_that(any(apply(rp1, 1, PortfolioAnalytics:::check_constraints, portfolio=group.portf)), is_true())
})

# portfolio with group constraints
group.portf <- add.constraint(init.portf, type="group", 
                              groups=list(1:2,3:4), 
                              group_min=c(0.08, 0.05), 
                              group_max=c(0.55, 0.85),
                              group_pos=c(2,2))

# generate portfolios to satisfy weight_sum, box, and group constraints
rp2 <- random_portfolios(group.portf, 1000, eliminate=FALSE)
test_that("we have created at least 1 feasible portfolio to satisfy weight_sum, box, and group constraints", {
  expect_that(any(apply(rp2, 1, PortfolioAnalytics:::check_constraints, portfolio=group.portf)), is_true())
})

# add leverage exposure constraint
lev.portf <- add.constraint(init.portf, type="leverage_exposure", 
                            leverage=1.6)

# generate portfolios to satisfy weight_sum, box, and leverage constraints
rp3 <- random_portfolios(lev.portf, 1000, eliminate=FALSE)
test_that("we have created at least 1 feasible portfolio to satisfy weight_sum, box, and leverage constraints", {
  expect_that(any(apply(rp3, 1, PortfolioAnalytics:::check_constraints, portfolio=group.portf)), is_true())
})

# add position limit constraint
pos1.portf <- add.constraint(init.portf, type="position_limit", 
                             max_pos=3)

# generate portfolios to satisfy weight_sum, box, and position limit constraints
rp4 <- random_portfolios(pos1.portf, 1000, eliminate=FALSE)
test_that("we have created at least 1 feasible portfolio to satisfy weight_sum, box, and position limit constraints", {
  expect_that(any(apply(rp4, 1, PortfolioAnalytics:::check_constraints, portfolio=group.portf)), is_true())
})

# add position limit constraint with long and short position limits
pos2.portf <- add.constraint(init.portf, type="position_limit", 
                       max_pos_long=3, max_pos_short=1)

# generate portfolios to satisfy weight_sum, box, and position limit constraints
rp5 <- random_portfolios(pos2.portf, 1000, eliminate=FALSE)
test_that("we have created at least 1 feasible portfolio to satisfy weight_sum, box, and long/short position limit constraints", {
  expect_that(any(apply(rp5, 1, PortfolioAnalytics:::check_constraints, portfolio=group.portf)), is_true())
})

