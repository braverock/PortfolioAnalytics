
require(testthat)
require(PortfolioAnalytics)

context("constraints")

N <- 4
init.portf <- portfolio.spec(assets=N)
# Weight_sum constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="weight_sum", 
                             min_sum=0.99, 
                             max_sum=1.01)
# Box constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="box", 
                             min=0, 
                             max=1)
# Group constraint
init.portf <- add.constraint(portfolio=init.portf,
                             type="group",
                             groups=list(c(1, 3), c(2, 4)),
                             group_min=c(0.15, 0.25),
                             group_max=c(0.65, 0.55))
# Turnover constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="turnover", 
                             turnover_target=0.6)
# Diversification constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="diversification", 
                             div_target=0.55)
# Position limit constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="position_limit", 
                             max_pos=3, 
                             max_pos_long=2,
                             max_pos_short=1)
# Return constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="return", 
                             return_target=0.007)
# Factor exposure constraint
init.portf <- add.constraint(portfolio=init.portf, 
                             type="factor_exposure",
                             B=rep(1, N),
                             lower=0.9, 
                             upper=1.1)

tmp_constraints <- PortfolioAnalytics:::get_constraints(init.portf)

test_that("weight_sum constraint is consistent", {
  expect_that(tmp_constraints$min_sum, equals(0.99))
  expect_that(tmp_constraints$max_sum, equals(1.01))
})

test_that("box constraint is consistent", {
  expect_that(as.numeric(tmp_constraints$min), equals(rep(0, N)))
  expect_that(as.numeric(tmp_constraints$max), equals(rep(1, N)))
})

test_that("group constraint is consistent", {
  expect_that(is.list(tmp_constraints$groups), is_true())
  expect_that(tmp_constraints$groups[[1]], equals(c(1, 3)))
  expect_that(tmp_constraints$groups[[2]], equals(c(2, 4)))
  expect_that(tmp_constraints$group_labels, equals(c("group1", "group2")))
  expect_that(tmp_constraints$cLO, equals(c(0.15, 0.25)))
  expect_that(tmp_constraints$cUP, equals(c(0.65, 0.55)))
})

test_that("turnover constraint is consistent", {
  expect_that(tmp_constraints$turnover_target, equals(0.6))
})

test_that("diversification constraint is consistent", {
  expect_that(tmp_constraints$div_target, equals(0.55))
})

test_that("position limit constraint is consistent", {
  expect_that(tmp_constraints$max_pos, equals(3))
  expect_that(tmp_constraints$max_pos_long, equals(2))
  expect_that(tmp_constraints$max_pos_short, equals(1))
})

test_that("return constraint is consistent", {
  expect_that(tmp_constraints$return_target, equals(0.007))
})

B <- matrix(1, ncol=1, nrow=N)
rownames(B) <- paste("Asset", 1:N, sep=".")
colnames(B) <- "factor1"

test_that("factor exposure constraint is consistent", {
  expect_that(tmp_constraints$B, equals(B))
  expect_that(tmp_constraints$lower, equals(0.9))
  expect_that(tmp_constraints$upper, equals(1.1))
})
