library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("DEoptim")

# --- Setup ---

utils::data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf <- add.objective(portfolio = init.portf, type = "risk", name = "StdDev")

# ROI (QP) - maxSR=TRUE triggers Sharpe ratio maximization
maxSR.lo.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI",
  maxSR = TRUE, trace = TRUE
)

# Relax sum constraint for stochastic methods
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01

maxSR.lo.RP <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

maxSR.lo.DE <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)

# --- Tests: init.portf structure ---

test_that("init.portf contains mean as an objective", {
  expect_equal(init.portf$objectives[[1]]$name, "mean")
})

test_that("init.portf contains StdDev as an objective", {
  expect_equal(init.portf$objectives[[2]]$name, "StdDev")
})

# --- Tests: maxSR.lo.ROI (deterministic — exact values) ---

test_that("maxSR.lo.ROI objective measure mean = 0.004357475", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$mean),
    0.004357475,
    tolerance = 1e-6
  )
})

test_that("maxSR.lo.ROI objective measure StdDev = 0.007726142", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxSR.lo.ROI)$StdDev),
    0.007726142,
    tolerance = 1e-6
  )
})

# --- Tests: maxSR.lo.RP (stochastic — numeric checks only) ---

test_that("maxSR.lo.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$mean))
})

test_that("maxSR.lo.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.RP)$StdDev))
})

# --- Tests: maxSR.lo.DE (stochastic — numeric checks only) ---

test_that("maxSR.lo.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$mean))
})

test_that("maxSR.lo.DE objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSR.lo.DE)$StdDev))
})

# --- maxSR must be read as a value, not as an expression --------------------
#
# maxSR arrives through `...` and was recovered with
# match.call(expand.dots = TRUE)$maxSR, which yields the unevaluated
# expression.  A literal TRUE happens to work; anything else does not.
# `if (maxSR)` on a symbol raises "argument is not interpretable as logical",
# so driving maxSR from a variable -- what a loop or a backtest does -- failed.

sr.portf <- add.objective(
  add.objective(
    add.constraint(
      add.constraint(portfolio.spec(assets = funds), type = "full_investment"),
      type = "long_only"),
    type = "return", name = "mean"),
  type = "risk", name = "StdDev")

test_that("maxSR accepts a variable, not only a literal TRUE", {
  flag <- TRUE
  lit <- optimize.portfolio(R = R, portfolio = sr.portf,
                            optimize_method = "ROI", maxSR = TRUE, trace = TRUE)
  var <- optimize.portfolio(R = R, portfolio = sr.portf,
                            optimize_method = "ROI", maxSR = flag, trace = TRUE)
  expect_equal(as.numeric(var$weights), as.numeric(lit$weights), tolerance = 1e-10)
})

test_that("maxSR accepts an expression that evaluates to TRUE", {
  opt <- optimize.portfolio(R = R, portfolio = sr.portf, optimize_method = "ROI",
                            maxSR = isTRUE(TRUE), trace = TRUE)
  ref <- optimize.portfolio(R = R, portfolio = sr.portf, optimize_method = "ROI",
                            maxSR = TRUE, trace = TRUE)
  expect_equal(as.numeric(opt$weights), as.numeric(ref$weights), tolerance = 1e-10)
})

test_that("a maxSR variable holding FALSE is not read as TRUE", {
  flag <- FALSE
  off <- optimize.portfolio(R = R, portfolio = sr.portf,
                            optimize_method = "ROI", maxSR = flag, trace = TRUE)
  ref <- optimize.portfolio(R = R, portfolio = sr.portf,
                            optimize_method = "ROI", maxSR = FALSE, trace = TRUE)
  expect_equal(as.numeric(off$weights), as.numeric(ref$weights), tolerance = 1e-10)
})
