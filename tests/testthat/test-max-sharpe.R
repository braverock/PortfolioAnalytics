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

# --- An exact full investment constraint must not be degenerate -------------
#
# gmv_opt() used to encode min_sum == max_sum as two opposing inequality rows
# (sum(w) >= 1 and -sum(w) >= -1).  Both are always active and linearly
# dependent, which makes quadprog's active set rank deficient; it then fails
# with "constraints are inconsistent, no solution!" on problems whose entire
# constraint set is sum(w) = 1 and 0 <= w <= 1.  ROI does not propagate that
# error, it returns a solution of NAs, so optimize.portfolio silently produced
# an all-NA weight vector.

test_that("exact full investment solves and matches quadprog (min variance)", {
  Rw <- edhec[3:20, ]  # 18 months, 13 series: a window that used to fail
  fi.portf <- add.objective(
    add.constraint(add.constraint(portfolio.spec(assets = colnames(Rw)),
                                  type = "full_investment"),
                   type = "long_only"),
    type = "risk", name = "var")
  cn <- PortfolioAnalytics:::get_constraints(fi.portf)
  expect_equal(cn$min_sum, cn$max_sum)

  opt <- optimize.portfolio(R = Rw, portfolio = fi.portf, optimize_method = "ROI")
  w <- as.numeric(extractWeights(opt))
  expect_false(anyNA(w))
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_gte(min(w), -1e-8)

  N <- ncol(Rw)
  ref <- quadprog::solve.QP(
    Dmat = 2 * cov(Rw), dvec = rep(0, N),
    Amat = cbind(rep(1, N), diag(N)), bvec = c(1, rep(0, N)), meq = 1)$solution
  expect_equal(w, ref, tolerance = 1e-6)
})

test_that("a tight but unequal weight sum stays an interval", {
  # The equality encoding must fire only when the two bounds are the same
  # number. A caller who deliberately allows sum(w) in [1 - 1e-9, 1 + 1e-9]
  # is asking for an interval and must keep it; all.equal() would have
  # collapsed this one.
  eps <- 1e-9
  wide.portf <- add.objective(
    add.constraint(add.constraint(portfolio.spec(assets = funds),
                                  type = "weight_sum",
                                  min_sum = 1 - eps, max_sum = 1 + eps),
                   type = "long_only"),
    type = "risk", name = "var")
  cn <- PortfolioAnalytics:::get_constraints(wide.portf)
  expect_false(identical(cn$min_sum, cn$max_sum))

  opt <- optimize.portfolio(R = R, portfolio = wide.portf, optimize_method = "ROI")
  w <- as.numeric(extractWeights(opt))
  expect_false(anyNA(w))
  expect_gte(sum(w), 1 - eps - 1e-10)
  expect_lte(sum(w), 1 + eps + 1e-10)
})

test_that("exact full investment solves on every rolling window it used to fail on", {
  win <- 18L
  fi.portf <- add.objective(
    add.constraint(add.constraint(portfolio.spec(assets = colnames(edhec)),
                                  type = "full_investment"),
                   type = "long_only"),
    type = "risk", name = "var")
  # Derive the range from the data rather than hard-coding it, so the test
  # cannot run past the end if edhec ever changes length, and so it covers
  # every window of this width.
  starts <- seq_len(nrow(edhec) - win + 1L)
  bad <- vapply(starts, function(i) {
    o <- try(optimize.portfolio(R = edhec[i:(i + win - 1L), ], portfolio = fi.portf,
                                optimize_method = "ROI"), silent = TRUE)
    inherits(o, "try-error") || anyNA(extractWeights(o))
  }, logical(1))
  expect_equal(sum(bad), 0L)
})
