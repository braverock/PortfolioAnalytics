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

# --- Correctness tests: maxSR must return the tangency portfolio ----------
#
# These are correctness tests, not regression tests.  The portfolio returned
# by maxSR=TRUE is compared against
#   (a) the analytic long-only tangency portfolio, and
#   (b) every point of the long-only mean-variance frontier,
# both solved independently with quadprog.  They are run for the plain call
# (moments come from colMeans(R)) and for a momentFUN that supplies excess
# return moments, which is the only way to give the ROI maxSR path a
# non-zero risk-free rate.

skip_if_not_installed("quadprog")

# Analytic long-only tangency portfolio:
#   min w'Sw  s.t.  mu'w = 1, w >= 0,  then rescale so that sum(w) = 1.
lo_tangency <- function(mu, sigma) {
  N <- length(mu)
  sol <- quadprog::solve.QP(
    Dmat = 2 * sigma, dvec = rep(0, N),
    Amat = cbind(mu, diag(N)), bvec = c(1, rep(0, N)), meq = 1
  )
  w <- sol$solution / sum(sol$solution)
  names(w) <- names(mu)
  w
}

# Highest Sharpe ratio attainable on the long-only, fully invested
# mean-variance frontier, solved on a grid of target returns.
lo_frontier_max_sr <- function(mu, sigma, n = 200) {
  N <- length(mu)
  targets <- seq(min(mu), max(mu), length.out = n + 2)
  targets <- targets[-c(1, n + 2)]
  srs <- vapply(targets, function(tg) {
    sol <- try(quadprog::solve.QP(
      Dmat = 2 * sigma, dvec = rep(0, N),
      Amat = cbind(mu, rep(1, N), diag(N)),
      bvec = c(tg, 1, rep(0, N)), meq = 2
    ), silent = TRUE)
    if (inherits(sol, "try-error")) return(NA_real_)
    w <- sol$solution
    sum(w * mu) / sqrt(sum(crossprod(w, sigma) * w))
  }, numeric(1))
  max(srs, na.rm = TRUE)
}

port_sr <- function(w, mu, sigma) {
  sum(w * mu) / sqrt(sum(crossprod(w, sigma) * w))
}

mv.portf <- portfolio.spec(assets = funds)
mv.portf <- add.constraint(portfolio = mv.portf, type = "full_investment")
mv.portf <- add.constraint(portfolio = mv.portf, type = "long_only")
mv.portf <- add.objective(portfolio = mv.portf, type = "return", name = "mean")
mv.portf <- add.objective(portfolio = mv.portf, type = "risk", name = "StdDev")

sample.mu <- colMeans(R)
sample.sigma <- cov(R)

test_that("gmv_opt rejects a target_mean of the wrong type or length", {
  cn <- PortfolioAnalytics:::get_constraints(mv.portf)
  m  <- list(mean = sample.mu, var = sample.sigma)
  call_it <- function(tm) PortfolioAnalytics:::gmv_opt(
    R = R, constraints = cn, moments = m, lambda = 1,
    target = as.numeric(sample.mu %*% rep(1 / length(funds), length(funds))),
    lambda_hhi = NULL, conc_groups = NULL, solver = "quadprog",
    target_mean = tm)

  expect_error(call_it(sample.mu[1:2]), "one entry per asset")
  expect_error(call_it(as.character(sample.mu)), "one entry per asset")
  expect_no_error(call_it(sample.mu))
})

test_that("maxSR without momentFUN returns the long-only tangency portfolio", {
  opt <- optimize.portfolio(
    R = R, portfolio = mv.portf,
    optimize_method = "ROI", maxSR = TRUE, trace = TRUE
  )
  w <- opt$weights
  expect_false(anyNA(w))
  expect_equal(sum(w), 1, tolerance = 1e-6)

  w.tan <- lo_tangency(sample.mu, sample.sigma)
  expect_equal(as.numeric(w), as.numeric(w.tan), tolerance = 1e-4)

  sr.opt <- port_sr(w, sample.mu, sample.sigma)
  expect_gte(sr.opt, lo_frontier_max_sr(sample.mu, sample.sigma) - 1e-6)
})

# A momentFUN supplying excess-return moments must be honoured by every stage
# of the maxSR solve.  Two risk-free rates are used: 0.001 previously returned
# a feasible but badly sub-optimal portfolio, 0.0025 previously returned NA
# weights without any warning.
for (rf in c(0.001, 0.0025)) {
  local({
    rf.local <- rf
    excess.mu <- sample.mu - rf.local
    moment.fun <- function(R, portfolio, ...) {
      list(mu = colMeans(R) - rf.local, sigma = cov(R))
    }

    test_that(paste0("maxSR with excess-return momentFUN (rf = ", rf.local,
                     ") returns the long-only tangency portfolio"), {
      opt <- optimize.portfolio(
        R = R, portfolio = mv.portf,
        optimize_method = "ROI", maxSR = TRUE,
        momentFUN = moment.fun, trace = TRUE
      )
      w <- opt$weights
      expect_false(anyNA(w))
      expect_equal(sum(w), 1, tolerance = 1e-6)

      w.tan <- lo_tangency(excess.mu, sample.sigma)
      expect_equal(as.numeric(w), as.numeric(w.tan), tolerance = 1e-4)

      sr.opt <- port_sr(w, excess.mu, sample.sigma)
      expect_gte(sr.opt, lo_frontier_max_sr(excess.mu, sample.sigma) - 1e-6)

      # the reported mean must be the excess mean supplied by momentFUN
      expect_equal(
        as.numeric(extractObjectiveMeasures(opt)$mean),
        sum(w * excess.mu),
        tolerance = 1e-8
      )
    })
  })
}

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

test_that("exact full investment solves on every rolling window it used to fail on", {
  win <- 18L
  fi.portf <- add.objective(
    add.constraint(add.constraint(portfolio.spec(assets = colnames(edhec)),
                                  type = "full_investment"),
                   type = "long_only"),
    type = "risk", name = "var")
  starts <- seq(1L, 80L, by = 1L)
  bad <- vapply(starts, function(i) {
    o <- try(optimize.portfolio(R = edhec[i:(i + win - 1L), ], portfolio = fi.portf,
                                optimize_method = "ROI"), silent = TRUE)
    inherits(o, "try-error") || anyNA(extractWeights(o))
  }, logical(1))
  expect_equal(sum(bad), 0L)
})
