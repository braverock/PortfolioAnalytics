###############################################################################
# tests/testthat/test-factor-model-wide.R
#
# statistical.factor.model() refuses a panel with fewer observations than
# assets. That restriction is the default and stays the default: a short, wide
# panel is where sample second moments are least trustworthy.
#
# allow.fewer.obs = TRUE lifts it, for the case the asymptotic principal
# components literature is built for. These tests pin both sides: that the
# default is unchanged in every respect, and that the opt-in path is bounded
# where the arithmetic requires it.
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)

utils::data(edhec)
R <- edhec[, 1:8]                       # 8 assets, plenty of months
wide <- R[1:5, ]                        # 5 observations, 8 assets: m < N

test_that("the default refuses a wide panel, with the message it always had", {
  expect_error(statistical.factor.model(wide, k = 2),
               "fewer observations than assets")
  expect_error(statistical.factor.model(wide, k = 2, allow.fewer.obs = FALSE),
               "fewer observations than assets")
})

test_that("allow.fewer.obs = TRUE fits the wide panel", {
  expect_no_error(statistical.factor.model(wide, k = 2, allow.fewer.obs = TRUE))
})

test_that("the wide fit produces a covariance of the right shape and rank", {
  S <- extractCovariance(
    statistical.factor.model(wide, k = 2, allow.fewer.obs = TRUE))
  expect_equal(dim(S), c(ncol(wide), ncol(wide)))
  expect_equal(qr(S)$rank, ncol(wide))
  expect_gt(min(eigen(S, symmetric = TRUE, only.values = TRUE)$values), 0)
})

test_that("on the opt-in path k is bounded by m - 2", {
  m <- nrow(wide)
  # extractCovariance() divides by m - k - 1, so k = m - 1 would return an
  # infinite covariance instead of an error. The asset count does not enter:
  # on this path there are more assets than observations.
  expect_no_error(statistical.factor.model(wide, k = m - 2L,
                                           allow.fewer.obs = TRUE))
  expect_error(statistical.factor.model(wide, k = m - 1L,
                                        allow.fewer.obs = TRUE),
               "requests more factors than the data supports")
  expect_error(statistical.factor.model(wide, k = 99L, allow.fewer.obs = TRUE),
               "requests more factors than the data supports")
})

test_that("every k the bound admits yields a finite covariance", {
  # The bound exists for this reason, so check the reason and not only the
  # boundary.
  for (k in seq_len(nrow(wide) - 2L)) {
    S <- extractCovariance(
      statistical.factor.model(wide, k = k, allow.fewer.obs = TRUE))
    expect_true(all(is.finite(S)), info = paste("k =", k))
    expect_gt(min(diag(S)), 0)
  }
})

test_that("the message says what was asked for and what was available", {
  msg <- tryCatch(
    statistical.factor.model(wide, k = 99L, allow.fewer.obs = TRUE),
    error = function(e) conditionMessage(e))
  expect_match(msg, "k = 99")
  expect_match(msg, as.character(nrow(wide)))
  expect_match(msg, as.character(ncol(wide)))
})

test_that("too few observations for any factor are refused", {
  # m - 2 < 1 leaves no admissible k at all.
  expect_error(statistical.factor.model(R[1, ], k = 1, allow.fewer.obs = TRUE),
               "at least three observations")
  expect_error(statistical.factor.model(R[1:2, ], k = 1,
                                        allow.fewer.obs = TRUE),
               "at least three observations")
  expect_no_error(statistical.factor.model(R[1:3, ], k = 1,
                                           allow.fewer.obs = TRUE))
})

test_that("k <= 0 is still refused, and before the new check", {
  expect_error(statistical.factor.model(wide, k = 0, allow.fewer.obs = TRUE),
               "positive integer")
  expect_error(statistical.factor.model(wide, k = -1, allow.fewer.obs = TRUE),
               "positive integer")
})

test_that("the argument must be named, so nothing positional changes meaning", {
  # allow.fewer.obs follows ..., so a third positional argument still reaches
  # prcomp() as it always did.
  expect_error(statistical.factor.model(wide, 2, TRUE),
               "fewer observations than assets")
})

test_that("a tall panel is untouched, with the option set either way", {
  tall <- R[1:60, ]
  a <- extractCovariance(statistical.factor.model(tall, k = 3))
  b <- extractCovariance(statistical.factor.model(tall, k = 3,
                                                  allow.fewer.obs = TRUE))
  expect_equal(as.numeric(a), as.numeric(b))

  # Reproduce the decomposition independently.
  x     <- zoo::coredata(tall)
  pc    <- prcomp(x)
  betas <- pc$rotation[, 1:3, drop = FALSE]
  f     <- x %*% betas
  resid <- x - f %*% t(betas)
  resid <- sweep(resid, 2, colMeans(resid))
  # extractCovariance() divides the residual sums of squares by m - k - 1,
  # not by m - 1, so var() is not the right denominator here.
  stock_m2 <- colSums(resid^2) / (nrow(x) - 3L - 1L)
  S_ref <- betas %*% stats::cov(f) %*% t(betas) + diag(stock_m2)

  expect_equal(as.numeric(a), as.numeric(S_ref), tolerance = 1e-8)
})

test_that("the square case is untouched", {
  # m = N is not the wide path, so nothing here changes: a full basis
  # reconstructs the data and the covariance comes back finite, as before.
  square <- R[seq_len(ncol(R)), ]       # m = N = 8
  S <- extractCovariance(statistical.factor.model(square, k = ncol(square)))
  expect_true(all(is.finite(S)))
  expect_equal(as.numeric(S),
               as.numeric(stats::cov(zoo::coredata(square))),
               tolerance = 1e-12)
})
