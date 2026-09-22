##### Rebalancing periods that fail must not poison the whole result #####
#
# optimize.portfolio.rebalancing() runs its foreach loops with
# .errorhandling = "pass", so a period whose optimisation raises does not abort
# the run: the condition object is stored in the result list in place of the
# portfolio. Nothing downstream expected that. extractWeights() built its result
# matrix from element [[1]] and then assigned unlist(x$weights) row by row, which
# is NULL for a failed period, so the whole object became unusable:
#
#   Error in result[i, ] <- unlist(rebal_object[[i]]$weights) :
#     number of items to replace is not a multiple of replacement length
#
# The failure is silent as well as fatal: the condition is raised inside the
# loop and never reaches the user.

skip_on_cran()
library(PortfolioAnalytics)

utils::data(edhec)
R <- edhec[, 1:4]
funds <- colnames(R)

spec <- add.objective(
  add.constraint(
    add.constraint(portfolio.spec(assets = funds), type = "full_investment"),
    type = "long_only"),
  type = "risk", name = "var")

# A momentFUN that raises on selected windows. It stands in for anything that
# can fail for one period and not another: a solver, an estimator, a data gap.
fail_in_month <- function(months) {
  force(months)
  function(R, portfolio) {
    if (as.integer(format(zoo::index(R)[nrow(R)], "%m")) %in% months)
      stop("moment estimation failed for this window")
    list(mu = colMeans(R), sigma = cov(R), m3 = NULL, m4 = NULL,
         mean = colMeans(R), var = cov(R))
  }
}

run <- function(momentFUN, training = 24, window = 24) {
  suppressWarnings(optimize.portfolio.rebalancing(
    R = R, portfolio = spec, optimize_method = "ROI",
    rebalance_on = "months", training_period = training,
    rolling_window = window, momentFUN = momentFUN))
}

test_that("a failing period does not abort the run", {
  skip_if_not_installed("ROI.plugin.quadprog")

  flaky <<- fail_in_month(c(3L, 9L))
  opt <- run("flaky")
  expect_s3_class(opt, "optimize.portfolio.rebalancing")

  failed <- vapply(opt$opt_rebalancing,
                   function(x) !inherits(x, "optimize.portfolio"), logical(1))
  expect_gt(sum(failed), 0)
})

test_that("the failing periods are reported rather than passed over in silence", {
  skip_if_not_installed("ROI.plugin.quadprog")

  flaky <<- fail_in_month(c(3L, 9L))
  expect_warning(
    optimize.portfolio.rebalancing(
      R = R, portfolio = spec, optimize_method = "ROI",
      rebalance_on = "months", training_period = 24,
      rolling_window = 24, momentFUN = "flaky"),
    "did not produce a portfolio")
})

test_that("extractWeights returns NA for the failed periods and works otherwise", {
  skip_if_not_installed("ROI.plugin.quadprog")

  flaky <<- fail_in_month(c(3L, 9L))
  opt <- run("flaky")
  failed <- vapply(opt$opt_rebalancing,
                   function(x) !inherits(x, "optimize.portfolio"), logical(1))

  w <- extractWeights(opt)
  expect_equal(nrow(w), length(opt$opt_rebalancing))
  expect_equal(ncol(w), length(funds))

  # NA exactly where the period failed, and nowhere else.
  na_rows <- apply(is.na(w), 1, any)
  expect_equal(unname(na_rows), unname(failed))

  # The periods that did solve are untouched: long only and fully invested.
  ok <- w[!na_rows, , drop = FALSE]
  expect_true(all(ok >= -1e-8))
  expect_true(all(abs(rowSums(ok) - 1) < 1e-6))
})

test_that("a failure in the very first period is handled too", {
  skip_if_not_installed("ROI.plugin.quadprog")

  # The shape of the result used to be read off element [[1]], so the first
  # period failing was the worst case. Find which month that is rather than
  # guessing, then make exactly that month fail.
  clean <<- fail_in_month(integer(0))
  first_month <- as.integer(format(
    as.Date(names(run("clean")$opt_rebalancing)[1]), "%m"))

  flaky_first <<- fail_in_month(first_month)
  opt <- run("flaky_first")
  w <- extractWeights(opt)
  expect_equal(ncol(w), length(funds))
  expect_true(all(is.na(w[1, ])))
  expect_false(all(is.na(w)))
})

test_that("extractObjectiveMeasures survives a failed period", {
  skip_if_not_installed("ROI.plugin.quadprog")

  flaky <<- fail_in_month(c(3L, 9L))
  opt <- run("flaky")
  om <- extractObjectiveMeasures(opt)
  expect_equal(nrow(om), length(opt$opt_rebalancing))
  expect_true(any(is.na(om)))
  expect_false(all(is.na(om)))
})

test_that("extractStats returns NULL for a failed period rather than failing", {
  skip_if_not_installed("ROI.plugin.quadprog")

  flaky <<- fail_in_month(c(3L, 9L))
  opt <- run("flaky")
  st <- extractStats(opt)
  expect_equal(length(st), length(opt$opt_rebalancing))
  expect_true(any(vapply(st, is.null, logical(1))))
  expect_true(any(!vapply(st, is.null, logical(1))))
})

test_that("a run in which every period fails says so plainly", {
  skip_if_not_installed("ROI.plugin.quadprog")

  always <<- function(R, portfolio) stop("always fails")
  opt <- run("always")
  expect_error(extractWeights(opt), "every rebalancing period failed")
})

test_that("a period that returns NA weights is reported too, not only a raised error", {
  skip_if_not_installed("ROI.plugin.quadprog")

  # ROI signals solver failure by returning a solution of NAs rather than by
  # raising, so this path is silent on both counts: no condition, and nothing
  # for the loop to catch. A singular covariance matrix is the ordinary way to
  # get there -- more assets than observations, or a redundant series.
  singular_in_month <<- function(R, portfolio) {
    S <- cov(R)
    if (as.integer(format(zoo::index(R)[nrow(R)], "%m")) %in% c(6L, 12L)) {
      S[, 2] <- S[, 1]
      S[2, ] <- S[1, ]
    }
    list(mu = colMeans(R), sigma = S, m3 = NULL, m4 = NULL,
         mean = colMeans(R), var = S)
  }

  expect_warning(
    optimize.portfolio.rebalancing(
      R = R, portfolio = spec, optimize_method = "ROI",
      rebalance_on = "months", training_period = 24,
      rolling_window = 24, momentFUN = "singular_in_month"),
    "did not produce a portfolio")

  # Nothing was raised: every period is still an optimize.portfolio object.
  opt <- run("singular_in_month")
  expect_true(all(vapply(opt$opt_rebalancing,
                         function(x) inherits(x, "optimize.portfolio"), logical(1))))
  w <- extractWeights(opt)
  expect_true(any(is.na(w)))
  expect_false(all(is.na(w)))
})

test_that("the warning says what went wrong, not only that something did", {
  skip_if_not_installed("ROI.plugin.quadprog")

  # A singular covariance is refused by quadprog for a specific, nameable
  # reason. ROI puts it in its status message and gmv_opt() keeps it, so the
  # warning can pass it on instead of saying "no solution".
  w <- tryCatch(
    optimize.portfolio.rebalancing(
      R = R, portfolio = spec, optimize_method = "ROI",
      rebalance_on = "months", training_period = 24,
      rolling_window = 24, momentFUN = "singular_in_month"),
    warning = function(w) conditionMessage(w))

  expect_type(w, "character")
  expect_match(w, "Reason:")
  expect_match(w, "positive definite")
  expect_match(w, "Periods:")
})

test_that("a single optimize.portfolio call keeps the solver status too", {
  skip_if_not_installed("ROI.plugin.quadprog")

  S <- cov(R[1:36, ])
  S[, 2] <- S[, 1]
  S[2, ] <- S[1, ]
  singular_moments <<- function(R, portfolio)
    list(mu = colMeans(R), sigma = S, m3 = NULL, m4 = NULL,
         mean = colMeans(R), var = S)

  opt <- optimize.portfolio(R[1:36, ], spec, optimize_method = "ROI",
                            momentFUN = "singular_moments")
  expect_true(all(is.na(extractWeights(opt))))
  expect_false(is.null(opt$solver_status))
  expect_match(opt$solver_status, "positive definite")
})

test_that("a reason arriving with CRLF does not break the warning across lines", {
  # The reason is pasted into a one-line message, so any newline a solver puts
  # in its status text has to be folded away -- carriage returns included.
  fold <- PortfolioAnalytics:::warn.failed.periods
  fake <- list(a = simpleError("first line
second line"),
               b = structure(list(weights = c(x = 0.5, y = 0.5)),
                             class = "optimize.portfolio"))
  msg <- tryCatch(fold(fake), warning = function(w) conditionMessage(w))
  expect_type(msg, "character")
  expect_match(msg, "first line second line")
  expect_false(grepl("first line", msg))
})
