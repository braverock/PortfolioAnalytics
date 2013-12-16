
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_efficient_frontier.R", package="PortfolioAnalytics"))

context("mean-var efficient frontier")

test_that("meanvar.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanvar.ef$frontier), 25) })

test_that("colnames(meanvar.ef$frontier) are consistent", 
          { expect_equal(colnames(meanvar.ef$frontier), c("mean", "StdDev", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")) })

test_that("first row of meanvar.ef$frontier is consistent", 
          { expect_equal(as.numeric(meanvar.ef$frontier[1,]), c(0.006765658, 0.01334460, 178.0782, 0.15, 0.15, 0.15, 0.15, 0.4), 
                         tolerance=1e-6) })

test_that("last row of meanvar.ef$frontier is consistent", 
          { expect_equal(as.numeric(meanvar.ef$frontier[25,]), c(0.007326513, 0.02070151, 428.5526, 0.15, 0.15, 0.15, 0.4, 0.15),
                         tolerance=1e-6) })

context("mean-etl efficient frontier")

test_that("meanetl.ef$frontier has 25 rows", 
          { expect_equal(nrow(meanetl.ef$frontier), 25) })

test_that("colnames(meanetl.ef$frontier) are consistent", 
          { expect_equal(colnames(meanetl.ef$frontier), c("mean", "ES", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")) })

test_that("first row of meanetl.ef$frontier is consistent", 
          { expect_equal(as.numeric(meanetl.ef$frontier[1,]), c(0.006887368, 0.02637039, 0.02637039, 0.15, 0.4, 0.15, 0.15, 0.15),
                         tolerance=1e-6) })

test_that("last row of meanetl.ef$frontier is consistent", 
          { expect_equal(as.numeric(meanetl.ef$frontier[25,]), c(0.007326513, 0.04642908, 0.04642908, 0.15, 0.15, 0.15, 0.4, 0.15),
                         tolerance=1e-6) })
