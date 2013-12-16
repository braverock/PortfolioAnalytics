
##### Load packages #####
require(testthat)
require(PortfolioAnalytics)

##### Source Demo Script #####
source(system.file("demo/demo_return_target.R", package="PortfolioAnalytics"))


context("target return as an objective")

test_that("ret.obj.portf contains mean as an objective", 
          { expect_that(ret.obj.portf$objectives[[1]]$name == "mean", is_true()) })

test_that("ret.obj.portf contains mean as an objective with target = 0.007", 
          { expect_equal(ret.obj.portf$objectives[[1]]$target, 0.007) })

test_that("ret.obj.opt objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(ret.obj.opt)$mean), 0.007, tolerance=0.0001) })

test_that("opt.obj.de objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(opt.obj.de)$mean), 0.007, tolerance=0.0001) })

test_that("opt.obj.rp objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(opt.obj.rp)$mean), 0.007, tolerance=0.0001) })

context("target return as a constraint")

test_that("ret.obj.portf contains target return as a constraint", 
          { expect_that(ret.constr.portf$constraints[[3]]$type == "return", is_true()) })

test_that("ret.obj.portf contains mean as a constraint with target = 0.007", 
          { expect_equal(ret.constr.portf$constraints[[3]]$return_target, 0.007) })

test_that("ret.constr.opt objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(ret.constr.opt)$mean), 0.007, tolerance=0.0001) })

test_that("opt.constr.de objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(opt.constr.de)$mean), 0.007, tolerance=0.0001) })

test_that("opt.constr.rp objective measure mean = 0.007", 
          { expect_equal(as.numeric(extractObjectiveMeasures(opt.constr.rp)$mean), 0.007, tolerance=0.0001) })


