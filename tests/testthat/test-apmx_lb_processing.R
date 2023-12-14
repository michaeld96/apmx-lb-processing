# source("apmx_lb_processing.R")
# library(apmx)

test_that("null value for cov_option", {

  LB <- as.data.frame(LB)

  lb <- dplyr::filter(LB, LBCOMPFL == "Y")

  cov_options <- c("Baseline (D1)", "Screening")

  lb_vec <- c()

  cov_null_value <- NULL

  expect_error(apmx_lab_processing(lb, lb_vals, cov_null_value,"-828"), 
               "cov_option must be a vector of column names, or o2, o3, or TODO")
})
