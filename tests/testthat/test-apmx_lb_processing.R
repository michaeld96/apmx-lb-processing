# source("apmx_lb_processing.R")
# library(apmx)

test_that("null value for cov_option", {

  LB <- as.data.frame(LB)

  lb <- dplyr::filter(LB, LBCOMPFL == "Y")

  cov_options <- c("Baseline (D1)", "Screening")

  lb_vec <- c()

  cov_null_value <- NULL

  expect_error(apmx_lab_processing(lb, lb_vals, cov_null_value,"-828"), 
               "cov_option must be a vector of baseline dates, o2, o3, or, o4.")
})

######################################################
# TODO:                                              #
# 1. Add test for cov_option = "o2"                  #
# 2. Add test for cov_option = "o3"                  #
# 3. Add test for cov_option = "o4"                  #
# 4. Test calculations from R script to match this   #
#    for Baseline and Screening.                     #
######################################################