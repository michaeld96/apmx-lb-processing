######################################################
# To test this: testthat::test_dir("tests/testthat") #
######################################################
library(haven)

# getting EX, PC, DM, and LB data
library(apmx)

LB <- as.data.frame(LB)

lb <- dplyr::filter(LB, LBCOMPFL == "Y")

cov_options <- c("Baseline (D1)", "Screening")

lb_vec <- c()

apmx_lab_processing <- function(lb, lb_vals, cov_option, missing_val)
{
  o2 <- FALSE
  o3 <- FALSE
  # check if lb is a dataframe
  if (!is.data.frame(lb)) {
    stop("lb must be a dataframe")
  }
  
  # checking to see if cov_option is valid
  if (is.null(cov_option)) {
    stop("cov_option must be a vector of column names, or o2, o3, or TODO")
  }
  if (is.vector(cov_option)) {
    lb <- dplyr::mutate(
        dplyr::filter(
            dplyr::filter(
                dplyr::filter(
                    LB, 
                    LBCOMPFL == "Y"
                ), 
                LBVST %in% c("Baseline (D1)", "Screening")
            ), 
            LBPARAMCD %in% c("ALB", "AST", "ALT", "BILI", "CREAT")
        ), 
        LBORRES = as.numeric(LBORRES)
    )

  }
  else if (cov_option == "o2") {
    o2 <- TRUE
  }
  else if (cov_option == "o3") {
    o3 <- TRUE
  }
  # This is for time varying 
  else {
    stop("TODO")
  }
  

  print("function is finished")
  
}

apmx_lab_processing(lb, lb_vals, cov_options, "-828")


