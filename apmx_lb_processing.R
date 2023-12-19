######################################################
# To test this: testthat::test_dir("tests/testthat") #
######################################################
library(haven)

# getting EX, PC, DM, and LB data
library(apmx)
library(dplyr)
library(tidyr)


lb <- as.data.frame(LB)

# lb <- dplyr::filter(LB, LBCOMPFL == "Y") # our function is going to do this.

lb_params <- c("ALB", "AST", "ALT", "BILI", "CREAT")

cov_options <- c("Baseline (D1)", "Screening")

lb_vec <- c()

# function that will find what row each lb_param is in
find_lb_row_pos <- function(vec_of_lb_params, lb_dataset)
{
  # find which col ends with 'U'.
    lb_vec <- c()
    for (i in 1:length(vec_of_lb_params)) {
        matches <- which(lb_dataset$LBPARAMCD == vec_of_lb_params[i])
        if (length(matches) > 0) {
            lb_vec[i] <- matches[1]
        }
        else {
            lb_vec[i] <- NA
        }
    }
    return(lb_vec)
}


apmx_lab_processing <- function(lb, lb_params, cov_option, missing_val) 
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
        # applying filters and then mutating
        lb_filtered <- dplyr::filter(LB, LBCOMPFL == "Y")
        lb_filtered <- dplyr::filter(lb_filtered, LBVST %in% cov_options)
        lb_filtered <- dplyr::filter(lb_filtered, LBPARAMCD %in% lb_params)
        lb_filtered <- dplyr::mutate(lb_filtered, LBORRES = as.numeric(LBORRES))

        # Arrange, group, filter, and ungroup
        lb_arranged <- dplyr::arrange(lb_filtered, USUBJID, LBPARAMCD, LBDT)
        lb_grouped <- dplyr::group_by(lb_arranged, USUBJID, LBPARAMCD)
        lb_selected <- dplyr::filter(lb_grouped, row_number() == max(row_number()))
        lb <- dplyr::ungroup(lb_selected)

        # Select columns and pivot wider, then mutate
        lb_selected <- dplyr::select(lb, USUBJID, LBPARAMCD, LBORRES)
        lb_wide <- tidyr::pivot_wider(lb_selected, names_from = "LBPARAMCD", values_from = "LBORRES")
        # could create a function that finds the units for each lab value
        lb_param_coords <- find_lb_row_pos(lb_params, lb)

        # find which column ends in u.
        cols_ending_in_u <- grep("U$", colnames(lb))

        # for each lb_param, find the corresponding unit
        # create a vector that will be populated with the units.
        unit_vector <- c()
        for (i in 1:length(lb_param_coords)) {
            unit_vector[i] <- lb[[lb_param_coords[i], "LBORRESU"]]
            # counter <- counter + 1 # mutate takes care of this.
        }

        lb_params_u <- c()        
          # for all lb_params, append a 'U' to the end.
        for (i in 1:length(lb_params)) {
            lb_params_u[i] <- paste0(lb_params[i], 'U')
        }

        # now that we have the units, we will mutate lb_wide with the units
        for (i in 1:length(unit_vector)) {
            # Create a symbol for the new column name
            col_name <- sym(lb_params_u[i])

            # Use the := operator to assign the value in unit_vector to the new column
            lb_wide <- mutate(lb_wide, !!col_name := unit_vector[i])
        }
        
        return(lb_wide)

    } 
    else if (cov_option == "o2") {
        o2 <- TRUE
    } 
    else if (cov_option == "o3") {
        o3 <- TRUE
    } 
    else {
        stop("TODO")
    }
}


result <- apmx_lab_processing(lb, lb_params, cov_options, "-828")




