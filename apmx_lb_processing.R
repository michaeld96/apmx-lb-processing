######################################################
# To test this: testthat::test_dir("tests/testthat") #
######################################################

######################################################
# NOTES:                                             #
# 1. I am going to need to make a dupe for ADAM      #
# data sets if I cannot generalize what I am writing #
# now. Example, for LBPARAMCD, in an ADAM data set   #
# this may be labeled differently.                   #
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

# HELPER FUNCTIONS
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

# function will take in lab parameters and return a vector of the same parameters with a 'U' appended to the end.
lb_params_u_appended <- function(lb_params)
{
    lb_params_u <- c()        
    for (i in 1:length(lb_params)) {
        lb_params_u[i] <- paste0(lb_params[i], 'U')
    }
    return(lb_params_u)
}

# Returns the units assoicated with lb parameters.
# lb - data frame.
# lb_param_coords - row that lb parameter is located.
lb_params_gen_unit_vector <- function(lb, lb_param_coords)
{
    unit_vector <- c()
    for (i in 1:length(lb_param_coords)) {
        unit_vector[i] <- lb[[lb_param_coords[i], "LBORRESU"]]
    }
    
    return(unit_vector)
}

lb_params_append_df <- function(lb_wide, lb_params_u, unit_vector)
{
    for (i in 1:length(unit_vector)) {
        # create a symbol for the new column name
        col_name <- sym(lb_params_u[i])

        # use the := operator to assign the value in unit_vector to the new column
        lb_wide <- mutate(lb_wide, !!col_name := unit_vector[i])
    }
    return(lb_wide)
}

# TODO: TEST!
warn_missing_data <- function(df) 
{
    # Check if 'USUBJID' is a column in the dataframe
    if (!"USUBJID" %in% names(df)) {
        stop("The dataframe does not have a 'USUBJID' column.")
    }
    
    # Iterate over each unique USUBJID
    for (usubjid in unique(df$USUBJID)) 
    {
        # Subset the dataframe for the current USUBJID
        df_subset <- df[df$USUBJID == usubjid, ]
        
        # Check each column for missing values
        for (col in names(df_subset)) 
        {
            if (any(is.na(df_subset[[col]]))) {
                warning(paste("USUBJID", usubjid, "is missing data in column", col))
            }
        }
    }
}

# Check unit column to see if each entry is the same.
check_units <- function(df, params, unit_vector)
{
    # For every column that starts with a 'U' we are going to see if the first entry matches all of the entries.
    
    # Parameters are the columns. So what we're going to do is
    # for each parameter we are going to see what row it's in "LBPARAM"
    # and for each spot we are going to check for "LOBBREU" and make sure that the 
    # first entry, the unit, is the same for each "LBPARAM"

    diff_unit_vars <- c()

    pos_unit_vector <- 1
    for (param in params) {
        logical_vector <- df$LBPARAMCD == param
        row_positions <- which(logical_vector)
        for (pos in row_positions) {
            if (df$LBORRESU[pos] != unit_vector[pos_unit_vector]) {
                if (!(param %in% diff_unit_vars)) {
                    diff_unit_vars <- append(diff_unit_vars, param)
                }
            }
        }
        pos_unit_vector <- pos_unit_vector + 1
    }

    if (length(diff_unit_vars) != 0) {
        warning("The following variables do not have the same units: ", paste(diff_unit_vars, collapse = ", "))
    }
}



apmx_lab_processing <- function(lb, lb_params, cov_option, missing_val = -999) 
{

    # check if lb is a dataframe
    if (!is.data.frame(lb)) {
        stop("lb must be a dataframe")
    }

    # checking to see if cov_option is valid
    if (is.null(cov_option)) {
        stop("cov_option must be a vector of baseline dates, o2, o3, or, o4.")
    }

    # check if any subject is missing data.
    # TODO: TEST
    # warn_missing_data(lb)

    if (length(cov_option) > 1) {
        # applying filters and then mutating
        # ASK: would LBCOMPFL always need to be Y? (Thinking that this is complete.)
        lb_filtered <- dplyr::filter(LB, LBCOMPFL == "Y")
        lb_filtered <- dplyr::filter(lb_filtered, LBVST %in% cov_options)
        lb_filtered <- dplyr::filter(lb_filtered, LBPARAMCD %in% lb_params)
        lb_filtered <- dplyr::mutate(lb_filtered, LBORRES = as.numeric(LBORRES))

        # Arrange, group, filter, and ungroup
        # ASK: select the lab collected immediately prior to first dose
        # ASK: Is the workflow below the same for all data collected?
        lb_arranged <- dplyr::arrange(lb_filtered, USUBJID, LBPARAMCD, LBDT)
        lb_grouped <- dplyr::group_by(lb_arranged, USUBJID, LBPARAMCD)
        lb_selected <- dplyr::filter(lb_grouped, row_number() == max(row_number()))
        lb <- dplyr::ungroup(lb_selected)

        # Select columns and pivot wider, then mutate
        # ASK: For select here, will these variables vary?
        lb_selected <- dplyr::select(lb, USUBJID, LBPARAMCD, LBORRES)
        lb_wide <- tidyr::pivot_wider(lb_selected, names_from = "LBPARAMCD", values_from = "LBORRES")
        # could create a function that finds the units for each lab value
        lb_param_coords <- find_lb_row_pos(lb_params, lb)

        # find which column ends in u.
        # cols_ending_in_u <- grep("U$", colnames(lb))

        # for each lb_param, find the corresponding unit.
        # create a vector that will be populated with the units.
        unit_vector <- lb_params_gen_unit_vector(lb, lb_param_coords)

        # Checking to see if all units are the same for the variables.
        check_units(lb, lb_params, unit_vector)

        # for all lb_params, append a 'U' to the end.
        lb_params_u <- lb_params_u_appended(lb_params)

        # now that we have the units, we will mutate lb_wide with the units
        lb_wide <- lb_params_append_df(lb_wide, lb_params_u, unit_vector)
        
        return(lb_wide)

    }
    else if (cov_option == "o2") {
        stop("o3 is not yet implemented.")
    }
    else if (cov_option == "o3") {
        stop("o3 is not yet implemented.")
    }
    # time-varying.
    # ASK: Would this be better named as "time-varying" or something like 't'?
    else if (cov_option == "o4") {
        # first, going to filter data.
        lb_filtered <- dplyr::filter(LB, LBCOMPFL == "Y")
        lb_filtered <- dplyr::filter(lb_filtered, LBPARAMCD %in% lb_params)
        lb_filtered <- dplyr::mutate(lb_filtered, LBORRES = as.numeric(LBORRES))

        # now we are going to select USUBJID
        # ASK: Is this alway going to be the case??
        lb_param_coords <- find_lb_row_pos(lb_params, lb)
        unit_vector <- lb_params_gen_unit_vector(lb, lb_param_coords)
        lb_params_u <- lb_params_u_appended(lb_params)
        lb_filtered <- dplyr::select(lb_filtered, USUBJID, DTIM = LBDT, !!lb_params := LBORRES)
        lb_appended_with_u <- lb_params_append_df(lb_filtered, lb_params_u, unit_vector)
        return(lb_appended_with_u)
    }
    else {
        stop("cov_option must be a vector of baseline dates, o2, o3, or, o4.")
    }
}


result <- apmx_lab_processing(lb, lb_params, cov_options, "-828")

# seeing tast.
# lb <- as.data.frame(LB)

# tast <- apmx_lab_processing(lb, "AST", "o4", "-111")

# talt <- apmx_lab_processing(lb, "ALT", "o4", "-111")

# testing the removal a cell.
# lb[6,4] <- NA

# apmx_lab_processing(lb, lb_params, cov_options, "-828")