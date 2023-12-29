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

lb_params <- c("ALB", "AST", "ALT", "BILI", "CREAT")

cov_options <- c("Baseline (D1)", "Screening")

lb_vec <- c()

# HELPER FUNCTIONS
# function that will find what row each lb_param is in
find_lb_row_pos <- function(vec_of_lb_params, lb_dataset)
{
  # Initialize a list to store the positions
  lb_positions_list <- vector("list", length(vec_of_lb_params))

  # Loop through the vector of lab parameters
  for (i in seq_along(vec_of_lb_params)) {
    # Find the indices of all matches for the current lab parameter
    lb_positions_list[[i]] <- which(lb_dataset$LBPARAMCD == vec_of_lb_params[i])
  }

  # Set the names of the list elements to the lab parameters
  names(lb_positions_list) <- vec_of_lb_params

  return(lb_positions_list)
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

# Returns the units associated with lb parameters.
# lb - data frame.
# lb_param_coords - row that lb parameter is located.
lb_params_gen_unit_vector <- function(lb, lb_param_coords, lb_params)
{
    unit_vector <- list()

    for (param in lb_params) {
        coords <- lb_param_coords[[param]]   

        # Initialize an empty vector to store the units
        units_for_param <- vector("character", length(coords))

        for (j in 1:length(coords)) {
            units_for_param[j] <- lb[[coords[j], "LBORRESU"]]
        }

        # Assign the collected units to the corresponding parameter in the unit_vector list
        unit_vector[[param]] <- units_for_param
    }
    
    return(unit_vector)
}

lb_params_append_df <- function(lb_wide, lb_params_u, unit_vector)
{
    for (i in 1:length(unit_vector)) {
        # create a symbol for the new column name
        col_name <- sym(lb_params_u[i])

        # use the := operator to assign the value in unit_vector to the new column
        lb_wide <- mutate(lb_wide, !!col_name := unit_vector[[i]][1])
    }
    return(lb_wide)
}

# TODO: TEST!
warn_missing_data <- function(df) 
{
    # Check if 'USUBJID' is a column in the data frame
    if (!"USUBJID" %in% names(df)) {
        stop("The dataframe does not have a 'USUBJID' column.")
    }
    
    # Iterate over each unique USUBJID
    for (usubjid in unique(df$USUBJID)) 
    {
        # Subset the data frame for the current USUBJID
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

    for (param in params) {
        check <- unit_vector[[param]]
        all_same <- all(check == check[1])
        if (!all_same) {
            diff_unit_vars <- append(diff_unit_vars, param)
        }

    }

    if (length(diff_unit_vars) != 0) {
        warning("The following variables do not have the same units: ", paste(diff_unit_vars, collapse = ", "))
    }
}

# Subject-level dataset warnings issued that have more than one value per variable in the same visit.
# ASK: When we say more than one value per variable, we are talking about the same visit?
subject_check_vars <- function(df)
{
    # group subjects with lab visit and lab parameters.
    grouped_data <- dplyr::group_by(df, USUBJID, LBVST)

    # Summarize to count distinct lab parameter records for each group.
    count_params <- dplyr::summarise(
        grouped_data,
        each_param_count = n(),
        unique_param_count = n_distinct(LBPARAMCD)
    )

    # Filter to find groups with non-unique lab parameter records.
    non_unique_params <- dplyr::filter(count_params, each_param_count != unique_param_count)

    # Select the groups with non-unique lab parameters.
    warn <- dplyr::select(
        non_unique_params,
        USUBJID,
        LBVST
    )

    warn <- ungroup(warn)

    if (nrow(warn) > 0) {
        warning(
            paste(warn$USUBJID, "has more than one value for a given variable.")
        )
    }
}

# Time Varying dataset check: this function will check a dataset to see if there is values
# from the same time point.
time_varying_check <- function(df)
{
    # Group USUBJID and DTIM together to get unique data points.
    grouped <- dplyr::group_by(df, USUBJID, DTIM)

    # Summarise to count number of values in each group.
    counts <- dplyr::summarise(grouped, count = n())

    # Filter to see what groups have a count that is greater than 1.
    dupe_warn <- dplyr::filter(counts, count > 1)

    # Ungroup.
    dupe_warn_ungroup <- dplyr::ungroup(dupe_warn)

    if (nrow(dupe_warn) > 0) {
        warning(
            paste("The following subject(s) have duplicate date times during the same visit:", dupe_warn[[1]], dupe_warn[[2]])
        )
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
        lb_filtered <- dplyr::filter(lb, LBCOMPFL == "Y")
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

        # for each lb_param, find the corresponding unit.
        # create a vector that will be populated with the units.
        unit_vector <- lb_params_gen_unit_vector(lb, lb_param_coords, lb_params)

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
        lb_filtered <- dplyr::filter(lb, LBCOMPFL == "Y")
        lb_filtered <- dplyr::filter(lb_filtered, LBPARAMCD %in% lb_params)
        lb_filtered <- dplyr::mutate(lb_filtered, LBORRES = as.numeric(LBORRES))

        # now we are going to select USUBJID
        # ASK: Is this alway going to be the case??
        lb_param_coords <- find_lb_row_pos(lb_params, lb)
        unit_vector <- lb_params_gen_unit_vector(lb, lb_param_coords, lb_params)
        lb_params_u <- lb_params_u_appended(lb_params)
        lb_filtered <- dplyr::select(lb_filtered, USUBJID, DTIM = LBDT, !!lb_params := LBORRES)
        time_varying_check(lb_filtered)
        lb_appended_with_u <- lb_params_append_df(lb_filtered, lb_params_u, unit_vector)
        return(lb_appended_with_u)
    }
    else {
        stop("cov_option must be a vector of baseline dates, o2, o3, or, o4.")
    }
}


# result <- apmx_lab_processing(lb, lb_params, cov_options, "-828")
# seeing tast.
# lb <- as.data.frame(LB)

tast <- apmx_lab_processing(lb, "AST", "o4", "-111")

talt <- apmx_lab_processing(lb, "ALT", "o4", "-111")

# testing the removal a cell.
# lb[6,4] <- NA

# apmx_lab_processing(lb, lb_params, cov_options, "-828")

# START: TESTING SUBJECT LEVEL WARNINGS FOR DUPE VALUES OF VARIABLES.
# lb <- as.data.frame(LB)
# new_row <- data.frame(
#     STUDYID = "ABC102",
#     SITEID = 4,
#     USUBJID = "ABC102-04-008",
#     LBCAT = "Serum Biochemistry",
#     LBCOMPFL = "Y",
#     LBDT = "2022-07-10",
#     LBVST = "End of Treatment",
#     VISCRFN = 6,
#     LBTPT = "Pre-dose",
#     LBTPTN = 1,
#     LBPARAMCD = "GGT",
#     LBPARAM = "gamma glutamyl transferase",
#     LBPARAMN = 17,
#     LBORRES = "2.695",
#     LBORRESC = "2.695",
#     LBORRESU = "U/L"
# )

# appended_lb <- rbind(lb, new_row)

# subject_check_vars(lb)

# subject_check_vars(appended_lb)
# END: TESTING SUBJECT LEVEL WARNINGS FOR DUPE VALUES OF VARIABLES.

# START: TESTING TIME-VARYING COVARIETS

lb <- as.data.frame(LB)

new_row <- data.frame(
    STUDYID = "ABC102",
    SITEID = 1,
    USUBJID = "ABC102-01-001",
    LBCAT = "Serum Biochemistry",
    LBCOMPFL = "Y",
    LBDT = "2022-03-13",
    LBVST = "Screening",
    VISCRFN = 1,
    LBTPT = "Pre-dose",
    LBTPTN = 1,
    LBPARAMCD = "AST",
    LBPARAM = "aspartate aminotransferase",
    LBPARAMN = 15,
    LBORRES = "34.222",
    LBORRESC = "34.222",
    LBORRESU = "U/L"
)

appended_lb <- rbind(lb, new_row)

tast <- apmx_lab_processing(appended_lb, "AST", "o4", "-111")

# time_varying_check(appended_lb)

