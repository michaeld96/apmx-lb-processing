
source("apmx_lb_processing.R")

##### TEST 1 #####
# expect fail - fails as intended
# test <- apmx_lab_processing(lb,
#                             lb_params = c("AST", "ALT", "ALB", "BILI", "CREAT"))

##### TEST 2 #####
# expect fail - fails as intended
# test <- apmx_lab_processing(lb,
#                             lb_params = c("AST", "ALT", "ALB", "BILI", "CREAT"),
#                             cov_option = 1)

#### TEST 3 #####
# expect success - success!
test2 <- apmx_lab_processing(lb,
                             lb_params = c("AST", "ALT", "ALB", "BILI", "CREAT"),
                             cov_option = "o2")

##### TEST 4 #####
# expect success - success!
test3 <- apmx_lab_processing(lb,
                             lb_params = c("AST", "ALT", "ALB", "BILI", "CREAT"),
                             cov_option = "o3")

##### TEST 5 #####
# expect success - fails :(
# browser()
test4 <- apmx_lab_processing(lb,
                             lb_params = c("AST", "ALT", "ALB", "BILI", "CREAT"),
                             cov_option = "o4")
# Error in `dplyr::select()` at Stephen.amori/Downloads/apmx_lb_processing.R:373:8:
#   The LHS of `:=` must be a string, not a character vector.

##### NOTE 1: Roxygen2 notes #####
# R packages require functions to have notes written with Roxygen2
# Roxygen is a specific format for notes that integrate with base R and RStudio
# For example, try running the following code:

  # library(apmx)
  # ?pk_build()

# It will provide a detailed description of the function in a consistent format.
# You can find the Roxygen "skeleton" in the apmx git repository.
# The are comments at the top of each
# Compare the comment structure of pk_build() against the output of ?pk_build() to see how it works

# Please add the Roxygen structure to each function so they are easy to read and interpret
# You will only need to add the @export and @examples parameter to you exported function, apmx_lab_processing()
# You can learn more about Roxygen here: https://r-pkgs.org/man.html

##### NOTE 2: Different subject counts #####
# Note that dataframes test2 and test3 have different numbers of subjects

length(unique(lb$USUBJID)) #22 subjects
length(unique(test2$USUBJID)) #20 subjects
length(unique(test3$USUBJID)) #22 subjects

# This is because there are two subjects with no labs labeled "Baseline (D1)", which is the key visit flag used in the function

"Baseline (D1)" %in% lb$LBVST[lb$USUBJID=="ABC102-01-005" | lb$USUBJID=="ABC102-02-004" & lb$LBCOMPFL=="Y"]

# We should issue a warning that the output does not contain those two subjects
# "Could not find baseline labs for the following subjects: ABC102-01-005, ABC102-02-004"
# You can compare the subjects contained in lb (input) against subjects in output

##### NOTE 3: Filtering #####
# In the body of the function, you are filtering on `LBCOMPFL == "Y"`
# This is generally a good idea!
# However, it is not guaranteed that the input lb dataset will have that column, or that it will contain "Y"
# Perhaps it is best to allow the user to pre-filter prior to calling you function
# For example:
    # output <- lb %>%
    #   filter(LBCOMPFL == "Y") %>%
    #   apmx_lab_processing(lb_params = c("AST", "ALT"), cov_option = "o2")
# This way, the user can control which records are included and which aren't.

##### NOTE 4: Method o2 visit selection #####
# Method 02 works as intended. Congrats!

# Now, we should allow the user to select which column and which item in that column to filter on
# It's not a guarantee that the baseline visit is called "Baseline (D1)"
# It's not a guarantee that the column is called LBVST

# Perhaps we can add an additional argument for the column and another argument for the value?
# These arguments would be required if method o2 is selected

# If it seems like this function has too many parameters, it may be easier to write one function for each cov_method

##### NOTE 5: Result variable name #####
# You are currently pulling results from column LBORRES
# This is good!

# There are a few other columns the user may want to pull results from
# They are commonly called:
  # LBSTRESN
  # AVAL

# Can you add an argument for the column name with the results, where the default is LBORRES?

# This also applies for LBORRESU, the column for units
# LBORRESU is a good default. However, the user may want to select a different column, like LBSTRESU

##### NOTE 6: Category variable name #####
# You are currently pulling categories from column LBPARAMCD
# This is good!

# There are a few other columns the user may want to pull results from
# They are commonly called:
  # LBPARAM
  # PARAM
  # PARAMCD

# Can you add an argument for the column name with the results, where the default is LBPARAMCD?
