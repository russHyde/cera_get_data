

# This script should do the following:
# Read in the UCI/HAR dataset
# Merge the training and test data into a single dataset
# Subset the columns of the dataset - keeping those columns containing a mean
#   or sd value for a measurement
# Rename the activities in the dataset
# Rename the columns of the dataset
# Create a second tidy dataset based on the subset/renamed dataset
# - this should contain the average of each variable for each activity and
# subject

# tidy-data should have the following:
# - each measured variable in a single column
# - each different observation should be in a different row
# - one table for each kind of data
# - when multiple tables are present, there should be a linking column for use
# in merging

# code-book should contain:
# - info about the variables in the submitted data (units etc)
# - info about the summary choices (eg, use of mean or median)
# - info about the expt study design
# - Section called 'Study design' containing thorough description of data
# collection
# - Section called 'Code book' that describes each var and its units

###############################################################################

#' Imports the contents of the features.txt file from the UCI HAR Dataset
#' 
#' Converts the 561 feature names to more R-appropriate names
#' 
#' The returned values have no parentheses, no dashes, no commas
#'
parse_features_file <- function(
        filename
    ){
    require(magrittr)
    require(stringr)
    stopifnot(file.exists(filename))
    features <- read.table(filename, sep = " ", header = FALSE)[, 2] %>%
        as.character() %>%
        # replace trailing parentheses
        str_replace_all("\\(*\\)$", "") %>%
        # replace any other parens, dashes or commas with underscore
        str_replace_all("[(),-]", "_") %>%
        # ensure no multiple-underscores are observed
        str_replace_all("_+", "_") 
    features
}

parse_x_vals_file <- function(
        filename,
        feature_names
    ){
    require(readr)
    require(magrittr)

    stopifnot(file.exists(filename))
    stopifnot(is.character(feature_names))

    # There are 561 variables in the file, each column is 16-characters wide
    xs <- read_fwf(
        filename,
        col_positions = fwf_widths(rep(16, 561)),
        col_types = cols(.default = col_double()),
        progress = FALSE
        )

    stopifnot(length(feature_names) == ncol(xs))

    set_colnames(xs, feature_names)
}

combine_subjects_and_xvals <- function(
        xval_filename,
        subject_filename,
        feature_names,
        dataset_id
    ){
    require(magrittr)
    require(tibble)

    stopifnot(file.exists(subject_filename))

    xvals <- parse_x_vals_file(xval_filename, feature_names)
    subjects <- scan(subject_filename, what = "character")

    stopifnot(length(subjects) == nrow(xvals))

    xvals %>%
        tibble::add_column(
            subject = subjects,
            dataset = rep(dataset_id, nrow(xvals)),
            .before = 1
            )
}

#' Imports each dataframe from the test and train subdirectories of the
#' unzipped "UCI HAR Dataset" directory
#'
#' User should ensure the file is unzipped prior to running this
#'
#' test directory contains files:
#'   - subject_test.txt
#'   - X_test.txt
#'   - y_test.txt
#'   - Inertial Signals/
#'     - body_acc_[x|y|z]_test.txt
#'     - body_gyro_[x|y|z]_test.txt
#'     - total_acc_[x|y|z]_test.txt
#'
#' train directory contains files:
#'   - subject_train.txt
#'   - X_train.txt
#'   - y_train.txt
#'   - Inertial Signals/
#'     - body_acc_[x|y|z]_train.txt
#'     - body_gyro_[x|y|z]_train.txt
#'     - total_acc_[x|y|z]_train.txt
#'
#' 
#' The X_[train|test].txt files contain 561 columns
#' - the cols are space separated, but the number of space chars is random
#' - reading in and splitting on single-spaces, there are at-most 561 non-NA
#'     entries
#' - there are also 561 entries in the features.txt file
import_uci_har_data <- function(dir_prefix = "UCI HAR Dataset"){
    
    stopifnot(dir.exists(dir_prefix))
    test_dir <- file.path(dir_prefix, "test")
    train_dir <- file.path(dir_prefix, "train")

    features <- parse_features_file(
        file.path(dir_prefix, "features.txt")
        )
    
    test_vals <- combine_subjects_and_xvals(
        file.path(test_dir, "X_test.txt"),
        file.path(test_dir, "subject_test.txt"),
        feature_names = features,
        dataset_id = "test"
        )

    train_vals <- combine_subjects_and_xvals(
        file.path(train_dir, "X_train.txt"),
        file.path(train_dir, "subject_train.txt"),
        feature_names = features,
        dataset_id = "train"
        )
    
    rbind(test_vals, train_vals)
}

#' Restricts the columns of a UCI-HAR dataset to keep only those that contain
#' info on the subject, or the mean or std-dev of a measurable quantity
#'
#' This function disregards "meanFreq" columns
#'
#' Whether a subject was used in the test- or the training dataset is retained
#'
select_uci_har <- function(
        uci_har_data
    ){
    require(dplyr)

    keep_cols <- grepl("subject|dataset|_mean|_std", colnames(uci_har_data))
    uci_har_data[, keep_cols] %>%
        dplyr::select(-contains("meanFreq"))
    }


# TODO:
rename_activities_uci_har <- function(
        uci_har_data
    ){
    # TODO
    }

###############################################################################
