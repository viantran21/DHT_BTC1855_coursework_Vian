#' Assignment 4.
#' Purpose: Clean a dataset and perform modifications/analyses as specified
#' in the assignment instructions.
#' Plan:
#' 1. Read the data into a data frame. Keep this as a working copy and make 
#' a new data frame for any modifications.
#' 2. Explore data frame and identify any issues, whether structural or data
#' related, and fix them if applicable. This will be the majority of the
#' script. Specifically check for uniformity, consistency, non-missingness
#' and non-redundancy.
#' 3. Take a closer look at variables specified in the assignment, specifically
#' looking for outliers and removing them from analysis.
#' 4. Remove potential hoax sightings by examining the "comments" variable
#' for clues/indicators.
#' 5. Add another column with delay in reporting time and remove observations
#' with negative (invalid) delay in reporting time.
#' 6. Report average delay in reporting time, grouped by country variable.
#' 7. Create a histogram for the "duration_seconds" variable, post-data cleaning
#' and removal of outliers.
