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

# Read data into a data frame.
raw_data <- read.csv("ufo_subset.csv")
# Keep a working copy for any modifications. 
clean_data <- raw_data

# Explore data frame.
raw_data
dim(raw_data)
summary(raw_data)
str(raw_data)
library(dplyr)
glimpse(raw_data)

#' Plotting data to identify uniformity issues. "duration.seconds" looks
#' good, though likely some outliers present.
hist(raw_data$duration.seconds)
#' Trying to similarly plot "duration.hours.min" reveals that it is not
#' numeric type. Units also inconsistent. Cannot change this to continuous
#' numerical data, so best to keep this as character type for now. 
#' "duration.seconds" can be used instead for any plotting/analysis.
hist(raw_data$duration.hours.min)

#' Confirming that variable names are descriptive, have no white spaces and
#' have no special characters.
names(raw_data)
#' Change "duration.seconds" to include underscore instead of period. No 
#' need to change "duration.hours.min" since it is redundant and will be
#' removed. Change is for consistency in variable names. Also change 
#' "datetime" variable name to include an underscore.
names(clean_data)[names(clean_data)=="duration.seconds"] <- "duration_seconds"
names(clean_data)[names(clean_data)=="datetime"] <- "date_time"

# Check if any duplicate observations. No duplicates, so no need to remove.
TRUE %in% duplicated(raw_data)

#' Drop "duration.hours.min" column as it is redundant with 
#' "duration.seconds" and has other problems as outlined above.
clean_data <- subset(clean_data, select = -c(duration.hours.min))

# Blank spaces in data frame set to NA.
clean_data[clean_data==""] <- NA

#' Ensure that date/time elements in "date_time" and "duration_seconds" 
#' variables are in a consistent format.
library(lubridate)
#' Convert "date_time" to date class using strptime(), then parse using 
#' parse_date_time() specifying "ymd HMS" format. If not NA, then we know
#' the element in "date_time" is in the correct, specified format. If NA,
#' then we know it is in the wrong format.
format1 <- !is.na(parse_date_time(strptime(clean_data$date_time, "%Y-%m-%d %H:%M"), orders="ymd HMS"))
#' OOPS! It looks like any element with a time of 0:00 is being recognized
#' as having no time component. The below does the same as the above, but
#' parses for 0:00 instead.
format2 <- !is.na(parse_date_time(strptime(clean_data$date_time, "%Y-%m-%d %H:%M"), orders="ymd 0:00"))
#' Let's compare the two logical vectors to make sure that all indices with NAs
#' in format1 are not NAs in format2 and thus valid. Since the whole vector is
#' TRUE, that means all elements in "date_time" are in the correct format.
zero_second_check <- which(format1==FALSE) == which(format2==TRUE)
length(which(zero_second_check==FALSE))
#' Do the same for "date_posted". Since the whole vector is TRUE, that
#' means all elements in "date_posted" are in the specified format, dmy.
format3 <- !is.na(parse_date_time(strptime(clean_data$date_posted, "%d-%m-%Y"), orders="ymd"))
length(which(format3 == "FALSE"))
# Change variable types from characters to more correct types.
clean_data$date_time <- strptime(clean_data$date_time, "%Y-%m-%d %H:%M")
clean_data$date_posted <- strptime(clean_data$date_posted,"%d-%m-%Y")
clean_data$city <- factor(clean_data$city)
clean_data$state <- factor(clean_data$state)
clean_data$country <- factor(clean_data$country)
clean_data$shape <- factor(clean_data$shape)

# Let's examine the variable, "country".
table(clean_data$country, useNA = "ifany")
barplot(table(clean_data$country, useNA = "ifany"))
#' Looks good for a categorical variable! All missing values already made
#' into NAs.
# Let's examine the variable, "shape".
table(clean_data$shape, useNA = "ifany")
barplot(table(clean_data$shape, useNA = "ifany"))
#' Looks good for a categorical variable! Since unknown values are denoted
#' as "unknown", we must be aware that NAs are missing values not unknown
#' values.
# Let's examine the variable, "duration_seconds".
table(clean_data$duration_seconds)
boxplot(clean_data$duration_seconds)
#' There appear to be many points that are outliers (orders of magnitude 
#' larger than the other points. Let's set these values to NA instead of
#' removing the entire observation, as the other variables in the 
#' observation may be valuable. We'll set the threshold to <= 86400 
#' seconds, as that is how many seconds are in one day, and values above
#' that may be erroneous.
boxplot(clean_data$duration_seconds)
clean_data$duration_seconds[clean_data$duration_seconds > 86400] <- NA
boxplot(clean_data$duration_seconds)

# Remove observations with NUFORC comments using grepl() function.
clean_data <- clean_data[which(grepl("NUFORC", clean_data$comments) == FALSE),] 

#' Use mutate() function to create a new variable for delay in reporting
#' time, calculated using the difftime() function.
clean_data <- clean_data %>%
  mutate(report_delay=difftime(clean_data$date_posted,clean_data$date_time, units="days"))

#' Subset only the observations with a positive "report_delay" and verify
#' that the updated data frame has no negative values in "report_delay".
clean_data <- clean_data[which(difftime(clean_data$date_posted,clean_data$date_time)>0),]
length(which(difftime(clean_data$date_posted,clean_data$date_time)<0))

# Create new data frame to display average "report_delay" by country.
new_data <- clean_data
new_data %>%
  # Group by country variable.
  group_by(country) %>%
  #' Summarize to create a tibble with average "report_delay" for each 
  #' level of the grouped variable.
  summarize(average_report_delay=mean(report_delay,na.rm = T))

#' Create a histogram for "duration_seconds" variable, post data cleaning and
#' assigning NAs to outliers.
hist(clean_data$duration_seconds)
#' Histogram would benefit from logarithmic transformation. Now there
#' appears to be a normal-esque distribution, which if true, is helpful 
#' for making assumptions for further analysis/tests.
hist(log10(clean_data$duration_seconds),main="",xlab = "Log10(Duration of Sighting in Seconds)")