                        ## R NOTES ##
# Pound sign in front depicts notes. (Will de-activate a line of code)
# %>% essentially means "then" - continuing a function to next operation.
# df = dataframe - refers to your dataset that you are using, assuming it is named df

# load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)


# to check what packages are installed
installed.packages()

# ?function will bring up the help window for that function (bottom right window)
?summarise

# See All column names.
names(data)

# See all column types.
glimpse(tempdata)

# rm(x) will remove an object you have created.  
rm(values)


# gather function switches from wide format to a long format
?gather #this gets a help window for this function
disp_purp <- gather(data, key= metric, value= x)
# spread is the opposite - switches from a long format to wide


# This will select fields from Facility to Species, Release_stream to disp_purp
select(Facility:Species, Release_stream:disp_purp) %>%
  # this selects all but "Action" field, and renames "location" to "Release_location"
  select(-Action, Release_location = location)


which(is.na(df$field))   # this will tell you which ROWS are 'NA' for the specified field.

which(is.null(df$field))  # this will tell you which rows are NULL for the specified field.