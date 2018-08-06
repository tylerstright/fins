#------------------------------------------------------------------------------
# Load Fins data (corrected spreadsheet) that has been altered to be consistent
# across all projects and traps.
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 7/30/18
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

# load FINS data.
data <- read_excel('./data/MODIFIED UNIQUE All_Research_FINS_data_standardized.xlsx')

# load NPT Mark/Tag Protocol. label df: mt_protocol
mt_protocol <- read_excel('./data/NPT Mark and Tag Protocol.xlsx')

#------------------------------------------------------------------------------
# Combine fins data with protocol and create useable dataset
#------------------------------------------------------------------------------
fins_data <- data %>%
  mutate(Trap_Year = year(`Trapped Date`)) %>%
  separate(Trap, into = c("weir", "Trap"), sep = " - ") %>%
  left_join(mt_protocol) %>%
  mutate(Marks = ifelse(str_detect(`Applied Marks`, mark_type), TRUE,
                        ifelse(str_detect(`Applied Tags`, tag_type), TRUE,
                               ifelse(str_detect(`Applied PIT`, tag_type), TRUE, FALSE))))

#------------------------------------------------------------------------------
# Combine fins data with protocol and create useable dataset
#------------------------------------------------------------------------------
save(fins_data, file = 'C:/R input/fins_data.Rda')
save(fins_data, file = './data/fins_data.Rda')
      
