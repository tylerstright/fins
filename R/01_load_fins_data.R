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
# Create dataframe for `StreamName` associated with `weir`
#------------------------------------------------------------------------------
weir <- c('Bradford Weir (Lolo Creek)', 'Joseph Creek Weir', 'Camp Creek Weir',
          'Imnaha River Weir', 'Freezeout Creek Weir', 'Gumboot Creek Weir', 
          'Dry Creek Weir', 'Upper Lolo Weir', 'Lolo Creek Weir', 
          'Johnson Creek Weir', 'NPT Hatchery Trap', 'Lostine River Weir', 
          'Newsome Creek Weir', 'Lower Granite Dam Trap')
StreamName <- c('Lolo Creek', 'Joseph Creek', 'Camp Creek', 'Imnaha River', 
                'Freezeout Creek', 'Gumboot Creek', 'Dry Creek', 'Lolo Creek', 
                'Lolo Creek', 'Johnson Creek', 'NPT Hatchery Trap', 
                'Lostine River', 'Newsome Creek', 'Lower Granite Dam Trap')
streams <- data.frame(weir, StreamName)

#------------------------------------------------------------------------------
# Combine fins data with protocol, StreamName, and create useable dataset
#------------------------------------------------------------------------------
fins_data <- data %>%
  separate(Trap, into = c("weir", "Trap"), sep = " - ") %>%
  mutate(Trap_Year = year(`Trapped Date`)) %>%
  left_join(mt_protocol) %>%
  left_join(streams) %>%
  mutate(Marks = ifelse(str_detect(`Applied Marks`, mark_type), TRUE,
                        ifelse(str_detect(`Applied Tags`, tag_type), TRUE,
                               ifelse(str_detect(`Applied PIT`, tag_type), TRUE, FALSE))))


#------------------------------------------------------------------------------
# Save modified fins data
#------------------------------------------------------------------------------
save(fins_data, file = 'C:/R input/fins_data.Rda')
save(fins_data, file = './data/fins_data.Rda')
