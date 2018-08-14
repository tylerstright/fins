#------------------------------------------------------------------------------
#               Summary Count of Purpose and Mortalities for all Fish
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 
#------------------------------------------------------------------------------
# load packages and FINS data
library(tidyverse)

load(file = './data/fins_data.Rda')

#------------------------------------------------------------------------------
# create list of purposes for Summary.
purpose.list <- c('Biological Sampling', 'Brood Stock', 'Distribution', 
                  'Fisheries','General Holding', 'Natural Spawning', 
                  'Nutrient Enhancement', 'Other', 'Recycled', 'Stray Removal', 
                  'Stray Relocation', 'Stray Removal Distribution', 'Unknown', 
                  'Within FINS Facility' )
# Summarize count of fish with same "Purpose"
npt_purpose <- fins_data %>%
  filter(Purpose %in% purpose.list) %>%
  group_by(weir, Trap_Year, Purpose, Species, `Moved To Facility`) %>%
  summarise(count = sum(Count)) %>%
  spread (key = Purpose, value = count) 
#------------------------------------------------------------------------------
# Create list of mortality types
all.morts <- c('Trap Mort', 'DOA', 'Killed')

# Summarize count of Killed, Trap Mort, DOA and Total dead fish records
npt_deadfish <- fins_data %>%
  filter(`Living Status` %in% all.morts) %>%
  group_by(weir, Trap_Year, `Living Status`, Species, `Moved To Facility`) %>%
  summarise(morts = sum(Count)) %>%
  spread(key = `Living Status`, value = morts, fill = 0 ) %>%
  select(weir, Trap_Year, `Moved To Facility`, `Trap Mort`, DOA, Killed) %>%
  mutate(total_morts = `Trap Mort` + DOA + Killed) %>%
  arrange(weir, desc(Trap_Year))

#------------------------------------------------------------------------------
# Joins two tables to create a count for all mortalities and fish destined for 
# the same purpose
npt_summary <- left_join(npt_purpose, npt_deadfish)
