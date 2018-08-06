#------------------------------------------------------------------------------
#               Stray Removals
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 
#------------------------------------------------------------------------------

# load packages
library(tidyverse)

# Fins data
load(file = './data/fins_data.Rda')

# Count of Stray Removals from weir with Destination (Moved To Facility)
npt_strays <- fins_data %>%
  filter(Purpose == 'Stray Removal') %>%
  group_by(weir, Trap_Year, `Moved To Facility`) %>%
  summarise(strays_removed = sum(Count)) %>%
  arrange(weir)

# Count of 'Trap Mort' records
npt_trapmort <- fins_data %>%
  filter(`Living Status` == 'Trap Mort') %>%
  group_by(weir, Trap_Year, `Moved To Facility`) %>%
  summarise(trap_morts = sum(Count))

