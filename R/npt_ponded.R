#------------------------------------------------------------------------------
# Generate Steelhead mark recapture data and estimate abundance at all NPT
# traps.
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 
#------------------------------------------------------------------------------

# load packages
library(tidyverse)

# Fins data
load(file = './data/fins_data.Rda')


categories <- c('Brood Stock', 'General Holding', 'Stray Removal')

npt_broodstock <- fins_data %>%
  filter(Purpose %in% categories) %>%
  group_by(weir, Trap_Year, `Moved To Facility`, Purpose) %>%
  summarise(count = sum(Count)) %>%
  spread(key = Purpose, value = count ) %>%
  arrange(weir)


# Count of Brood stock from weir with Destination (Moved To Facility)
npt_broodstock <- fins_data %>%
  filter(Purpose == 'Brood Stock') %>%
  group_by(weir, Trap_Year, `Moved To Facility`) %>%
  summarise(Broodstock = sum(Count)) %>%
  arrange(weir)

# Count of fish destined for General Holding  
gen_holding <- fins_data %>%
  filter(Purpose == 'General Holding') %>%
  group_by(weir, Trap_Year, `Moved To Facility`) %>%
  summarise(gen_holding = sum(Count)) %>%
  arrange(weir)

# shows all fish destined for Brood Stock or General Holding for all Projects, all Years.
npt_ponded <- left_join(npt_broodstock, gen_holding)
