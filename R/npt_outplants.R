#------------------------------------------------------------------------------
# Generate outplant data for all NPT Programs
#
#                                                 ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/2/18
#------------------------------------------------------------------------------

# load packages
library(tidyverse)

# load standardized NPT Fins data
load(file = './data/fins_data.Rda')

#### this filters to show all Outplants in streams for all years.
npt_outplants <- fins_data %>%
  filter(grepl('Outplant', `Moved To`)) %>% # filter by `Moved To` values containing: Outplant
  group_by(Facility, weir, Trap_Year, Species, `Moved To`, Disposition, Purpose) %>%
  summarise(count = n()) %>%
  separate(`Moved To`, into = c('Action', 'Release_location'), sep = " - ") %>%
  separate(Release_location, into = c('Release_stream', 'location'), sep = ": ") %>%
  select(-Action, Release_location = location) %>%
  arrange(Trap_Year) %>%
  ungroup() %>%
  group_by(Facility, weir, Trap_Year, Species, Release_stream) %>%
  summarise(n = sum(count))


# Takes npt_outplants and plots data in a Graph
outplant_bars <- npt_outplants %>%
  ggplot(aes(x = Trap_Year, y = n,  fill = Release_stream, colour = Release_stream)) +
  geom_bar(stat = 'identity') +
  #geom_line() +
  facet_wrap(~Trap) +
  theme_bw()

# To save above graph:
ggsave("outplant_bars.png", outplant_bars, path = "./data")

# Save npt_outplants as a CSV
write.csv(chinook_mcr, file = './data/npt_outplants.csv')
