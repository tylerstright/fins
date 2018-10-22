#------------------------------------------------------------------------------
#               Summary Count of Purpose and Mortalities for all Fish
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/8/18
#------------------------------------------------------------------------------
# Create list of FINS purposes for Summary.
purpose.list <- c('Biological Sampling', 'Brood Stock', 'Distribution', 
                  'Fisheries','General Holding', 'Natural Spawning', 
                  'Nutrient Enhancement', 'Other', 'Recycled', 'Stray Removal', 
                  'Stray Relocation', 'Stray Removal Distribution', 'Unknown', 
                  'Within FINS Facility', NA)

# Summarize count of fish with same "Purpose"
npt_purpose <- fins_data %>%
  filter(Purpose %in% purpose.list) %>%
  group_by(weir, Trap_Year, Purpose, Species, `Moved To Facility`) %>%
  summarise(count = sum(Count)) %>%
  spread (key = Purpose, value = count, fill = "")
#------------------------------------------------------------------------------
# Create list of MORTALITY types
all.morts <- c('TrapMort', 'DOA', 'Killed')

# Summarize count of Killed, Trap Mort, DOA and Total dead fish records
npt_deadfish <- fins_data %>%
  filter(`Living Status` %in% all.morts) %>%
  group_by(weir, Trap_Year, `Living Status`, Species) %>%
  summarise(morts = sum(Count)) %>%
  spread(key = `Living Status`, value = morts, fill = 0 ) %>%
  select(weir, Trap_Year, Species, TrapMort, DOA, Killed) %>%
  mutate(Total_morts = TrapMort + DOA + Killed) %>%
  arrange(weir, desc(Trap_Year))


#------------------------------------------------------------------------------
# Summarize Out-planted fish
npt_outplants <- fins_data %>%
  filter(grepl('Outplant', `Moved To`)) %>% 
  group_by(weir, Trap_Year, Species, `Moved To`, Disposition, Purpose) %>%
  summarise(count = n()) %>%
  separate(`Moved To`, into = c('Action', 'Release_location'), sep = " - ") %>%
  separate(Release_location, into = c('Release_stream', 'location'), sep = ": ") %>%
  select(-Action, Release_location = location) %>%
  arrange(Trap_Year) %>%
  ungroup() %>%
  group_by(Weir = weir, Trap_Year, Species, Release_stream) %>%
  summarise(n = sum(count))

#------------------------------------------------------------------------------
# Join Purpose and Mortality Summaries
npt_summary <- left_join(npt_purpose, npt_deadfish) %>%
  left_join(npt_outplants)

#------------------------------------------------------------------------------
# Save npt_summary as a CSV
# write.csv(npt_summary, file = './data/npt_summary.csv')


