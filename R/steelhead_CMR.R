#------------------------------------------------------------------------------
# Generate Steelhead mark recapture data and estimate abundance at all NPT
# traps.
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 7/30/18
#------------------------------------------------------------------------------

# load packages
library(tidyverse)

# Fins data
load(file = './data/fins_data.Rda')

# Creating the dataframe for all newly captured Steelhead  
# newcaptured_steelhead <- tempdata %>%
#   filter(Species == 'Steelhead') %>%
#   filter(Recap != 'TRUE') %>%
#   select(weir, Trap, Trap_Year, Species, Origin, `Living Status`, Disposition, Purpose, `Moved To`, Count, `Applied Marks`, `Applied Tags`, `Applied PIT`) %>%
#   group_by(weir, Trap_Year) %>%

# Get number of upstream fish that were released with a mark
marks <- fins_data %>%
  filter(Species == 'Steelhead') %>%
  filter(`Moved To` == 'Upstream') %>%
  filter(Marks == "TRUE") %>%
  filter(Recap != 'TRUE') %>%
  group_by(weir, Trap_Year) %>%
  summarise(Marks = sum(Count))
  
# Get number of downstream moving fish without marks = Captures - doesn't include recaptures
captures <- fins_data %>%
  filter(Species == 'Steelhead') %>%
  filter(`Moved To` == 'Downstream') %>%
  #filter(Marks == FALSE) %>% # no applied marks
  filter(Recap == FALSE) %>% # first capture
  group_by(weir, Trap_Year) %>%
  summarise(Captures = sum(Count))

# Get number of downstream moving fish with marks = Recaptures
recaptures<- fins_data %>%
  filter(Species == 'Steelhead') %>%
  filter(`Moved To` == 'Downstream') %>%
  filter(Recap == 'TRUE') %>%
   mutate(fishid = as.character(1:n()),  # create unique fish id - need to move to the entire fins dataset
         UniqueFishID = (ifelse(!is.na(`Existing PIT`), `Existing PIT`, fishid))) %>%
  select(-fishid) %>%
  arrange(desc(UniqueFishID), `Trapped Date`) %>% # arrange in order of capture events
  distinct(UniqueFishID, .keep_all = TRUE) %>% # keep first (by datetime) recapture event
  group_by(weir, Trap_Year) %>%
  summarise(Recaptures = sum(Count))
    
   
steelhead_mcr <- left_join(marks, captures) %>%
                    left_join(recaptures) %>%
  mutate(Captures = ifelse(is.na(Captures), 0, Captures),
         n1 = Marks,
         n2 = Captures + Recaptures,
         m2 = Recaptures,
         Nhat = (((n1 + 1)*(n2 + 1))/(m2 + 1)) - 1,
         Vhat = ((n1+1)*(n2+1)*(n1-m2)*(n2-m2))/((m2+1)^2*(m2+2)),
         lower95 = Nhat - 1.96*sqrt(Vhat),
         upper95 = Nhat + 1.96*sqrt(Vhat)) 

ggplot(steelhead_mcr, aes(x = Trap_Year, y = Nhat, colour = weir)) +
  geom_point(size = 2, position = position_dodge(width = .2)) +
  geom_errorbar(aes(ymin = lower95, ymax = upper95), position = position_dodge(width = .2)) +
  facet_wrap(~weir, scale = 'free_y', drop = TRUE) +
  theme_bw()

