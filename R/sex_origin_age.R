#------------------------------------------------------------------------------
#  Stratifying Chinook estimates by Sex, Age, and Origin
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/15/18
#------------------------------------------------------------------------------

# load packages and FINS/NPT carcass data
library(tidyverse)

load(file = './data/fins_data.Rda')
load(file = './data/npt_carcass.Rda')
#------------------------------------------------------------------------------
# Alter npt_carcass
npt_stratify <- npt_carcass %>%
  filter(AboveWeir == 'Yes') %>%  # filter for above weir carcasses
  group_by(StreamName, Trap_Year)

#------------------------------------------------------------------------------
#   Calculating Hatchery Fraction (ORIGIN)

nat_spawn <- npt_stratify %>%    
  filter(VerifiedOrigin == 'Natural') %>%
  summarise(natural_spawners = n())

hat_spawn <- npt_stratify %>%
  filter(VerifiedOrigin == 'Hatchery') %>%
  summarise(hatchery_spawners = n())

hatchery_fraction <- left_join(nat_spawn, hat_spawn) %>%
  select(StreamName, Trap_Year, natural_spawners, hatchery_spawners) %>%
  mutate(natural_spawners = ifelse(is.na(natural_spawners), 0, natural_spawners),
         hatchery_spawners = ifelse(is.na(hatchery_spawners), 0, hatchery_spawners),
         known_spawners = natural_spawners + hatchery_spawners) %>%
  mutate(nH = hatchery_spawners,
         nO = known_spawners,
         pHOS = nH/nO,
         pNOS = (1-pHOS))

#------------------------------------------------------------------------------
#   Calculating Percent Females  (SEX)

f_carcass <- npt_stratify %>%    
  filter(Sex == 'Female') %>%       # ONLY includes AboveWeir carcasses.
  summarise(female_carcass = n())

m_carcass <- npt_stratify %>%
  filter(Sex == 'Male') %>%
  summarise(male_carcass = n())

percent_females <- left_join(f_carcass, m_carcass) %>%
  select(StreamName, Trap_Year, female_carcass, male_carcass) %>%
  mutate(female_carcass = ifelse(is.na(female_carcass), 0, female_carcass),
         male_carcass = ifelse(is.na(male_carcass), 0, male_carcass),
         known_sex = female_carcass + male_carcass) %>%
  mutate(nf = female_carcass,
         ns = known_sex,
         fhat = nf/ns)

#------------------------------------------------------------------------------
#   Stratifying Age classes.

          'TBD'

# Join Sex and Origin %, select for final values.
sex_origin <- left_join(percent_females, hatchery_fraction) #%>%
select(StreamName, Trap_Year, pHOS, fhat)
