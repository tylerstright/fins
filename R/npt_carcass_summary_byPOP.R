#------------------------------------------------------------------------------
#       Carcass/Transect data import and modification for estimates
#
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/15/18
#------------------------------------------------------------------------------
# load packages and FINS/NPT carcass data
library(tidyverse)
library(RODBC)
#------------------------------------------------------------------------------
# Create connection with database
con <- odbcConnect('sgs_master', uid = 'guest', pwd = 'guest') # named dsn in admin tools.
#------------------------------------------------------------------------------
# Extract SQL carcass and transect database table  via above (con)nection
sql_carcassdetail <- sqlFetch(con, 'carcass_detail')
sql_transect_metadata <- sqlFetch(con, 'transect_metadata')
#------------------------------------------------------------------------------
# Create dataframe to add 'weir' to transect metadata based on POP_NAME
POP_NAME <- c('East Fork South Fork Salmon River', 'Lolo Creek','Snake River 
              Lower Mainstem', 'Upper South Fork Clearwater','Secesh River', 
              'South Fork Salmon River mainstem')
weir <- c('Johnson Creek Weir', 'Lolo Creek Weir', 'Lower Granite Dam', 
          'Newsome Creek Weir', 'Secesh DIDSON', 'SFSR Weir')
POP_weir <- data.frame(POP_NAME, weir)
#------------------------------------------------------------------------------
# Join carcass, transect, and POP_weir
sql_ctw <- left_join(sql_transect_metadata, POP_weir) %>%
  select(StreamName, weir, POP_NAME, TransectName, AboveWeir) %>%
  group_by(StreamName, POP_NAME, weir, AboveWeir) %>%
  left_join(sql_carcassdetail) %>%
  mutate(Trap_Year = SurveyYear) %>%
  ungroup() %>%
  group_by(POP_NAME, Trap_Year)
#------------------------------------------------------------------------------
#   Calculate Hatchery Fraction (ORIGIN)
nat_spawn <- sql_ctw %>%    
  filter(VerifiedOrigin == 'Natural') %>%
  summarise(natural_spawners = sum(Count))

hat_spawn <- sql_ctw %>%
  filter(VerifiedOrigin == 'Hatchery') %>%
  summarise(hatchery_spawners = sum(Count))

hatchery_fraction <- left_join(nat_spawn, hat_spawn)
#------------------------------------------------------------------------------
# Calculate Pre-spawn mortality
prespawn_mortality <- sql_ctw %>%
  group_by(POP_NAME, Trap_Year) %>%
  filter(Spawned == 'No') %>%
  summarise(prespawn_morts = sum(Count))
#------------------------------------------------------------------------------
# Sum TOTAL carcasses observed both upstream and downstream
all_carcass <- sql_ctw %>%
  summarise(all_carcasses = sum(Count))
#------------------------------------------------------------------------------
#   Calculating Percent Females for Upsteam (us) of weir 
f_us_carcass <- sql_ctw %>%    
  filter(AboveWeir == 'Yes', 
         Sex == 'Female') %>%       
  summarise(us_female_carcass = sum(Count))

m_us_carcass <- sql_ctw %>%
  filter(AboveWeir == 'Yes',
         Sex == 'Male') %>%
  summarise(us_male_carcass = sum(Count))

f_us <- left_join(f_us_carcass, m_us_carcass)
#------------------------------------------------------------------------------
#   Calculating Percent Females for Downstream (ds) of weir 
f_ds_carcass <- sql_ctw %>%    
  filter(AboveWeir == 'No', 
         Sex == 'Female') %>%       
  summarise(ds_female_carcass = sum(Count))

m_ds_carcass <- sql_ctw %>%
  filter(AboveWeir == 'No',
         Sex == 'Male') %>%
  summarise(ds_male_carcass = sum(Count))

f_ds <- left_join(f_ds_carcass, m_ds_carcass)
#------------------------------------------------------------------------------
## Join Sex, Origin, & Prespawn Mortality
origin_sexMOD <- full_join(f_us, f_ds)%>%
  left_join(hatchery_fraction) %>%
  left_join(prespawn_mortality) %>%
  left_join(all_carcass) %>%
  mutate(ds_female_carcass = ifelse(is.na(ds_female_carcass), 0, ds_female_carcass),
         ds_male_carcass = ifelse(is.na(ds_male_carcass), 0, ds_male_carcass),
         ds_known_sex = ds_female_carcass + ds_male_carcass) %>%
  mutate(ds_percent_females = ds_female_carcass/(ds_known_sex)) %>%
  mutate(us_female_carcass = ifelse(is.na(us_female_carcass), 0, us_female_carcass),
         us_male_carcass = ifelse(is.na(us_male_carcass), 0, us_male_carcass),
         us_known_sex = us_female_carcass + us_male_carcass) %>%
  mutate(us_percent_females = us_female_carcass/(us_known_sex)) %>%
  mutate(all_carcasses = ifelse(is.na(all_carcasses), 0, all_carcasses)) %>%
  mutate(natural_spawners = ifelse(is.na(natural_spawners), 0, natural_spawners),
         hatchery_spawners = ifelse(is.na(hatchery_spawners), 0, hatchery_spawners),
         known_spawners = natural_spawners + hatchery_spawners) %>%
  mutate(nH = hatchery_spawners,
         nO = known_spawners,
         pHOS = nH/nO,
         pNOS = (1-pHOS)) %>%
  mutate(prespawn_morts = ifelse(is.na(prespawn_morts), 0, prespawn_morts)) %>%
  mutate(prespawnmort_percent = prespawn_morts/(ds_female_carcass + us_female_carcass))
#------------------------------------------------------------------------------
