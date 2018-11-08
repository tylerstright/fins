#------------------------------------------------------------------------------
#       Carcass/Transect data import and modification for estimates
#
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/15/18
#------------------------------------------------------------------------------
# Extract SQL carcass data 
sql_carcassdetail <- sqlFetch(con, 'carcass_detail')

#------------------------------------------------------------------------------
# Join carcass, transect, and streams[df]
raw_carcass <- left_join(sql_transect_metadata, streams) %>%
  select(StreamName, weir, POP_NAME, TransectName, AboveWeir) %>%
  group_by(StreamName, POP_NAME, weir, AboveWeir) %>%
  inner_join(sql_carcassdetail) %>%                       # inner join
  mutate(Trap_Year = SurveyYear) %>%
  ungroup() %>%
  group_by(POP_NAME, Trap_Year)

#------------------------------------------------------------------------------
#   Calculate Hatchery Fraction (ORIGIN)
nat_spawn <- raw_carcass %>%    
  filter(VerifiedOrigin == 'Natural') %>%
  summarise(natural_spawners = sum(Count))

hatchery_fraction <- raw_carcass %>%
  filter(VerifiedOrigin == 'Hatchery') %>%
  summarise(hatchery_spawners = sum(Count)) %>%
  right_join(nat_spawn)

#------------------------------------------------------------------------------
# Calculate Pre-spawn mortality
prespawn_mortality <- raw_carcass %>%
  group_by(POP_NAME, Trap_Year) %>%
  filter(Spawned == 'No') %>%
  summarise(prespawn_morts = sum(Count))
#------------------------------------------------------------------------------
# Sum TOTAL carcasses observed both upstream and downstream
all_carcass <- raw_carcass %>%
  summarise(all_carcasses = sum(Count))
#------------------------------------------------------------------------------
#   Calculating Percent Females for Upsteam (us) of weir 
f_us_carcass <- raw_carcass %>%    
  filter(AboveWeir == 'Yes', 
         Sex == 'Female') %>%       
  summarise(us_female_carcass = sum(Count))

us_carcasses <- raw_carcass %>%
  filter(AboveWeir == 'Yes',
         Sex == 'Male') %>%
  summarise(us_male_carcass = sum(Count)) %>%
  right_join(f_us_carcass)
#------------------------------------------------------------------------------
#   Calculating Percent Females for Downstream (ds) of weir 
f_ds_carcass <- raw_carcass %>%    
  filter(AboveWeir == 'No', 
         Sex == 'Female') %>%       
  summarise(ds_female_carcass = sum(Count))

ds_carcasses <- raw_carcass %>%
  filter(AboveWeir == 'No',
         Sex == 'Male') %>%
  summarise(ds_male_carcass = sum(Count)) %>%
  right_join(f_ds_carcass)
#------------------------------------------------------------------------------
## Join Sex, Origin, & Prespawn Mortality
carcass_origin_sex <- full_join(us_carcasses, ds_carcasses)%>%
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
# save(carcass_origin_sex, file = './data/carcass_origin_sex.Rda')

