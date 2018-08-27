#------------------------------------------------------------------------------
#      Redds/Transect data import and modification for estimates
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/16/18
#------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(readxl)
library(RODBC)
#------------------------------------------------------------------------------
# Create connection with SQL database
con <- odbcConnect('sgs_master', uid = 'guest', pwd = 'guest') # named dsn in admin tools.

# Extract SQL redd and transect database table via above (con)nection
sql_reddsummary <- sqlFetch(con, 'redd_summary')
sql_transect_metadata <- sqlFetch(con, 'transect_metadata')
#------------------------------------------------------------------------------
# Create dataframe to add 'weir' to transect metadata based on POP_NAME
POP_NAME <- c('East Fork South Fork Salmon River', 'Lolo Creek','Snake River 
              Lower Mainstem', 'Upper South Fork Clearwater','Secesh River', 
              'South Fork Salmon River mainstem')
weir <- c('Johnson Creek Weir', 'Lolo Creek Weir', 'Lower Granite Dam', 
          'Newsome Creek Weir', 'Secesh DIDSON', 'SFSR Weir')
POP_weir <- data.frame(POP_NAME, weir)

# Join redd, transect, and POP_weir
sql_rtw <- left_join(sql_transect_metadata, POP_weir) %>%
  group_by(StreamName, weir, POP_NAME, TransectName, AboveWeir) %>%
  left_join(sql_reddsummary) %>%
  mutate(Trap_Year = SurveyYear) %>%
  ungroup() %>%
  group_by(POP_NAME, Trap_Year)
#------------------------------------------------------------------------------
# Calculating number of Redds Above and Below Weir by StreamName
# 
# ds_redds <- sql_rtw %>%
#   filter(AboveWeir == 'No') %>%
#   group_by(POP_NAME, weir, StreamName, Trap_Year) %>%
#   summarise(redds_downstream = sum(NewReddCount))
# 
# us_redds <- sql_rtw %>%
#   filter(AboveWeir == 'Yes') %>%
#   group_by(POP_NAME, weir, StreamName, Trap_Year) %>%
#   summarise(redds_upstream = sum(NewReddCount))
# 
# us_ds_redds <- left_join(ds_redds, us_redds) %>%
#   select(POP_NAME, weir, StreamName, Trap_Year, redds_upstream, redds_downstream) %>%
#   mutate(redds_upstream = ifelse(is.na(redds_upstream), 0, redds_upstream),
#          redds_downstream = ifelse(is.na(redds_downstream), 0, redds_downstream),
#          total_redds = redds_downstream + redds_upstream)
#
#------------------------------------------------------------------------------
# Calculating number of Redds Above and Below Weir by POP_NAME

ds_redds_POP <- sql_rtw %>%
  filter(AboveWeir == 'No') %>%
  group_by(POP_NAME, Trap_Year) %>%
  summarise(redds_downstream = sum(NewReddCount))

us_redds_POP <- sql_rtw %>%
  filter(AboveWeir == 'Yes') %>%
  group_by(POP_NAME, Trap_Year) %>%
  summarise(redds_upstream = sum(NewReddCount))

us_ds_redds_POP <- left_join(ds_redds_POP, us_redds_POP) %>%
  select(POP_NAME, Trap_Year, redds_upstream, redds_downstream) #%>%
  mutate(redds_upstream = ifelse(is.na(redds_upstream), 0, redds_upstream),
         redds_downstream = ifelse(is.na(redds_downstream), 0, redds_downstream),
         total_redds = redds_downstream + redds_upstream)
#------------------------------------------------------------------------------









#------------------------------------------------------------------------------
# # Join chinook_mcr and redd summary to estimate downstream abundance
# chinook_mcr_redds <- left_join(chinook_mcr, POPredd_summary) %>%
#   group_by(weir, StreamName, POP_NAME, Trap_Year) %>%
#   mutate(fishperredd_da = (redds_downstream*(Nhat/redds_upstream)),
#          fishperredd_var = ((redds_downstream/redds_upstream)^2)*Vhat)
