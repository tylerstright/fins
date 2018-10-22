#------------------------------------------------------------------------------
#      Redds/Transect data import and modification
#
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/16/18
#------------------------------------------------------------------------------
# Create connection with SQL database
con <- odbcConnect('sgs_master', uid = 'guest', pwd = 'guest') # named dsn in admin tools.

# Extract SQL redd and transect database table via above (con)nection
sql_reddsummary <- sqlFetch(con, 'redd_summary')
sql_transect_metadata <- sqlFetch(con, 'transect_metadata')
#------------------------------------------------------------------------------
# Join redd, transect, and streams[df]
raw_redds <- left_join(sql_transect_metadata, streams) %>%
  group_by(StreamName, weir, POP_NAME, TransectName, AboveWeir) %>%
  left_join(sql_reddsummary) %>%
  mutate(Trap_Year = SurveyYear) %>%
  ungroup() %>%
  group_by(POP_NAME, Trap_Year)

#------------------------------------------------------------------------------
# Calculating number of Redds Above and Below Weir by POP_NAME

downstream_redds <- raw_redds %>%
  filter(AboveWeir == 'No') %>%
  group_by(POP_NAME, Trap_Year) %>%
  summarise(redds_downstream = sum(NewReddCount))

upstream_redds <- raw_redds %>%
  filter(AboveWeir == 'Yes') %>%
  group_by(POP_NAME, Trap_Year) %>%
  summarise(redds_upstream = sum(NewReddCount))

npt_redds <- left_join(downstream_redds, upstream_redds) %>%
  select(POP_NAME, Trap_Year, redds_upstream, redds_downstream) #%>%
  mutate(redds_upstream = ifelse(is.na(redds_upstream), 0, redds_upstream),
         redds_downstream = ifelse(is.na(redds_downstream), 0, redds_downstream),
         total_redds = redds_downstream + redds_upstream)
#------------------------------------------------------------------------------
# save(npt_redds, file = './data/npt_redds.Rda')
