#------------------------------------------------------------------------------
#      Chinook: Joining SGS Data with FINS Data for Mark/Recap Estimates
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/6/18
#------------------------------------------------------------------------------
# Filter and join transect metadata to carcass detail
npt_carcassraw <- sql_transect_metadata %>%
  select(StreamName, POP_NAME, TributaryTo, TransectName, AboveWeir, WeirSiteName) %>%
  right_join(sql_carcassdetail) %>%
  mutate(Trap_Year = SurveyYear) %>%
  filter(AboveWeir == 'Yes') %>%  # filter for above weir carcasses
  group_by(StreamName, POP_NAME, Trap_Year, AboveWeir, Recapture) %>%
  separate(WeirSiteName, into = c("StreamName", "Trap"), sep = " Weir")
#------------------------------------------------------------------------------
# Summarize npt_carcassraw (NPT SGS) for captures and recaptures
carcass_recaps <- npt_carcassraw  %>%
  filter(Recapture == 'Yes') %>%
  group_by(StreamName, POP_NAME, Trap_Year) %>%
  summarise(carcass_recaptures = n())            

carcass_summary <- npt_carcassraw  %>%
  filter(Recapture == 'No') %>%
  group_by(StreamName, POP_NAME, Trap_Year) %>%
  summarise(carcass_unmarked = n()) %>%
  right_join(carcass_recaps)

#------------------------------------------------------------------------------
# import Lostine Mark/Tag Protocol, associate stream and POP name
lostine_protocol <- read_excel('./data/Lostine River Weir Mark and Tag Protocol.xlsx') %>%
  left_join(streams)

# Import/Summarize captures and recaptures for LOSTINE RIVER (ODFW SGS)
ODFWSGS <- read_excel('./data/ODFW SGS 1998-2018.xlsx') %>%
  mutate(Trap_Year = Year,
         StreamName = River) %>%
  filter(StreamName == 'Lostine River') %>%
  left_join(lostine_protocol) %>%
  mutate(Recap = ifelse(str_detect(`Opercle Punch Type`, 'LOP'), TRUE, FALSE)) %>%    # only X LOP are considered Recaps
  filter(!grepl('Radio', Comments, ignore.case = TRUE))

#------------------------------------------------------------------------------
# Summarize (ODFW) Lostine River SGS for captures and recaptures *ABOVE WEIR*
aboveweir.list <- c('Above Weir', 'Diversion', 'Lostine Weir') 

lostine_captures <- ODFWSGS %>%
  filter(Recap == 'FALSE',
         AboveOrBelowWeir %in%  aboveweir.list) %>%    # does this filter include the correct carcasses?
  group_by(StreamName, Trap_Year) %>%
  summarise(carcass_unmarked = n()) 

lostine_carcass <- ODFWSGS %>%
  filter(Recap == 'TRUE',
         AboveOrBelowWeir %in%  aboveweir.list) %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(carcass_recaptures = n()) %>%
  right_join(lostine_captures)

#------------------------------------------------------------------------------
# Join NPT SGS and ODFW (Lostine) SGS summaries
combined_sgs <- bind_rows(carcass_summary, lostine_carcass)

#==============================================================================
# Get number of upstream fish that were released ABOVE weir, ALIVE, with a mark
chinook_marks <- fins_data %>%
  filter(Species == 'Chinook') %>%
  filter(`Living Status` == 'Alive') %>%
  filter(above_below == 'Above') %>%
#  filter(Marks == "TRUE") %>%     # Indicates it was given ANY of the marks/tags used that year.
  filter(Recap != 'TRUE') %>%    # *** Lostine can have Upstream moving Recaps (Recycled/Fisheries Fish)
  group_by(weir, StreamName, Trap_Year) %>%
  summarise(weir_marks = sum(Count))

#------------------------------------------------------------------------------
# Join summarized weir mark data with summarized SGS data, calculate estimates
chinook_mcr <- left_join(chinook_marks, combined_sgs) %>%
  select(weir, StreamName, POP_NAME, Trap_Year, weir_marks, carcass_recaptures, carcass_unmarked) %>%
  mutate(carcass_unmarked = ifelse(is.na(carcass_unmarked), 0, carcass_unmarked),
         carcass_recaptures = ifelse(is.na(carcass_recaptures), 0, carcass_recaptures),
         n1 = weir_marks,
         n2 = carcass_recaptures + carcass_unmarked,
         m2 = carcass_recaptures,
         Nhat = (((n1 + 1)*(n2 + 1))/(m2 + 1)) - 1,
         Vhat = ((n1+1)*(n2+1)*(n1-m2)*(n2-m2))/((m2+1)^2*(m2+2)),
         lower95 = Nhat - 1.96*sqrt(Vhat),
         upper95 = Nhat + 1.96*sqrt(Vhat))

#==============================================================================
#------------------------------------------------------------------------------
#  ggplot(chinook_mcr, aes(x = Trap_Year, y = Nhat, colour = weir)) +
#    geom_point(size = 2, position = position_dodge(width = .2)) +
#    geom_errorbar(aes(ymin = lower95, ymax = upper95), position = position_dodge(width = .2)) +
#    facet_wrap(~weir, scale = 'free_y', drop = TRUE) +
#    theme_bw()
#------------------------------------------------------------------------------
# Save chinook_mcr data
# save(chinook_mcr, file = './data/chinook_mcr.Rda')
#------------------------------------------------------------------------------
# Save chinook_mcr as a CSV
# write.csv(chinook_mcr, file = './data/chinook_mcr.csv')

