#------------------------------------------------------------------------------
#      Chinook: Joining SGS Data with FINS Data for Mark/Recap Estimates
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/6/18
#------------------------------------------------------------------------------
# load packages
library(tidyverse)
library(readxl)
library(RODBC)
#------------------------------------------------------------------------------
# Create connection with database
con <- odbcConnect('sgs_master', uid = 'guest', pwd = 'guest') # named dsn in admin tools.
#------------------------------------------------------------------------------
#   Extract SQL carcass and transect database table  via above (con)nection
sql_carcassdetail <- sqlFetch(con, 'carcass_detail')
sql_transect_metadata <- sqlFetch(con, 'transect_metadata')
#------------------------------------------------------------------------------
# Filter and join transect metadata to carcass detail
temp <- sql_transect_metadata %>%
  select(StreamName, POP_NAME, TributaryTo, TransectName, AboveWeir, WeirSiteName)

temp <- left_join(sql_carcassdetail, temp) %>%
  mutate(Trap_Year = SurveyYear)

npt_carcass <- temp %>%
  filter(AboveWeir == 'Yes') %>%  # filter for above weir carcasses
  group_by(StreamName, POP_NAME, Trap_Year, AboveWeir, Recapture) %>%
  separate(WeirSiteName, into = c("StreamName", "Trap"), sep = " Weir")
#------------------------------------------------------------------------------
# Summarize npt_carcass (NPT SGS) for captures and recaptures
carcass_recaps <- npt_carcass  %>%
  filter(Recapture == 'Yes') %>%
  group_by(StreamName, POP_NAME, Trap_Year) %>%
  summarise(carcass_recaptures = n())            

carcass_caps <- npt_carcass  %>%
  filter(Recapture == 'No') %>%
  group_by(StreamName, POP_NAME, Trap_Year) %>%
  summarise(carcass_unmarked = n())            

tmpnpt_sgs <- left_join(carcass_recaps, carcass_caps)
#------------------------------------------------------------------------------
# Import/Summarize captures and recaptures for LOSTINE RIVER (ODFW SGS)
rawODFWdata <- read_excel('./data/ODFW SGS 1998-2018.xlsx')

# import Lostine Mark/Tag Protocol
lostine_protocol <- read_excel('./data/Lostine River Weir Mark and Tag Protocol.xlsx')

# set dataframe to issue POP to lostine
POP_NAME <- c('Wallowa River')

weir <- c('Lostine River Weir')

POP_lostine <- data.frame(POP_NAME, weir)

lostine_protocol <- left_join(lostine_protocol, POP_lostine)
#------------------------------------------------------------------------------
# modify ODFW SGS data to have matching fields with 'fins_data' and Marking Protocol
ODFWSGS <- rawODFWdata %>%
  mutate(Trap_Year = Year,
         StreamName = River) %>%
  filter(StreamName == 'Lostine River')

# GRSME: filter for Radio Tags and Carcass Recaps (LOP Present)
tmpODFWSGS <- left_join(ODFWSGS, lostine_protocol) %>%
  mutate(Recap = ifelse(str_detect(`Opercle Punch Type`, 'LOP'), TRUE, FALSE)) %>%    # only X LOP are considered Recaps
  filter(!grepl('Radio', Comments, ignore.case = TRUE))

#------------------------------------------------------------------------------
# Summarize (ODFW) Lostine River SGS for captures and recaptures ***ABOVE WEIR***
aboveweir.list <- c('Above Weir', 'Diversion', 'Lostine Weir') 

lostine_captures <- tmpODFWSGS %>%
  filter(Recap == 'FALSE',
         AboveOrBelowWeir %in%  aboveweir.list) %>%    # does this filter include the correct carcasses?
  group_by(StreamName, Trap_Year) %>%
  summarise(carcass_unmarked = n()) 

lostine_recaps <- tmpODFWSGS %>%
  filter(Recap == 'TRUE',
         AboveOrBelowWeir %in%  aboveweir.list) %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(carcass_recaptures = n())

lostine_sgs <- left_join(lostine_captures, lostine_recaps)
#------------------------------------------------------------------------------
# Join NPT SGS and ODFW (Lostine) SGS summaries
sgs_cr <- bind_rows(tmpnpt_sgs, lostine_sgs)

#==============================================================================
#------------------------------------------------------------------------------
# Get number of upstream fish that were released with a mark
chinook_marks <- fins_data %>%
  filter(Species == 'Chinook') %>%
#  filter(`Moved To` == 'Upstream') %>%   #replaced by 'above_below'
  filter(above_below == 'Above') %>%
#  filter(Marks == "TRUE") %>%     # Indicates it was given ANY of the marks/tags used that year.
  filter(Recap != 'TRUE') %>%    # *** Lostine can have Upstream moving Recaps (Recycled/Fisheries Fish)
  group_by(weir, StreamName, Trap_Year) %>%
  summarise(weir_marks = sum(Count))

#------------------------------------------------------------------------------
# Join summarized weir mark data with summarized SGS data, calculate estimates
chinook_mcr <- left_join(chinook_marks, sgs_cr) %>%
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
save(chinook_mcr, file = './data/chinook_mcr.Rda')
#------------------------------------------------------------------------------
# Save chinook_mcr as a CSV
write.csv(chinook_mcr, file = './data/chinook_mcr.csv')
#------------------------------------------------------------------------------
# Remove temporary clutter from workspace
rm(tmpODFWSGS, lostine_captures, lostine_recaps, lostine_protocol, carcass_mod, 
   carcass_recaps, carcass_caps, chinook_marks, sgs_cr, tmpnpt_sgs, tmpODFWSGS,
   lostine_sgs, rawODFWdata)
