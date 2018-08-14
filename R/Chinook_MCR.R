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
#   Extract carcass database table from SQL Server via above (con)nection
      sql_carcassdetail <- sqlFetch(con, 'carcass_detail')
#------------------------------------------------------------------------------
        # Save ****RAW**** Data
          save(sql_carcassdetail, file = './data/sql_carcassdetailraw.Rda')
#------------------------------------------------------------------------------

# Summarize carcass_detail (NPT SGS) for captures and recaptures

carcass_mod <- sql_carcassdetail  %>%
  mutate(Trap_Year = SurveyYear)

carcass_recaps <- carcass_mod  %>%
  filter(Recapture == 'Yes') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_recaps = n())            

carcass_caps <- carcass_mod  %>%
  filter(Recapture == 'No') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_captures = n())            
            
tmpnpt_sgs <- left_join(carcass_recaps, carcass_caps)

rm(carcass_mod, carcass_recaps, carcass_caps)

#------------------------------------------------------------------------------
# Import/Summarize captures and recaptures for LOSTINE RIVER (ODFW SGS)
rawODFWdata <- read_excel('./data/ODFW SGS 1998-2018.xlsx')

# import Lostine Mark/Tag Protocol
lostine_protocol <- read_excel('./data/Lostine River Weir Mark and Tag Protocol.xlsx')
#------------------------------------------------------------------------------
# modify ODFW SGS data to have matching fields with 'fins_data' and Marking Protocol
ODFWSGS <- rawODFWdata %>%
  mutate(Trap_Year = Year,
         StreamName = River) %>%
  filter(StreamName == 'Lostine River')

tmpODFWSGS <- left_join(ODFWSGS, lostine_protocol) %>%
  mutate(Recap = ifelse(str_detect(`Opercle Punch Type`, mark_type), TRUE, FALSE))
#Check the NA values

# Summarize (ODFW) Lostine River SGS for captures and recaptures
lostine_captures <- tmpODFWSGS %>%
  filter(Recap == 'FALSE') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_captures = n()) 

lostine_recaps <- tmpODFWSGS %>%
  filter(Recap == 'TRUE') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_recaps = n())

lostine_sgs <- left_join(lostine_captures, lostine_recaps)

rm(tmpODFWSGS, lostine_captures, lostine_recaps)
#------------------------------------------------------------------------------
# Join NPT SGS and ODFW (Lostine) SGS summaries
sgs_cr <- bind_rows(tmpnpt_sgs, lostine_sgs)

#------------------------------------------------------------------------------
# Get number of upstream fish that were released with a mark
marks <- fins_data %>%
  filter(Species == 'Chinook') %>%
  filter(`Moved To` == 'Upstream') %>%
  filter(Marks == "TRUE") %>%
  filter(Recap != 'TRUE') %>%
  group_by(weir, StreamName, Trap_Year) %>%
  summarise(weir.marks = sum(Count))

#------------------------------------------------------------------------------
# Get number of downstream moving fish without marks = Captures - doesn't include recaptures
captures <- fins_data %>%
  filter(Species == 'Chinook') %>%
  filter(`Moved To` == 'Downstream') %>%
  #filter(Marks == FALSE) %>% # no applied marks
  filter(Recap == FALSE) %>% # first fish handle
  group_by(weir, StreamName, Trap_Year) %>%
  summarise(weir.captures = sum(Count))

chinook_mc <- left_join(marks, captures)

#------------------------------------------------------------------------------
# Join summarized weir data with summarized SGS data and calculate estimates
chinook_mcr <- left_join(chinook_mc, sgs_cr) %>%
  select(StreamName, Trap_Year, weir.marks, weir.captures, c_recaps, c_captures) %>%
  mutate(Captures = ifelse(is.na(c_captures), 0, c_captures),
         n1 = weir.marks,
         n2 = c_recaps + c_captures,
         m2 = c_recaps,
         Nhat = (((n1 + 1)*(n2 + 1))/(m2 + 1)) - 1,
         Vhat = ((n1+1)*(n2+1)*(n1-m2)*(n2-m2))/((m2+1)^2*(m2+2)),
         lower95 = Nhat - 1.96*sqrt(Vhat),
         upper95 = Nhat + 1.96*sqrt(Vhat))

#  ggplot(chinook_mcr, aes(x = Trap_Year, y = Nhat, colour = weir)) +
#    geom_point(size = 2, position = position_dodge(width = .2)) +
#    geom_errorbar(aes(ymin = lower95, ymax = upper95), position = position_dodge(width = .2)) +
#    facet_wrap(~weir, scale = 'free_y', drop = TRUE) +
#    theme_bw()

# Save chinook_mcr as a CSV
write.csv(chinook_mcr, file = './data/chinook_mcr.csv')
