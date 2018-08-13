#------------------------------------------------------------------------------
#  Joining the ODFW SGS data for Chinook Mark/Recap Estimates (Lostine River)
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 8/13/18
#------------------------------------------------------------------------------# import ODFW SGS data
rawODFWdata <- read_excel('C:/TylerS Sync/FINS Info/R_Fins/data/ODFW SGS 1998-2018.xlsx')

#------------------------------------------------------------------------------
# import lostine Mark/Tag Protocol

lostine_protocol <- read_excel('./data/Lostine River Weir Mark and Tag Protocol.xlsx')
# OR Tyler's
lostine_protocol <- read_excel('C:/TylerS Sync/FINS Info/R_Fins/data/Lostine River Weir Mark and Tag Protocol.xlsx')

#------------------------------------------------------------------------------
# modify ODFW SGS data to have matching fields with 'fins_data'

ODFWSGS <- rawODFWdata %>%
  mutate(Trap_Year = Year,
         StreamName = River) %>%
  filter(StreamName == 'Lostine River')

# join with lostine mark/tag protocol, create "Recap" field based on existing marks
tmpODFWSGS <- left_join(ODFWSGS, lostine_protocol) %>%
  mutate(Recap = ifelse(str_detect(`Opercle Punch Type`, mark_type), TRUE, FALSE))

#------------------------------------------------------------------------------
# calculate 'captured' carcasses per year    ****2001-2008 have no "Marks" in Protocol****
lostine_captures <- tmpODFWSGS %>%
  filter(Recap == 'FALSE') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_captures = n()) 


#------------------------------------------------------------------------------
# calculate 'recaptured' carcasses per year
lostine_recaps <- tmpODFWSGS %>%
  filter(Recap == 'TRUE') %>%
  group_by(StreamName, Trap_Year) %>%
  summarise(c_recaps = n())

#------------------------------------------------------------------------------
# join tables and join to chinook_mcr

lostine_sgs <- left_join(lostine_captures, lostine_recaps)

tmp2 <- left_join(tmp, lostine_sgs)

tempfullsgs <- left_join(chinook_mcr, lostine_sgs)

