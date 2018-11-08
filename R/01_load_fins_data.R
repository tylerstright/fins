#------------------------------------------------------------------------------
# Load Fins data (corrected spreadsheet) that has been altered to be consistent
# across all projects and traps.
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 7/30/18
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(RODBC)

#------------------------------------------------------------------------------
# load FINS data.
data <- read_excel('./data/NPT_Research_FINS_Master.xlsx')

# load NPT Mark/Tag Protocol. label df: mt_protocol
mt_protocol <- read_excel('./data/NPT Mark and Tag Protocol.xlsx')

#------------------------------------------------------------------------------
# Create dataframe for `StreamName` and 'POP_NAME' associated with `weir`
weir <- c('Lolo Creek Weir', 
          'Joseph Creek Weir', 
          'Camp Creek Weir',
          'Imnaha River Weir', 
          'Freezeout Creek Weir', 
          'Gumboot Creek Weir', 
          'Dry Creek Weir', 
          'Johnson Creek Weir', 
          'NPT Hatchery Trap', 
          'Lostine River Weir', 
          'Newsome Creek Weir', 
          'Lower Granite Dam Trap',
          'Bradford Weir (Lolo Creek)', 
          'Upper Lolo Creek Weir', 
          'Lower Granite Dam', 
          'Secesh DIDSON', 
          'SFSR Weir')
StreamName <- c('Lolo Creek',  
                'Joseph Creek', 
                'Camp Creek', 
                'Imnaha River', 
                'Freezeout Creek', 
                'Gumboot Creek', 
                'Dry Creek', 
                'Johnson Creek', 
                'NPT Hatchery Trap', 
                'Lostine River', 
                'Newsome Creek', 
                'Snake River', 
                'Lolo Creek', 
                'Lolo Creek', 
                'Snake River', 
                'Secesh River', 
                'South Fork Salmon River') 
POP_NAME <- c('Lolo Creek', # Lolo Creek Weir
              'Joseph Creek',  # Joseph Creek Weir 
              'Imnaha River',  # Camp Creek Weir 
              'Imnaha River',  # Imnaha River Weir
              'Imnaha River',  # Freezeout Creek Weir
              'Imnaha River',  # Gumboot Creek Weir
              'Imnaha River',  # Dry Creek Weir
              'East Fork South Fork Salmon River', #Johnson Creek Weir 
              'Snake River Lower Mainstem', # NPT Hatchery Trap
              'Wallowa River',  #Lostine River Weir 
              'Upper South Fork Clearwater', #Newsome Creek Weir 
              'Lolo Creek', # Lower Granite Dam Trap
              'Lolo Creek', # Bradford Weir (Lolo Creek) 
              'Lolo Creek', # Upper Lolo Creek Weir
              'Snake River Lower Mainstem', # Lower Granite Dam
              'Secesh River', # Secesh DIDSON Weir
              'South Fork Salmon River') # SFSR Weir
streams <- data.frame(weir, StreamName, POP_NAME)
#------------------------------------------------------------------------------
# Create Above/Below weir designation for 'Moved To' locations

`Moved To` <- c("Camp Creek: Above Weir",
                "Camp Creek: Below Weir",
                "Clearwater River: Cherry Lane Boat Launch",
                "Clearwater River: Clearwater River at NPTH Trap",
                "Clearwater River: Lenore Boat Launch",
                "Clearwater River: NPTH Site 1705",
                "Dry Creek: Above Weir",
                "Dry Creek: Below Weir",
                "EFSFSR: Meadow Creek",
                "Freezeout Creek: Above Weir",
                "Freezeout Creek: Below Weir",
                "Gumboot Creek: Above Weir",
                "Gumboot Creek: Below Weir",
                "Imnaha River: Above Weir",
                "Imnaha River: Below Weir",
                "Johnson Creek: Above Weir",
                "Johnson Creek: Below Weir",
                "Johnson Creek: Landmark Bridge",
                "Johnson Creek: Lunch Creek",
                "Joseph Creek: Above Weir",
                "Joseph Creek: Below Weir",
                "Lolo Creek: Above Weir",
                "Lolo Creek: Below Weir",
                "Lookingglass Fish Hatchery",
                "Lostine River: Above Weir",
                "Lostine River: Acclimation Facility",
                "Lostine River: Below Weir",
                "Lostine River: Unknown Upstream of Weir",
                "Mahogany Creek: Above Weir",
                "Mahogany Creek: Below Weir",
                "McCall Fish Hatchery Inbox", 
                "MFH: NPT JCAPE RP 3A",
                "MFH: NPT JCAPE RP 3B",
                "Newsome Creek: Above Weir",
                "Newsome Creek: Below Weir",
                "Nez Perce Tribe",
                "NPT Hatchery Inbox",
                "NPTH Pond 1",
                "NPTH Pond 2",
                "Outplant - Bear Creek",
                "Outplant - Clearwater River",
                "Outplant - Lower Wallowa River",
                "Outplant - Meadow Creek (SFCLW)",
                "Outplant - O'Hara Creek",
                "Outplant - Selway River",
                "Outplant - South Fork Clearwater",
                "Outplant - Unknown",
                "Outplant - Wallowa River",
                "Outplant - Wallowa River: Eggleson Road Bridge",
                "Outplant - Wallowa River: Lower Diamond Road",
                "Outplant - Wallowa River: Russel Lane Bridge",
                "Outplant - Wallowa River: School Flat Road Bridge",
                "Outplant - Wallowa River: Sunrise Road Bridge",
                "Outplant - Wallowa River: Wade Gulch Lane Bridge",
                "RRFH: NPT JCAPE HP 3",
                "SFSR Satellite: NPT JCAPE CIRC",
                "SFSR Satellite: NPT JCAPE HP 1",
                "SFSR Satellite: NPT JCAPE HP 2",
                "SFSR Satellite: NPT JCAPE HP 2B",
                "Unknown",
                "Wallowa Fish Hatchery")


above_below <- c("Above", # Camp Creek: Above Weir
                 "Below", # Camp Creek: Below Weir
                 "Release", # Clearwater River: Cherry Lane Boat Launch
                 "Release", # Clearwater River: Clearwater River at NPTH Trap
                 "Release", # Clearwater River: Lenore Boat Launch
                 "Release", # Clearwater River: NPTH Site 1705
                 "Above", # Dry Creek: Above Weir
                 "Below", # Dry Creek: Below Weir
                 "Outplant", # EFSFSR: Meadow Creek
                 "Above", # Freezeout Creek: Above Weir
                 "Below", # Freezeout Creek: Below Weir
                 "Above", # Gumboot Creek: Above Weir
                 "Below", # Gumboot Creek: Below Weir
                 "Above", # Imnaha River: Above Weir
                 "Below", # Imnaha River: Below Weir
                 "Above", # Johnson Creek: Above Weir
                 "Below", # Johnson Creek: Below Weir
                 "Above", # Johnson Creek: Landmark Bridge
                 "Above", # Johnson Creek: Lunch Creek
                 "Above", # Joseph Creek: Above Weir
                 "Below", # Joseph Creek: Below Weir
                 "Above", # Lolo Creek: Above Weir
                 "Below", # Lolo Creek: Below Weir
                 "Hatchery", # Lookingglass Fish Hatchery
                 "Above", # Lostine River: Above Weir
                 "Above", # Lostine River: Acclimation Facility
                 "Below", # Lostine River: Below Weir
                 "Above", # Lostine River: Unknown Upstream of Weir
                 "Above", # Mahogany Creek: Above Weir
                 "Below", # Mahogany Creek: Below Weir
                 "Hatchery", # McCall Fish Hatchery Inbox
                 "Pond", # MFH: NPT JCAPE RP 3A
                 "Pond", # MFH: NPT JCAPE RP 3B
                 "Above", # Newsome Creek: Above Weir
                 "Below", # Newsome Creek: Below Weir
                 "Distribution", # Nez Perce Tribe
                 "Hatchery", # NPT Hatchery Inbox
                 "Pond", # NPTH Pond 1
                 "Pond", # NPTH Pond 2
                 "Outplant", # Outplant - Bear Creek
                 "Outplant", # Outplant - Clearwater River
                 "Outplant", # Outplant - Lower Wallowa River
                 "Outplant", # Outplant - Meadow Creek (SFCLW)
                 "Outplant", # Outplant - O'Hara Creek
                 "Outplant", # Outplant - Selway River
                 "Outplant", # Outplant - South Fork Clearwater
                 "Outplant", # Outplant - Unknown
                 "Outplant", # Outplant - Wallowa River
                 "Outplant", # Outplant - Wallowa River: Eggleson Road Bridge
                 "Outplant", # Outplant - Wallowa River: Lower Diamond Road
                 "Outplant", # Outplant - Wallowa River: Russel Lane Bridge
                 "Outplant", # Outplant - Wallowa River: School Flat Road Bridge
                 "Outplant", # Outplant - Wallowa River: Sunrise Road Bridge
                 "Outplant", # Outplant - Wallowa River: Wade Gulch Lane Bridge
                 "Pond", # RRFH: NPT JCAPE HP 3
                 "Pond", # SFSR Satellite: NPT JCAPE CIRC
                 "Pond", # SFSR Satellite: NPT JCAPE HP 1
                 "Pond", # SFSR Satellite: NPT JCAPE HP 2
                 "Pond", # SFSR Satellite: NPT JCAPE HP 2B
                 NA, # Unknown
                 "Hatchery/Distribution") # Wallowa Fish Hatchery

AB_weir <- tibble(`Moved To`, above_below)
#------------------------------------------------------------------------------
# Combine fins data w/ protocol, StreamName, AB_weir.
fins_data <- data %>%
  separate(Trap, into = c("weir", "Trap"), sep = " - ") %>%
  mutate(Trap_Year = year(`Trapped Date`)) %>%
  left_join(mt_protocol) %>%
  left_join(streams) %>%
  left_join(AB_weir) %>%   
  mutate(Marks = ifelse(str_detect(`Applied Marks`, mark_type), TRUE,
                        ifelse(str_detect(`Applied Tags`, tag_type), TRUE,
                               ifelse(str_detect(`Applied PIT`, tag_type), TRUE, FALSE))))
#------------------------------------------------------------------------------
# Save modified fins data
# save(fins_data, file = './data/fins_data.Rda')
# write.csv(fins_data, file = './data/fins_data.csv')
