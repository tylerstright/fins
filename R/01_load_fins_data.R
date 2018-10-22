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
data <- read_excel('./data/MODIFIED UNIQUE All_Research_FINS_data_standardized.xlsx')

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

`Moved To` <- c("Upstream", 
                "Lookingglass Fish Hatchery Inbox", 
                "Outplant - Wallowa River: Wade Gulch Lane Bridge",
                "Outplant - Bear Creek", 
                "Downstream", 
                "Outplant - Lostine River: Unknown Upstream of Weir", 
                "NPTH Pond 1", 
                "NPTH Pond 2", 
                "NO FISH", 
                "Unknown", 
                "Nez Perce Tribe", 
                "NPT Hatchery Inbox", 
                "Wallowa Fish Hatchery Inbox", 
                "NPT JCAPE: HP 1", 
                "NPT JCAPE: HP 2", 
                "SFSR: Circ", 
                "Outplant - Johnson Creek near Lunch Creek",
                "McCall Fish Hatchery Inbox", 
                "Rapid River Fish Hatchery Inbox", 
                "Outplant - Upper Johnson Creek- above weir", 
                "Outplant - Lower Wallowa River", 
                "Outplant - Lostine River: Acclimation Facility", 
                "Outplant - Wallowa River: School Flat Road Bridge", 
                "Outplant - Wallowa River", 
                "Outplant - Lenore Boat Launch", 
                "Outplant - Clearwater River at NPTH Trap", 
                "Outplant - Cherry Lane Boat Launch", 
                "Outplant - NPTH Site 1705", 
                "Outplant - East Fork South Fork Salmon River")


above_below <- c("Above",  # Upstream
                 NA, # Lookingglass Fish Hatchery Inbox
                 "Below", # Outplant - Wallowa River: Wade Gulch Lane Bridge
                 "Below", # Outplant - Bear Creek
                 "Below", # Downstream
                 "Above", # Outplant - Lostine River: Unknown Upstream of Weir
                 NA, # NPTH Pond 1
                 NA, # NPTH Pond 2
                 NA, # NO FISH
                 NA, # Unknown
                 NA, # Nez Perce Tribe
                 NA, # NPT Hatchery Inbox
                 NA, # Wallowa Fish Hatchery Inbox
                 NA, # NPT JCAPE: HP 1
                 NA, # NPT JCAPE: HP 2
                 NA, # SFSR: Circ
                 "Above", # Outplant - Johnson Creek near Lunch Creek
                 NA, # McCall Fish Hatchery Inbox
                 NA, # Rapid River Fish Hatchery Inbox
                 "Above", # Outplant - Upper Johnson Creek- above weir
                 "Below", # Outplant - Lower Wallowa River
                 "Above", # Outplant - Lostine River: Acclimation Facility
                 "Below", # Outplant - Wallowa River: School Flat Road Bridge
                 "Below", # Outplant - Wallowa River
                 "Below", # Outplant - Lenore Boat Launch
                 "Below", # Outplant - Clearwater River at NPTH Trap
                 "Below", # Outplant - Cherry Lane Boat Launch
                 "Below", # Outplant - NPTH Site 1705
                 "Below") # Outplant - East Fork South Fork Salmon River

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
