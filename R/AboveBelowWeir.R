#------------------------------------------------------------------------------
# Creating a table tying 'Move To'  to Above or Below weir
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 10/3/18
#------------------------------------------------------------------------------

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
