#------------------------------------------------------------------------------
# Load Fins data (corrected spreadsheet) that has been altered to be consistent
# across all projects and traps.
#
# Author: Tyler Stright
# Created: 7/30/18
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

# load FINS data.
data <- read_excel('C:/R input/MODIFIED UNIQUE All_Research_FINS_data_standardized.xlsx')

# load NPT Mark/Tag Protocol. label df: mt_protocol
mt_protocol <- read_excel('C:/R input/NPT Mark and Tag Protocol.xlsx')

#------------------------------------------------------------------------------
# Combine fins data with protocol and create useable dataset
#------------------------------------------------------------------------------
fins_data <- data %>%
  mutate(Trap_Year = year(`Trapped Date`)) %>%
  separate(Trap, into = c("weir", "Trap"), sep = " - ") %>%
  left_join(mt_protocol) %>%
  mutate(Marks = ifelse(str_detect(`Applied Marks`, mark_type), TRUE,
                        ifelse(str_detect(`Applied Tags`, tag_type), TRUE,
                               ifelse(str_detect(`Applied PIT`, tag_type), TRUE, FALSE))))

#------------------------------------------------------------------------------
# Combine fins data with protocol and create useable dataset
#------------------------------------------------------------------------------
save(fins_data, file = 'C:/R input/fins_data.Rda')
save(fins_data, file = './data/fins_data.Rda')
      
#  paste0("SELECT * FROM data WHERE Facility= NPT GRSME Program AND Species = Chinook")
  
#### this filters to show all Outplants in streams for all years.
outplant_data<- tempdata %>%
  #filter(Facility == 'NPT GRSME Program') %>%
  #filter(Species == 'Chinook') %>%
  #filter(Trap_Year == 2009) %>%
  filter(grepl('Outplant', `Moved To`)) %>% #`Moved To` == Outplant)
  group_by(Facility, Trap, Trap_Year, Species, `Moved To`, disp_purp) %>%
  summarise(count = n()) %>%
  separate(`Moved To`, into = c('Action', 'Release_location'), sep = " - ") %>%
  separate(Release_location, into = c('Release_stream', 'location'), sep = ": ") %>%
  #rename(Release_location = location) %>%
  #select(Facility:Species, Release_stream:disp_purp) %>%
  select(-Action, Release_location = location) %>%
  #select(disp_purp, everything()) %>%   # another helper function is: contains("pattern")
#            mean = mean(Length, na.rm = TRUE)) %>%
  arrange(Trap_Year) %>%
  ungroup() %>%
  group_by(Facility, Trap, Trap_Year, Species, Release_stream) %>%
  summarise(n = sum(count)) # %>%
  #ggplot(aes(x = Trap_Year, y = n,  fill = Release_stream, colour = Release_stream)) +
#geom_bar(stat = 'identity') +
  #geom_line() +
 # facet_wrap(~Trap) +
  #theme_bw()

# This will take the outplant_data and run it through ggplot to create a graph
outplant_bars<- outplant_data %>%
ggplot(aes(x = Trap_Year, y = n,  fill = Release_stream, colour = Release_stream)) +
  geom_bar(stat = 'identity') +
  #geom_line() +
  facet_wrap(~Trap) +
  theme_bw()

# To save above graph:
ggsave("outplant_bars.png", outplant_bars, path = "./data")



  
  
  #gather(key = key, value = value, Recap, Marks) %>%
  #mutate(type = ifelse(key == 'Marks', 'Mark', key),
  #       type = ifelse(key == 'Recap' & value == 'TRUE', 'Recap',
  #                     ifelse(key == 'Recap' & value == 'FALSE', 'Unmark', key)))
  #mutate(Recap = ifelse(is.na(Recap), "FALSE", Recap)) %>%  #if there is an NA in Recap, make it FALSE. Otherwise don't change it.




