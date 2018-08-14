#------------------------------------------------------------------------------
# Data Request for Tom Cooney
# Ryan N. Kinzer
# Created: 09/07/17
#
# Data is stored on the NPT DFRM-SQL server and in the database:
# npt_sgs_08232017
#
#------------------------------------------------------------------------------
# Install libraries
#------------------------------------------------------------------------------
library(plyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(RODBC)

#------------------------------------------------------------------------------
# Create connection with database
#------------------------------------------------------------------------------
con <- odbcConnect('sgs_master', uid = 'ryank', pwd = 'rk2018') # named dsn in admin tools.
#------------------------------------------------------------------------------
# Extract database tables
#------------------------------------------------------------------------------
tbl_project <- sqlFetch(con, 'tbl_Projects')


qry_carcass <- sqlFetch(con, 'carcass_check_qry')
tbl_SampleData_CWT <- sqlFetch(con, 'tbl_SampleData-CWT')
tbl_SampleData_Fin <- sqlFetch(con, 'tbl_SampleData-Fin')
tbl_SampleData_PIT <- sqlFetch(con, 'tbl_SampleData-PIT')
tbl_SampleData_Scales <- sqlFetch(con, 'tbl_SampleData-Scales')
tbl_SampleData_VIE <- sqlFetch(con, 'tbl_SampleData-VIE')

odbcClose(con)
#------------------------------------------------------------------------------
# Join database tables
#------------------------------------------------------------------------------
sgs_dat <- qry_carcass %>%
  left_join(select(tbl_SampleData_CWT, Carcass_FK, CWT_TotalAge),
            by = c('Carcass_ID' = 'Carcass_FK')) %>%
  left_join(select(tbl_SampleData_Fin, Carcass_FK, Fin_TotalAge),
            by = c('Carcass_ID' = 'Carcass_FK')) %>%
  left_join(select(tbl_SampleData_PIT, Carcass_FK, PIT_TotalAge),
            by = c('Carcass_ID' = 'Carcass_FK')) %>%
  left_join(select(tbl_SampleData_Scales, Carcass_FK, Scale_TotalAge),
            by = c('Carcass_ID' = 'Carcass_FK')) %>%
  left_join(select(tbl_SampleData_VIE, Carcass_FK, VIE_TotalAge),
            by = c('Carcass_ID' = 'Carcass_FK')) %>%
  mutate(TotalAge = ifelse(!is.na(CWT_TotalAge), CWT_TotalAge,
                           ifelse(!is.na(PIT_TotalAge),PIT_TotalAge,
                                  ifelse(!is.na(VIE_TotalAge), VIE_TotalAge,
                                         ifelse(!is.na(Fin_TotalAge), Fin_TotalAge,
                                                ifelse(!is.na(Scale_TotalAge), Scale_TotalAge, NA))))),
         Project = str_pad(Project, 2, pad = '0'),
         tmp_id = ifelse(`SurveyYear*` >= 2016, SampleNumber, as.character(Historic_SampleNumber))) %>%
  separate(tmp_id, into = c('s1', 's2', 's3', 's4', 's5', 's6'), sep = '-', remove = FALSE) %>%
  mutate(tmp_id_2 = ifelse(!is.na(s6), s6,
                           ifelse(!is.na(s5), s5,
                                  ifelse(!is.na(s4), s4,
                                         ifelse(!is.na(s3), s3,
                                                ifelse(!is.na(s2),s2,
                                                       ifelse(!is.na(s1), s1, NA)))))),
         tmp_id_3 = str_pad(tmp_id_2, 4, pad = '0'),
         sample_id = ifelse(is.na(tmp_id_3), -999, tmp_id_3),
         #sample_id_sgs = str_pad(sample_id_sgs, 4, pad = '0'),
         sample_id_sgs = paste0(Project, '-', SurveyDate, '-', sample_id)) %>%
  dplyr::select(-tmp_id, -s1, -s2, -s3, -s4, -s5, -s6, -tmp_id_2, -tmp_id_3, -sample_id) %>%
  mutate(sgs_id = paste0('sgs',Carcass_FK),
         PITCode = as.character(PITCode),
         pit = ifelse(PITCode == '-999', NA, PITCode),
         opercle_id = str_pad(as.numeric(gsub("\\D", "", `Tags-PetersonDisc`)), 4, pad = '0'),
         year_opercle_id = ifelse(is.na(opercle_id), NA,
                                  paste0(`SurveyYear*`, '-', opercle_id)),
         fins_sgs_id = ifelse(!is.na(year_opercle_id), year_opercle_id,
                              ifelse(!is.na(pit), pit, sgs_id)),
         Species = revalue(Species, c('S-CHN' = 'SpSm Chinook',
                                      'F-CHN' = 'Fall Chinook')),
         DNACollected = revalue(DNACollected, c('No' = FALSE,
                                                'C' = TRUE,
                                                'B' = TRUE,
                                                'A' = TRUE,
                                                'Yes' = TRUE)),
         Sex = revalue(Sex, c('Unknown' = NA))) %>%
  dplyr::rename(SurveyYear = `SurveyYear*`) %>%
  filter(Species == 'SpSm Chinook',
         POP_NAME %in% c('Secesh River', 'East Fork South Fork Salmon River'))

#------------------------------------------------------------------------------
# Summarize Data
#------------------------------------------------------------------------------
age_dat <- sgs_dat %>%
  filter(VerifiedOrigin != 'Unknown',
         !is.na(TotalAge)) %>%
  group_by(SurveyYear, POP_NAME, VerifiedOrigin, TotalAge) %>%
  summarise(n = n()) %>%
  mutate(TotalAge = paste0('Age_', TotalAge)) %>%
  spread(key = TotalAge, value = n, fill = 0) %>%
  arrange(POP_NAME, SurveyYear, VerifiedOrigin) %>%
  mutate(n = Age_2 + Age_3 + Age_4 + Age_5 + Age_6)

origin_dat <- sgs_dat %>%
  filter(VerifiedOrigin != 'Unknown',
         !is.na(TotalAge)) %>%
  group_by(SurveyYear, POP_NAME, VerifiedOrigin) %>%
  summarise(n = n()) %>%
  spread(key = VerifiedOrigin, value = n, fill = 0) %>%
  arrange(POP_NAME, SurveyYear) %>%
  mutate(n = Natural + Hatchery)


#------------------------------------------------------------------------------
# Save as .csv
#------------------------------------------------------------------------------
write.csv(sgs_dat, file = './Data/sgs_data.csv')
write.csv(age_dat, file = './Data/age_sample_size.csv')
write.csv(origin_dat, file = './Data/origin_sample_size.csv')
