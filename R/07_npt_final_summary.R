#------------------------------------------------------------------------------
#         Join all summaries to create a Final Chinook Summary Table
# 
#                                                ><(((*>   ><>  ~
# Author: Tyler Stright
# Created: 10/22/18
#------------------------------------------------------------------------------
# Join Chinook Mark/Recap, Carcass, and Redd summary data
npt_final <- left_join(chinook_mcr, carcass_origin_sex) %>%
  left_join(npt_redds) %>%
  mutate(redds_upstream = ifelse(is.na(redds_upstream), 0, redds_upstream),
         redds_downstream = ifelse(is.na(redds_downstream), 0, redds_downstream),
         total_redds = redds_downstream + redds_upstream) %>%
# FishPerRedd
  mutate(fishperredd_da = (redds_downstream*(Nhat/redds_upstream)),
         fishperredd_var = (((redds_downstream/redds_upstream)^2)*Vhat)) %>%
# FemalePerRedd   
  mutate(f_downstream = ((redds_downstream* ((Nhat*us_percent_females)/redds_upstream))),
         f_downstream_var = (((redds_downstream/redds_upstream)^2)*Vhat),
         femaleperredd_da = (f_downstream/ds_percent_females)) %>% # total DS abundance
#         femaleperredd_var = (((1/ds_percent_females^2)*f_downstream_var) + ((f_downstream^2)/ds_percent_females^4)* #VAR %female DS) %>%
# mutate(adultperredd_da = (redds_downsteam(Nhat)))

  mutate(total_spawner_abundance = (1-prespawnmort_percent)*(Nhat + femaleperredd_da)) %>%
        # total_spawner_abundance_var = ***Need this value yet***
  mutate(NOSA = (total_spawner_abundance*(1-pHOS)))

#------------------------------------------------------------------------------
# Save final summary table!
save(npt_final, file = './data/npt_final.Rda')
