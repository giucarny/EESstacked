# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Latvian sample)
# Author: M.Koernig
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Latvian codebook and EP results # =========================================================


EES2019_cdbk_lv <-
  EES2019_cdbk %>%
  filter(countryshort=='LV')

EP2019_lv <-
  EP2019 %>%
  filter(countryshort=='LV')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_lv %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_lv %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_lv %<>%
  filter(partyid!='LV90') %>% 
  mutate(Q7 = case_when(partyid=='LV01' ~ as.integer(1616),
                        partyid=='LV02' ~ as.integer(1611),
                        partyid=='LV03' ~ as.integer(1610), 
                        partyid=='LV04' ~ as.integer(1604),
                        partyid=='LV05' ~ as.integer(1603),
                        partyid=='LV06' ~ as.integer(1605),
                        partyid=='LV07' ~ as.integer(1608),
                        partyid=='LV08' ~ as.integer(1609),
                        partyid=='LV09' ~ as.integer(1601),
                        partyid=='LV10' ~ as.integer(1612),
                        partyid=='LV11' ~ as.integer(1606),
                        partyid=='LV12' ~ as.integer(1615),
                        partyid=='LV13' ~ as.integer(1602),
                        partyid=='LV14' ~ as.integer(1607),
                        partyid=='LV15' ~ as.integer(1614),
                        partyid=='LV16' ~ as.integer(1613),
                        T~NA_integer_))


EES2019_lv_enhcdbk <- 
  left_join(EES2019_cdbk_lv,
            EP2019_lv %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_lv_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lv$')) 