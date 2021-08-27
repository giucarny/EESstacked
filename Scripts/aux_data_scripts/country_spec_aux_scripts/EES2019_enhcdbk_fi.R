# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Finland sample)
# Author: J.Leiser
# last update: 2021-08-25
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Finland codebook and EP results # =========================================================
EES2019_cdbk_fi <-
  EES2019_cdbk %>%
  filter(countryshort=='FI')

EP2019_fi <-
  EP2019 %>%
  filter(countryshort=='FI')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_fi %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_fi %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_fi %<>%
  filter(partyid!='FI90') %>% 
  mutate(Q7 = case_when(partyid=='FI01' ~ as.integer(1004), #KESK
                        partyid=='FI02' ~ as.integer(1003), #KOK
                        partyid=='FI03' ~ as.integer(1001), #SDP
                        partyid=='FI04' ~ as.integer(1006), #VAS
                        partyid=='FI05' ~ as.integer(1005), #VIHR
                        partyid=='FI06' ~ as.integer(1007), #SFP (RKP)
                        partyid=='FI07' ~ as.integer(1002), #PS
                        partyid=='FI08' ~ as.integer(1008), #KD
                        partyid=='FI09' ~ as.integer(1009), #SIN/ Sininen tulevaisuus 
                        partyid=='FI10' ~ as.integer(1010), #PP / PIR
                        T~NA_integer_))


EES2019_fi_enhcdbk <- 
  left_join(EES2019_cdbk_fi,
            EP2019_fi %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_fi_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_fi$')) 

