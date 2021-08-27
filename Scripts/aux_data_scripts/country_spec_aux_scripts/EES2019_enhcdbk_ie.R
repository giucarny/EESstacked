# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Irish sample)
# Author: M.Koernig
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Irish codebook and EP results # =========================================================


EES2019_cdbk_ie <-
  EES2019_cdbk %>%
  filter(countryshort=='IE')

EP2019_ie <-
  EP2019 %>%
  filter(countryshort=='IE')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_ie %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_ie %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_ie %<>%
  filter(partyid!='IE90') %>% 
  mutate(Q7 = case_when(partyid=='IE01' ~ as.integer(1401),
                        partyid=='IE02' ~ as.integer(1402),
                        partyid=='IE03' ~ as.integer(1403), 
                        partyid=='IE04' ~ as.integer(1404),
                        partyid=='IE05' ~ as.integer(1405),
                        partyid=='IE06' ~ as.integer(1409),
                        partyid=='IE07' ~ as.integer(1407),
                        partyid=='IE08' ~ as.integer(1408),
                        T~NA_integer_))


EES2019_ie_enhcdbk <- 
  left_join(EES2019_cdbk_ie,
            EP2019_ie %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_ie_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ie$')) 