# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Malta sample)
# Author: W. Haeussling
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Maltese codebook and EP results # =========================================================


EES2019_cdbk_mt <-
  EES2019_cdbk %>%
  filter(countryshort=='MT')

EP2019_mt <-
  EP2019 %>%
  filter(countryshort=='MT')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_mt %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_mt %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_mt %<>%
  filter(partyid!='MT90') %>% 
  mutate(Q7 = case_when(partyid=='MT01' ~ as.integer(1902),
                        partyid=='MT02' ~ as.integer(1901),
                        partyid=='MT03' ~ as.integer(1903), 
                        partyid=='MT04' ~ as.integer(1904),
                        T~NA_integer_))

EES2019_mt_enhcdbk <- 
  left_join(EES2019_cdbk_mt,
            EP2019_mt %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')




# Check the new dataset 

#EES2019_mt_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_mt$')) 