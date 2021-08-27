# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Portuguese sample)
# Author: M.Koernig
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Portuguese codebook and EP results # =========================================================


EES2019_cdbk_pt <-
  EES2019_cdbk %>%
  filter(countryshort=='PT')

EP2019_pt <-
  EP2019 %>%
  filter(countryshort=='PT')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_pt %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_pt %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_pt %<>%
  filter(partyid!='PT90') %>% 
  mutate(Q7 = case_when(partyid=='PT01' ~ as.integer(2206),
                        partyid=='PT02' ~ as.integer(2203),
                        partyid=='PT03' ~ as.integer(2201), 
                        partyid=='PT04' ~ as.integer(2202),
                        partyid=='PT05' ~ as.integer(2204),
                        partyid=='PT06' ~ as.integer(2208),
                        partyid=='PT07' ~ as.integer(2209),
                        T~NA_integer_))


EES2019_pt_enhcdbk <- 
  left_join(EES2019_cdbk_pt,
            EP2019_pt %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_pt_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pt$')) 