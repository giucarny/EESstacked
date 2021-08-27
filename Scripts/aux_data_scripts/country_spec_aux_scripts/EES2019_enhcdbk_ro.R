# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Romanian sample)
# Author: M.Koernig
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Romanian codebook and EP results # =========================================================


EES2019_cdbk_ro <-
  EES2019_cdbk %>%
  filter(countryshort=='RO')

EP2019_ro <-
  EP2019 %>%
  filter(countryshort=='RO')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_ro %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_ro %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_ro %<>%
  filter(partyid!='RO90') %>% 
  mutate(Q7 = case_when(partyid=='RO01' ~ as.integer(2301),
                        partyid=='RO02' ~ as.integer(2303),
                        partyid=='RO03' ~ as.integer(2306), 
                        partyid=='RO04' ~ as.integer(2307),
                        partyid=='RO05' ~ as.integer(2308),
                        partyid=='RO06' ~ as.integer(2305),
                        partyid=='RO07' ~ as.integer(2302),
                        T~NA_integer_))


EES2019_ro_enhcdbk <- 
  left_join(EES2019_cdbk_ro,
            EP2019_ro %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_ro_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ro$')) 