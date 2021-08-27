# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (SLovenian sample)
# Author: M.Koernig
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Slovenian codebook and EP results # =========================================================


EES2019_cdbk_si <-
  EES2019_cdbk %>%
  filter(countryshort=='SI')

EP2019_si <-
  EP2019 %>%
  filter(countryshort=='SI')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_si %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_si %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_si %<>%
  filter(partyid!='SI90') %>% 
  mutate(Q7 = case_when(partyid=='SI01' ~ as.integer(2401),
                        partyid=='SI02' ~ as.integer(2403),
                        partyid=='SI03' ~ as.integer(2409), 
                        partyid=='SI04' ~ as.integer(2404),
                        partyid=='SI05' ~ as.integer(2406),
                        partyid=='SI06' ~ as.integer(2407),
                        partyid=='SI07' ~ as.integer(2408),
                        partyid=='SI08' ~ as.integer(2405),
                        partyid=='SI09' ~ as.integer(2402),
                        partyid=='SI10' ~ as.integer(2410),
                        partyid=='SI11' ~ as.integer(2412),
                        partyid=='SI14' ~ as.integer(2411),
                        T~NA_integer_))


EES2019_si_enhcdbk <- 
  left_join(EES2019_cdbk_si,
            EP2019_si %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_si_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_si$')) 