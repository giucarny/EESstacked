# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Poland sample)
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Poland codebook and EP results # =========================================================

EES2019_cdbk_pl <-
  EES2019_cdbk %>%
  filter(countryshort=='PL')

EP2019_pl <-
  EP2019 %>%
  filter(countryshort=='PL')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_pl %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_pl %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_pl %<>%
  filter(partyid!='PL90') %>% 
  mutate(Q7 = case_when(partyid=='PL01' ~ as.integer(2104), #PiS
                        partyid=='PL02' ~ as.integer(2101), #Konfederacja
                        partyid=='PL03' ~ as.integer(2106), #Kukiz '15
                        partyid=='PL04' ~ as.integer(2102), #Wiosna
                        partyid=='PL05' ~ as.integer(2103), #KE
                        partyid=='PL08' ~ as.integer(2107), #Polska Fair Play
                        partyid=='PL09' ~ as.integer(2105), #Lewica Razem
                        T~NA_integer_))

EES2019_pl_enhcdbk <- 
  left_join(EES2019_cdbk_pl,
            EP2019_pl %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 
# 
# EES2019_pl_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# only KE, not its constituent parties have votesh and seats

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pl$')) 
