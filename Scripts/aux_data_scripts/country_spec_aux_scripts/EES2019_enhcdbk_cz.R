# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Czech Rep. sample)
# Author: J.Leiser
# last update: 2021-08-25
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Czech Rep. codebook and EP results # =========================================================

EES2019_cdbk_cz <-
  EES2019_cdbk %>%
  filter(countryshort=='CZ')

EP2019_cz <-
  EP2019 %>%
  filter(countryshort=='CZ')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_cz %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_cz %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_cz %<>%
  filter(partyid!='CZ90') %>% 
  mutate(Q7 = case_when(partyid=='CZ01' ~ as.integer(604), #ODS
                        partyid=='CZ02' ~ as.integer(603), #CSSD
                        partyid=='CZ03' ~ as.integer(605), #KSCM
                        partyid=='CZ04' ~ as.integer(601), #KDU/CSL
                        partyid=='CZ05' ~ as.integer(602), #TOP 09
                        partyid=='CZ06' ~ as.integer(606), #ANO
                        partyid=='CZ07' ~ as.integer(607), #Pirati
                        partyid=='CZ08' ~ as.integer(608), #SPD
                        T~NA_integer_))


EES2019_cz_enhcdbk <- 
  left_join(EES2019_cdbk_cz,
            EP2019_cz %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_cz_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_cz$')) 
