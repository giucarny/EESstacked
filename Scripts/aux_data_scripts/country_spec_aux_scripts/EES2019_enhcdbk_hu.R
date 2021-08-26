# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Hungary sample)
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Hungary codebook and EP results # =========================================================

EES2019_cdbk_hu <-
  EES2019_cdbk %>%
  filter(countryshort=='HU')

EP2019_hu <-
  EP2019 %>%
  filter(countryshort=='HU')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

EES2019_cdbk_hu %>%
  dplyr::select(partyname, partyname_eng, Q7)

EP2019_hu %>%
  dplyr::select(partyname, partyname_eng, partyid)

EP2019_hu %<>%
  filter(partyid!='HU90') %>% 
  mutate(Q7 = case_when(partyid=='HU01' ~ as.integer(1301), #DK
                        partyid=='HU02' ~ as.integer(1302), #FIDESZ + KDNP
                        partyid=='HU03' ~ as.integer(1303), #JOBBIK
                        partyid=='HU04' ~ as.integer(1304), #LMP
                        partyid=='HU05' ~ as.integer(1305), #MKKP
                        partyid=='HU06' ~ as.integer(1308), #Momentum
                        partyid=='HU07' ~ as.integer(1306), #MSZP
                        partyid=='HU08' ~ as.integer(1307), #Mi Hazank
                        T~NA_integer_))

EES2019_hu_enhcdbk <- 
  left_join(EES2019_cdbk_hu,
            EP2019_hu %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_hu_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hu$')) 
