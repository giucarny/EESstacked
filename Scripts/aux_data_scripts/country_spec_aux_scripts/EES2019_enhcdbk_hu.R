# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Hungary sample)
# Author: J.Leiser
# last update: 2022-03-15
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

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_hu_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_hu_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1301 ~ 86310,  # DK
      Q7==1302 ~ 86524,  # FIDESZ-KDNP
      Q7==1303 ~ 86701,  # JOBBIK
      Q7==1304 ~ 86120,  # LMP
      Q7==1306 ~ 86220,  # MSZP  
      Q7==1307 ~ NA_real_,  # MH
      Q7==1308 ~ 86951,  # Momentum Mozgalom  
      Q7==1305 ~ NA_real_,  # MKKP
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_hu_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hu$')) 
