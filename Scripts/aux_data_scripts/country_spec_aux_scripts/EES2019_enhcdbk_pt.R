# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Portuguese sample)
# Author: M.Koernig & J.Leiser
# last update: 2022-03-16
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

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_pt_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_pt_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2202 ~ 35319,  # Partido Social Democrata  (PSD) 
      Q7==2204 ~ 35314,  # Centro Democratico e Social - Partido Popular (CDS-PP)
      Q7==2201 ~ 35311,  # Partido Socialista (PS)
      Q7==2203 ~ 35225,  # Coligacao Democratica Unitaria (CDU)  
      Q7==2206 ~ 35223,  # Bloco de Esquerda (BE)  
      Q7==2208 ~ 35101,  # Pessoas, Animais, Natureza (PAN) 
      Q7==2205 ~ NA_real_,  # Partido da Terra (MPT) 
      Q7==2207 ~ NA_real_,  # Partido Democratico Republicano (PDR) 
      Q7==2209 ~ 35601,  # Alianca  
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_pt_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pt$')) 