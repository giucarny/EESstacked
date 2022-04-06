# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Latvian sample)
# Author: M.Koernig & J.Leiser
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Latvian codebook and EP results # =========================================================


EES2019_cdbk_lv <-
  EES2019_cdbk %>%
  filter(countryshort=='LV')

EP2019_lv <-
  EP2019 %>%
  filter(countryshort=='LV')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_lv %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_lv %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_lv %<>%
  filter(partyid!='LV90') %>% 
  mutate(Q7 = case_when(partyid=='LV01' ~ as.integer(1616),
                        partyid=='LV02' ~ as.integer(1611),
                        partyid=='LV03' ~ as.integer(1610), 
                        partyid=='LV04' ~ as.integer(1604),
                        partyid=='LV05' ~ as.integer(1603),
                        partyid=='LV06' ~ as.integer(1605),
                        partyid=='LV07' ~ as.integer(1608),
                        partyid=='LV08' ~ as.integer(1609),
                        partyid=='LV09' ~ as.integer(1601),
                        partyid=='LV10' ~ as.integer(1612),
                        partyid=='LV11' ~ as.integer(1606),
                        partyid=='LV12' ~ as.integer(1615),
                        partyid=='LV13' ~ as.integer(1602),
                        partyid=='LV14' ~ as.integer(1607),
                        partyid=='LV15' ~ as.integer(1614),
                        partyid=='LV16' ~ as.integer(1613),
                        T~NA_integer_))


EES2019_lv_enhcdbk <- 
  left_join(EES2019_cdbk_lv,
            EP2019_lv %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_lv_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)


# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_lv_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_lv_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1611 ~ 87710,  # For Fatherland and Freedom - National Independence Movement of Latvia 
      Q7==1608 ~ NA_real_,  # New Conservative Party
      Q7==1609 ~ 87401,  # Development/For!
      Q7==1605 ~ NA_real_,  # Who owns the state?
      Q7==1610 ~ 87221,  # Social Democratic Party \"\"Harmony\"\
      Q7==1604 ~ 87110,  # Green and Farmers' Union
      Q7==1616 ~ 87402,  # Unity
      Q7==1601 ~ 87951,  # Latvian Russian Union 
      Q7==1602 ~ NA_real_,  # Latvian Nationalists
      Q7==1603 ~ NA_real_,  # Latvian Association of Regions 
      Q7==1606 ~ NA_real_,  # Progressives
      Q7==1607 ~ NA_real_,  # New Harmony
      Q7==1612 ~ NA_real_,  # Action Party
      Q7==1613 ~ NA_real_,  #  Awakening
      Q7==1614 ~ NA_real_,  # Center Party
      Q7==1615 ~ NA_real_,  # Latvian Social Democratic Workers' Party
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_lv_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lv$')) 