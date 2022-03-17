# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Croatian sample)
# Author: G.Carteny & J.Leiser
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Croatian codebook and EP results # =========================================================

EES2019_cdbk_hr <-
  EES2019_cdbk %>%
  filter(countryshort=='HR') 

EP2019_hr <-
  EP2019 %>%
  filter(countryshort=='HR') 


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_hr %>%
#   dplyr::select(partyname, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_hr %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.))

EP2019_hr %<>%
  filter(partyid!='HR90') %>% 
  mutate(Q7 = case_when(partyid=='HR01' ~ as.integer(404),
                        partyid=='HR02' ~ as.integer(412),
                        partyid=='HR03' ~ NA_integer_, 
                        partyid=='HR04' ~ as.integer(406),
                        partyid=='HR05' ~ as.integer(410),
                        partyid=='HR06' ~ as.integer(414),
                        partyid=='HR07' ~ as.integer(401),
                        partyid=='HR08' ~ as.integer(402),
                        partyid=='HR09' ~ as.integer(413),
                        partyid=='HR10' ~ as.integer(403),
                        partyid=='HR11' ~ as.integer(405),
                        partyid=='HR12' ~ as.integer(407),
                        partyid=='HR13' ~ as.integer(409),
                        partyid=='HR14' ~ as.integer(408),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_hr_enhcdbk <- 
  left_join(EES2019_cdbk_hr,
            EP2019_hr %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_hr_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>% 
#   print(., n=nrow(.))


# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_hr_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_hr_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==412 ~ 81301,  # Social Democratic Party of Croatia  
      Q7==404 ~ 81501,  # Croation Democratic Union
      Q7==414 ~ 81001,  # Human Shield/Zivi Zid
      Q7==405 ~ 81403,  # Amsterdamska koalicija
      Q7==406 ~ 81601,  # Bridge of Independent Lists 
      Q7==413 ~ 81402,  # START
      Q7==401 ~ NA_real_,  # Bandi? Milan 365
      Q7==402 ~ 81701,  # Coaltion of NHR (1191714) and HSP(1191713) 
      Q7==403 ~ NA_real_,  # Croatian Growth
      Q7==407 ~ NA_real_,  # MOZ?EMO
      Q7==408 ~ NA_real_,  # NEZAVISNA LISTA MARIJANA PETIR
      Q7==409 ~ 81002,  # NEZAVISNA LISTA MISLAV KOLAKUSI?
      Q7==410 ~ NA_real_,  # PAMETNO + UNIJA KVARNERA
      Q7==411 ~ NA_real_,  #  Independent Democratic Serbian Party  
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_hr_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs) %>% print(n = nrow(.))


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hr$')) 