# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (SLovenian sample)
# Author: M.Koernig & J.Leiser
# last update: 2022-03-16
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


# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_si_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_si_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2401 ~ 97320,  # Slovenska demokratska stranka in Slovenska ljudska stranka (SDS in SLS)
      Q7==2402 ~ 97401,  # Lista Marjana Sarca (LMS) 
      Q7==2403 ~ 97321,  # Socialni demokrati (SD) 
      Q7==2404 ~ 97510,  # Nova Slovenija - krs?anski demokrati  (NSi) 
      Q7==2405 ~ NA_real_,  # Levica
      Q7==2406 ~ NA_real_,  # Slovenska nacionalna stranka (SNS)
      Q7==2407 ~ NA_real_,  # Stranka modernega centra (SMC)
      Q7==2408 ~ NA_real_,  # Stranka Alenke Bratusek (SAB)
      Q7==2409 ~ 97951,  # Demokrati?na stranka upokojencev Slovenije (DESUS)
      Q7==2410 ~ NA_real_,  # Dom - domovinska liga (DOM)
      Q7==2411 ~ NA_real_,  # Zeleni Slovenije  
      Q7==2412 ~ NA_real_,  # Povezimo se
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_si_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_si$')) 