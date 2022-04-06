# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Irish sample)
# Author: M.Koernig & J.Leiser
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Irish codebook and EP results # =========================================================


EES2019_cdbk_ie <-
  EES2019_cdbk %>%
  filter(countryshort=='IE')

EP2019_ie <-
  EP2019 %>%
  filter(countryshort=='IE')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_ie %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_ie %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_ie %<>%
  filter(partyid!='IE90') %>% 
  mutate(Q7 = case_when(partyid=='IE01' ~ as.integer(1401),
                        partyid=='IE02' ~ as.integer(1402),
                        partyid=='IE03' ~ as.integer(1403), 
                        partyid=='IE04' ~ as.integer(1404),
                        partyid=='IE05' ~ as.integer(1405),
                        partyid=='IE06' ~ as.integer(1409),
                        partyid=='IE07' ~ as.integer(1407),
                        partyid=='IE08' ~ as.integer(1408),
                        T~NA_integer_))


EES2019_ie_enhcdbk <- 
  left_join(EES2019_cdbk_ie,
            EP2019_ie %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_ie_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_ie_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_ie_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1402 ~ 53520,  # Fine Gael 
      Q7==1403 ~ 53320,  # Labour Party 
      Q7==1401 ~ 53620,  # Fianna Fail 
      Q7==1404 ~ 53110,  # Green Party 
      Q7==1405 ~ 53951,  # Sinn Fein 
      Q7==1406 ~ NA_real_,  # Solidarity - People Before Profit
      Q7==1407 ~ 53301,  # Social Democrats 
      Q7==1408 ~ 53302,  # Independents 4 Change
      Q7==1409 ~ NA_real_,  # Independent
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_ie_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ie$')) 