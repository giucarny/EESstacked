# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (French sample)
# Author: M.Koernig
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the French codebook and EP results # =========================================================


EES2019_cdbk_fr <-
  EES2019_cdbk %>%
  filter(countryshort=='FR')

EP2019_fr <-
  EP2019 %>%
  filter(countryshort=='FR')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_fr %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_fr %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_fr %<>%
  filter(partyid!='FR90') %>% 
  mutate(Q7 = case_when(partyid=='FR01' ~ as.integer(1101),
                        partyid=='FR02' ~ as.integer(1105),
                        partyid=='FR03' ~ as.integer(1114), 
                        partyid=='FR04' ~ as.integer(1113),
                        partyid=='FR05' ~ as.integer(1111),
                        partyid=='FR07' ~ as.integer(1102),
                        partyid=='FR10' ~ as.integer(1110),
                        T~NA_integer_))


EES2019_fr_enhcdbk <- 
  left_join(EES2019_cdbk_fr,
            EP2019_fr %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_fr_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_fr_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1113 ~ 31601,
      Q7==1111 ~ 31701,
      Q7==1114 ~ 31115,
      Q7==1101 ~ 31201,
      Q7==1105 ~ 31301, #in Euromanifesto it´s a coalition of partys with 1105 as the only Q7 party
      Q7==1102 ~ 31401, #in Euromanifesto it´s a coalition of partys with 1102 as the only Q7 party
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_fr_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_fr$')) 