# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Spain sample)
# Author: W. Haeussling
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Spanish codebook and EP results # =========================================================


EES2019_cdbk_es <-
  EES2019_cdbk %>%
  filter(countryshort=='ES')

EP2019_es <-
  EP2019 %>%
  filter(countryshort=='ES')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_es %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_es %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_es %<>%
  filter(partyid!='ES90') %>% 
  mutate(Q7 = case_when(partyid=='ES01' ~ as.integer(2601),
                        partyid=='ES02' ~ as.integer(2602),
                        partyid=='ES03' ~ as.integer(2603), 
                        partyid=='ES04' ~ as.integer(2607),
                        partyid=='ES05' ~ as.integer(2604),
                        partyid=='ES06' ~ as.integer(2608),
                        partyid=='ES07' ~ as.integer(2605),
                        partyid=='ES08' ~ as.integer(2606),
                        partyid=='ES09' ~ as.integer(2609),
                        partyid=='ES10' ~ as.integer(2610),
                        T~NA_integer_))

EES2019_es_enhcdbk <- 
  left_join(EES2019_cdbk_es,
            EP2019_es %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

#Party 'Coalition Unidas Podemos Cambiar Europa (Unidas Podemos + Izquierda Unida + Catalunya en Comú + Barcelona en Comú)'
#with the countrycode 2603 appears twice in EES2019_es_enhcdbk due to two 
#different seat quantities.

#EES2019_es_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   filter(Q7==2603)



# Check the new dataset 

#EES2019_es_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_es$')) 