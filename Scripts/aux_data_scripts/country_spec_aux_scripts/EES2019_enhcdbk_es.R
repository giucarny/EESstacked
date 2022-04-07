# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Spain sample)
# Author: W. Haeussling, M.Koernig
# last update: 2022-03-17
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

# EES2019_es_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   filter(Q7==2603)



# Check the new dataset 

# EES2019_es_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_es_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2601 ~ 33320, #in Euromanifesto, 2601 is in Coalition with the minor party "Party of the Socialists of Catalonia"
      Q7==2602 ~ 33610,
      Q7==2603 ~ 33210,
      Q7==2604 ~ 33410,
      Q7==2605 ~ 33701,
      Q7==2606 ~ 33901,
      Q7==2608 ~ 33904,
      Q7==2607 ~ 33901,
      Q7==2609 ~ 33120, #In Euromanifesto: 2609 is called "Coalicion Primavera Europea", for which 2609 (Compromis por Europa) is the successor from 2019 onwards
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_es_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_es$')) 