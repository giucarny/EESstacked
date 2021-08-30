# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Belgian sample)
# Author: G.Carteny
# last update: 2021-08-30
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Belgian codebook and EP results # ===========================================================

EES2019_cdbk_be <-
  EES2019_cdbk %>%
  filter(countryshort=='BE') 

EP2019_be <-
  EP2019 %>%
  filter(countryshort=='BE') 


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_be %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_be %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.))

EP2019_be %<>%
  filter(partyid!='BE90') %>% 
  mutate(Q7 = case_when(partyid=='BE01' ~ as.integer(201),
                        partyid=='BE02' ~ as.integer(202),
                        partyid=='BE03' ~ as.integer(203), 
                        partyid=='BE04' ~ as.integer(204),
                        partyid=='BE05' ~ as.integer(205),
                        partyid=='BE06' ~ as.integer(206),
                        partyid=='BE07' ~ as.integer(207),
                        partyid=='BE08' ~ as.integer(208),
                        partyid=='BE09' ~ as.integer(209),
                        partyid=='BE10' ~ as.integer(210),
                        partyid=='BE11' ~ as.integer(211),
                        partyid=='BE12' ~ as.integer(213),
                        partyid=='BE13' ~ as.integer(214),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_be_enhcdbk <- 
  left_join(EES2019_cdbk_be,
            EP2019_be %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_be_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   print(., n=nrow(.))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$')) 