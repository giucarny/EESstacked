# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Croatian sample)
# Author: G.Carteny
# last update: 2021-08-30
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

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hr$')) 