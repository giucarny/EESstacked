# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (United Kingdom sample)
# Author: W.Haeussling
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the United Kingdom codebook and EP results # =========================================================


EES2019_cdbk_uk <-
  EES2019_cdbk %>%
  filter(countryshort=='UK')

EP2019_uk <-
  EP2019 %>%
  filter(countryshort=='UK')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_uk %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_uk %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_uk %<>%
  filter(partyid!='UK90', partyid!='UK91') %>% 
  mutate(Q7 = case_when(partyid=='UK01' ~ as.integer(2810),
                        partyid=='UK02' ~ as.integer(2811),
                        partyid=='UK03' ~ as.integer(2812), 
                        partyid=='UK04' ~ as.integer(2813),
                        partyid=='UK07' ~ as.integer(2801),
                        partyid=='UK08' ~ as.integer(2802),
                        partyid=='UK09' ~ as.integer(2806),
                        partyid=='UK10' ~ as.integer(2805),
                        partyid=='UK11' ~ as.integer(2809),
                        partyid=='UK12' ~ as.integer(2803),
                        partyid=='UK13' ~ as.integer(2804),
                        partyid=='UK14' ~ as.integer(2808),
                        partyid=='UK15' ~ as.integer(2807),
                        T~NA_integer_))


EES2019_uk_enhcdbk <- 
  left_join(EES2019_cdbk_uk,
            EP2019_uk %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

#From those parties dropped by the left_join function one party, namely the 
#'Alliance Party', received one seat, the other none.

# Check the new dataset 

# EES2019_uk_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_uk$')) 