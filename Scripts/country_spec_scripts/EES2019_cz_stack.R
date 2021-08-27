# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Czech Republic Sample) 
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1203
# countryshort = 'CZ'

# Keep the EES 2019 Czech Rep. sample # ===================================================================
EES2019_cz <- 
  EES2019 %>% 
  filter(countrycode==1203)

# Filter the codebook data # ==========================================================

# EP2019_cz <- 
#   EP2019 %>% 
#   filter(countryshort=='CZ')
# 
# 
# EES2019_cdbk_cz <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='CZ')

EES2019_cdbk_cz <-
  EES2019_cdbk %>%
  filter(countryshort=='CZ')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_cz$respid %>% 
  as.numeric()

#class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_cz %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit 



# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_cz %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# votes_crit 



# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_cz %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Czech Rep. EES 2019 SDM # ====================================================================

EES2019_cz_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_cz$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_cz$|_crit$'))  
rm(list = c('respid', 'party'))
