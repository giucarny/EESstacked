# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Spain Sample) 
# Author: W.Haeussling
# last update: 2021-08-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Spain sample # ===================================================================

EES2019_es <- 
  EES2019 %>% 
  filter(countrycode==1724)


# Filter the codebook and EP elections data # ==========================================================

EP2019_es <- 
  EP2019 %>% 
  filter(countryshort=='ES')


EES2019_cdbk_es <- 
  EES2019_cdbk %>% 
  filter(countryshort=='ES')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_es$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_es %>% 
  dplyr::select(partyname, Q10_PTV) 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_es %>% 
  filter(party_name!='Other parties') 

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_es %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Spanish EES 2019 SDM # ====================================================================

EES2019_es_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_es$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_es$|_crit$'))  
rm(list = c('respid', 'party'))