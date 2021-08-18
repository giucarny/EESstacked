# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Greek Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Greek sample # =====================================================================

EES2019_el <- 
  EES2019 %>% 
  filter(countrycode==1300)


# Filter the codebook and EP elections data # ==========================================================

EP2019_el <- 
  EP2019 %>% 
  filter(countryshort=='EL') %>% 
  dplyr::select(countryshort, party_name_eng, votesh) %>% 
  mutate(party_name_eng = party_name_eng %>% gsub('.*/','',.) %>% str_trim())  


EES2019_cdbk_el <- 
  EES2019_cdbk %>% 
  filter(countryshort=='GR')

EES2019_cdbk_el$countryshort <- 'EL'

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_el$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_el %>% 
  dplyr::select(partyname_eng, Q10_PTV) 

#ptv_crit: 7 parties

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_el %>% 
  filter(party_name_eng!='Other parties') 

#votes_crit: 14 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_el %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Greek EES 2019 SDM # ======================================================================

EES2019_el_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_el$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_el_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_el$|_crit$'))  
rm(list = c('respid', 'party'))