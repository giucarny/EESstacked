# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, French Sample) 
# Author: G.Carteny, Matthias Körnig(Ger529)
# last update: 2021-08-12
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 french sample # ===================================================================

EES2019_fr <- 
  EES2019 %>% 
  filter(countrycode==1250)


# Filter the codebook and EP elections data # ==========================================================

EP2019_fr <- 
  EP2019 %>% 
  filter(countryshort=='FR')


EES2019_cdbk_fr <- 
  EES2019_cdbk %>% 
  filter(countryshort=='FR')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_fr$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_fr %>% 
  dplyr::select(partyname, Q10_PTV) 

  #ptv_crit: 7 parties and coalitions
  #parties: LR, PS, RN, EELV, LFI, Generations.s, LRM

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_fr %>% 
  filter(party_name!='Other parties') 

  #votes_crit: 10 parties and coalitions
  #parties: FI, Coal, (PS+RDG+PP+N), EELV, LR, RN, DLF+CNIP, Coal, (LREM+Modem+MRSL), PCF, UDI, Generation.s
  #note: shortname differnces between ptv_crit and votes_crit: LFI is FI, LRM is LREM
  #note: PS and LRM only received a seat within a coalition with other parties
  #all parties with PTV variable obtained a seat in the EP. Thus, all 7 parties going into the selection

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_fr %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the French EES 2019 SDM # ====================================================================

EES2019_fr_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_fr$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_fr_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_fr$|_crit$'))  
rm(list = c('respid', 'party'))