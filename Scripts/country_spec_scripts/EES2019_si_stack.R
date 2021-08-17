# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Slovenian Sample) 
# Author: G.Carteny, M.Koernig
# last update: 2021-08-12
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 slovenian sample # ===================================================================

EES2019_si <- 
  EES2019 %>% 
  filter(countrycode==1705)


# Filter the codebook and EP elections data # ==========================================================

EP2019_si <- 
  EP2019 %>% 
  filter(countryshort=='SI')


EES2019_cdbk_si <- 
  EES2019_cdbk %>% 
  filter(countryshort=='SI')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_si$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_si %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 9 parties and coalitions
#parties/coalitions: SDS in SLS, LMŠ, SD, NSi, Levica, SNS, SMC, SAB, DESUS 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_si %>% 
  filter(party_name!='Other parties') 

#votes_crit: 14 parties and coalitions
#all parties/coalitions with PTV variable obtained at least one seat in the EP

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_si %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

#party: 9 parties and coalitions

# Create the Slovenian EES 2019 SDM # ====================================================================

EES2019_si_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_si$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_si_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_si$|_crit$'))  
rm(list = c('respid', 'party'))