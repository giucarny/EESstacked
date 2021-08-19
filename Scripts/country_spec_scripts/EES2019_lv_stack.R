# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Latvian Sample) 
# Author: G.Carteny, M.Koernig
# last update: 2021-08-12
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 latvian sample # ===================================================================

EES2019_lv <- 
  EES2019 %>% 
  filter(countrycode==1428)


# Filter the codebook and EP elections data # ==========================================================

EP2019_lv <- 
  EP2019 %>% 
  filter(countryshort=='LV')


EES2019_cdbk_lv <- 
  EES2019_cdbk %>% 
  filter(countryshort=='LV')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_lv$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_lv %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 7 parties and coalitions
#parties and coalitions: Coal. NA, JKP, Coal. AP!, KPV LV, Saskana SDP, Coal. ZZS, JV
#note: here not only the parties but also coalitions are listed

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_lv %>% 
  filter(partyname!='Other parties')

#votes_crit: 16 parties and coalitions
#all parties/coalitions with PTV variable obtained at least on eseat in the EP

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_lv %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Latvian EES 2019 SDM # ====================================================================

EES2019_lv_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_lv$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_lv_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lv$|_crit$'))  
rm(list = c('respid', 'party'))