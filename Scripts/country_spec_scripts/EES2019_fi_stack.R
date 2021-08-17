# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Finland Sample) 
# Author: J.Leiser
# last update: 2021-08-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1246
# countryshort = 'FI'

# Keep the EES 2019 Finland sample # ===================================================================
EES2019_fi <- 
  EES2019 %>% 
  filter(countrycode==1246)


# Filter the codebook and EP elections data # ==========================================================

EP2019_fi <- 
  EP2019 %>% 
  filter(countryshort=='FI')


EES2019_cdbk_fi <- 
  EES2019_cdbk %>% 
  filter(countryshort=='FI')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_fi$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_fi %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit #7 Parties with PTV score
# Parties = SDP, Perus/PS, KOK, KESK, VIHR, VAS, RKP

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_fi %>% 
  filter(party_name!='Other parties') 

# votes_crit 
# 10 Parties
# parties = KESK, KOK, sDP, VAS, VIHR, SFP (RKP), PS, KS, SIN, PP

# from the 7 parties with a PTV variable, all 7 obtained a seat in EP election
# only keep those 7

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_fi %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Finland EES 2019 SDM # ====================================================================

EES2019_fi_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_fi$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_fi_stack$countrycode)
# correct country code

# unique(EES2019_fi$respid) %>% length()
# unique(EES2019_fi_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_fi_stack)
# 7000 by 4 data frame as expected (7 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_fi$|_crit$'))  
rm(list = c('respid', 'party'))