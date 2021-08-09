# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Slovakia Sample) 
# Author: J.Leiser
# last update: 2021-08-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1703
# countryshort = 'SK'

# Keep the EES 2019 Slovakia sample # ===================================================================
EES2019_sk <- 
  EES2019 %>% 
  filter(countrycode==1703)


# Filter the codebook and EP elections data # ==========================================================

EP2019_sk <- 
  EP2019 %>% 
  filter(countryshort=='SK')


EES2019_cdbk_sk <- 
  EES2019_cdbk %>% 
  filter(countryshort=='SK')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_sk$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_sk %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit
# 9 parties
# parties = KDH, LSNS, Sme rodina, Smer-SD, SaS, OL'aNo, PS + SPOLU, SNS,
# Most-Hid


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_sk %>% 
  filter(party_name!='Other parties') 

# votes_crit
# 12 parties
# parties = KDH, SaS, SNS, SMER-SD, SMK-MPK, OL'aNO + NOVA, MOST-HID
# LSNS, Sme Rodina, PS + SPOLU, KU, KDZP

# from the 9 parties with a PTV variable, all 9 obtained a seat in EP election
# only keep those 9

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_sk %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Slovakia EES 2019 SDM # ====================================================================

EES2019_sk_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_sk$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_sk_stack$countrycode)
# correct country code

# unique(EES2019_sk$respid) %>% length()
# unique(EES2019_sk_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_sk_stack)
# 9000 by 4 data frame as expected (9 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_sk$|_crit$'))  
rm(list = c('respid', 'party'))
