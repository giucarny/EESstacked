# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Sweden Sample) 
# Author: J.Leiser
# last update: 2021-08-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1752
# countryshort = 'SE'

# Keep the EES 2019 Sweden sample # ===================================================================
EES2019_se <- 
  EES2019 %>% 
  filter(countrycode==1752)

# Filter the codebook and EP elections data # ==========================================================

EP2019_se <- 
  EP2019 %>% 
  filter(countryshort=='SE')


EES2019_cdbk_se <- 
  EES2019_cdbk %>% 
  filter(countryshort=='SE')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_se$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_se %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit
# 8 parties
# parties = S, M, MP, L, C, SD, KD, VP


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_se %>% 
  filter(party_name!='Other parties') 

# votes_crit
# 9 parties
# parties = V (= VP from above), S, C, L, M, KD, MP, SD, FI

# from the 8 parties with a PTV variable, all 8 obtained a seat in EP election
# only keep those 8

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_se %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Sweden EES 2019 SDM # ====================================================================

EES2019_se_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_se$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_se_stack$countrycode)
# correct country code

# unique(EES2019_se$respid) %>% length()
# unique(EES2019_se_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_se_stack)
# 8000 by 4 data frame as expected (8 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_se$|_crit$'))  
rm(list = c('respid', 'party'))
