# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Poland Sample) 
# Author: J.Leiser
# last update: 2021-08-12
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1616
# countryshort = 'PL'

# Keep the EES 2019 Poland sample # ===================================================================
EES2019_pl <- 
  EES2019 %>% 
  filter(countrycode==1616)

# Filter the codebook and EP elections data # ==========================================================

EP2019_pl <- 
  EP2019 %>% 
  filter(countryshort=='PL')


EES2019_cdbk_pl <- 
  EES2019_cdbk %>% 
  filter(countryshort=='PL')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_pl$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_pl %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit
# 8 parties
# parties = PO, PSL, SLD, PIS, Kukiz '15, Wiosna Roberta Biedronia, Razem, KE


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_pl %>% 
  filter(partyname!='Other parties') 

# votes_crit
# 7 parties
# parties = PiS, Konfederacja, Kukiz '15, Wiosna, KE ( = PO + PSL + SLD + others) 
# Polska Fair Play, Coal Lewica Razem

# All parties with PTV variable are in vote_crit either independently or as part of a 
# coalition

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_pl %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# We keep KE which contains PO, PSL and SLD parties. These 3 parties also lack values in Q7.

# party


# Create the Poland EES 2019 SDM # ====================================================================

EES2019_pl_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_pl$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_pl_stack$countrycode)
# correct country code

# unique(EES2019_pl$respid) %>% length()
# unique(EES2019_pl_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_pl_stack)
# 5000 by 4 data frame as expected (5 parties (PIS, Kukiz,
# Wiosna, Razem and KE) x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pl$|_crit$'))  
rm(list = c('respid', 'party'))
