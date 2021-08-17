# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Lithuania Sample) 
# Author: J.Leiser
# last update: 2021-08-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1440
# countryshort = 'LT'

# Keep the EES 2019 Lithuania sample # ===================================================================
EES2019_lt <- 
  EES2019 %>% 
  filter(countrycode==1440)

# Filter the codebook and EP elections data # ==========================================================

EP2019_lt <- 
  EP2019 %>% 
  filter(countryshort=='LT')


EES2019_cdbk_lt <- 
  EES2019_cdbk %>% 
  filter(countryshort=='LT')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_lt$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_lt %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit
# 7 parties
# parties = TS-LKD, LSDP, LS, DP, TT, LLRA, LVZS


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_lt %>% 
  filter(party_name!='Other parties') 

# votes_crit
# 14 parties
# parties = DP, LLRA-KSS, LRLS ( = LS from above), 
# LSDP, LVZS, TT, TS-LKD, LCP, LSDDP, LZP, LLS, VKM-AMT, VKM-PRPJ, VKM-VRSV
 

# from the 7 parties with a PTV variable, all 7 obtained a seat in EP election
# only keep those 7

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_lt %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Lithuania EES 2019 SDM # ====================================================================

EES2019_lt_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_lt$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_lt_stack$countrycode)
# correct country code

# unique(EES2019_lt$respid) %>% length()
# unique(EES2019_lt_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_lt_stack)
# 7000 by 4 data frame as expected (7 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$|_crit$'))  
rm(list = c('respid', 'party'))