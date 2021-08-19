# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Czech Republic Sample) 
# Author: J.Leiser
# last update: 2021-08-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1203
# countryshort = 'CZ'

# Keep the EES 2019 Czech Rep. sample # ===================================================================
EES2019_cz <- 
  EES2019 %>% 
  filter(countrycode==1203)

# Filter the codebook and EP elections data # ==========================================================

EP2019_cz <- 
  EP2019 %>% 
  filter(countryshort=='CZ')


EES2019_cdbk_cz <- 
  EES2019_cdbk %>% 
  filter(countryshort=='CZ')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_cz$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_cz %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit #8 Parties
# Parties = KDU/CSL, CSSD, ODS, KSCM, ANO, Piráti, SPD, TOP 09


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_cz %>% 
  filter(partyname!='Other parties') 

# votes_crit 
# 12 Parties
# parties = ODS, CSSD, KSCM, KDU-CSL, TOP 09 + STAN, ANO 2011, Piráti, SPD, HLAS,
# EU TROLL, Vedci pro CR, Coal ROZUMNÍ ND	

# from the 8 parties with a PTV variable, all 8 obtained a seat in EP election
# only keep those 8

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_cz %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Czech Rep. EES 2019 SDM # ====================================================================

EES2019_cz_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_cz$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_cz_stack$countrycode)
# correct country code

# unique(EES2019_cz$respid) %>% length()
# unique(EES2019_cz_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_cz_stack)
# 8000 by 4 data frame as expected (8 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_cz$|_crit$'))  
rm(list = c('respid', 'party'))
