# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Hungary Sample) 
# Author: J.Leiser
# last update: 2021-08-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1348
# countryshort = 'HU'

# Keep the EES 2019 Hungary sample # ===================================================================
EES2019_hu <- 
  EES2019 %>% 
  filter(countrycode==1348)

# Filter the codebook and EP elections data # ==========================================================

EP2019_hu <- 
  EP2019 %>% 
  filter(countryshort=='HU')


EES2019_cdbk_hu <- 
  EES2019_cdbk %>% 
  filter(countryshort=='HU')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_hu$respid %>% 
  as.numeric()

class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_hu %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit
# 7 parties
# parties = DK, FIDESZ - KDNP, JOBBIK, LMP, MSZP, MH, Momentum Mozgalum


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_hu %>% 
  filter(partyname!='Other parties') 

# votes_crit
# 8 parties
# parties = DK, FIDESZ + KDNP, JOBBIK, LMP, MKKP, Momentum Mozgalom, MH,
# MSZP + Párbeszéd

# from the 7 parties with a PTV variable, all 7 obtained a seat in EP election
# only keep those 7

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_hu %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# party

# Create the Hungary EES 2019 SDM # ====================================================================

EES2019_hu_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_hu$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Final plausibility check # ====================================================================

# unique(EES2019_hu_stack$countrycode)
# correct country code

# unique(EES2019_hu$respid) %>% length()
# unique(EES2019_hu_stack$respid) %>% length()
# number of unique respondents IDs in both original data and SDM match

# dim(EES2019_hu_stack)
# 7000 by 4 data frame as expected (7 parties x 1000 respondents)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hu$|_crit$'))  
rm(list = c('respid', 'party'))
