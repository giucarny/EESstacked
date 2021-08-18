# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Belgian Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Belgian sample # ===================================================================

EES2019_be <- 
  EES2019 %>% 
  filter(countrycode==1056)


# Filter the codebook and EP elections data # ==========================================================

EP2019_be <- 
  EP2019 %>% 
  filter(countryshort=='BE')


EES2019_cdbk_be <- 
  EES2019_cdbk %>% 
  filter(countryshort=='BE')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_be$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_be %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 14 parties (7 Flanders, 7 Wallonia)

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_be %>% 
  filter(party_name!='Other parties') 

#votes_crit: 19 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_be %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Belgian EES 2019 SDM # ====================================================================

EES2019_be_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_be$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_be_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$|_crit$'))  
rm(list = c('respid', 'party'))