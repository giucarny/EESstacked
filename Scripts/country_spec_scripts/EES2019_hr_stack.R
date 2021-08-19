# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Croatian Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Croatian sample # ===================================================================

EES2019_hr <- 
  EES2019 %>% 
  filter(countrycode==1191)


# Filter the codebook and EP elections data # ==========================================================

EP2019_hr <- 
  EP2019 %>% 
  filter(countryshort=='HR')


EES2019_cdbk_hr <- 
  EES2019_cdbk %>% 
  filter(countryshort=='HR')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_hr$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_hr %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 7 parties

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_hr %>% 
  filter(partyname!='Other parties') 

#votes_crit: 14 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_hr %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Croatian EES 2019 SDM # ====================================================================

EES2019_hr_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_hr$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_hr_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hr$|_crit$'))  
rm(list = c('respid', 'party'))