# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Estonia Sample) 
# Author: W.Haeussling
# last update: 2021-08-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Estonia sample # ===================================================================

EES2019_ee <- 
  EES2019 %>% 
  filter(countrycode==1233)


# Filter the codebook data # ==========================================================

#EP2019_ee <- 
#  EP2019 %>% 
#  filter(countryshort=='EE')

EES2019_cdbk_ee <- 
  EES2019_cdbk %>% 
  filter(countryshort=='EE')

#EES2019_cdbk is from EESstacked\Scripts\aux_data_scripts\EES2019_cdbk.R

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_ee$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_ee %>% 
  dplyr::select(partyname, Q10_PTV) 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_ee_enhcdbk %>% 
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

#EES2019_ee_enhcdbk is from 
#EESstacked\Scripts\aux_data_scripts\country_spec_aux_scripts\EES2019_enhcdbk_ee.R

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_ee %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Estonian EES 2019 SDM # ====================================================================

EES2019_ee_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_ee$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ee$|_crit$'))  
rm(list = c('respid', 'party'))