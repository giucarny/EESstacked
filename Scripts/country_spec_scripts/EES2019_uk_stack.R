# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, United Kingdom Sample) 
# Author: W.Haeussling
# last update: 2021-08-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 United Kingdom sample # ===================================================================

EES2019_uk <- 
  EES2019 %>% 
  filter(countrycode==1826)


# Filter the codebook data # ==========================================================

#EP2019_uk <- 
#  EP2019 %>% 
#  filter(countryshort=='UK')


EES2019_cdbk_uk <- 
  EES2019_cdbk %>% 
  filter(countryshort=='UK')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_uk$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_uk %>% 
  dplyr::select(partyname, Q10_PTV)

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_uk %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats) 

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_uk %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the British (UK) EES 2019 SDM # ====================================================================

EES2019_uk_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_uk$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#The party UKIP (2806) does not have a seat, but is included. The parties 
#Plaid (2809), SF (2810) and DUP (2811) all have 1 seat, but are not included.

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_uk$|_crit$'))  
rm(list = c('respid', 'party'))