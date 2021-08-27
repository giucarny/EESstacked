# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Slovakia Sample) 
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1703
# countryshort = 'SK'

# Keep the EES 2019 Slovakia sample # ===================================================================
EES2019_sk <- 
  EES2019 %>% 
  filter(countrycode==1703)


# Filter the codebook and EP elections data # ==========================================================

# EP2019_sk <- 
#   EP2019 %>% 
#   filter(countryshort=='SK')
# 
# 
# EES2019_cdbk_sk <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='SK')

EES2019_cdbk_sk <-
  EES2019_cdbk %>%
  filter(countryshort=='SK')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_sk$respid %>% 
  as.numeric()

#class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_sk %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_sk %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# votes_crit


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


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_sk$|_crit$'))  
rm(list = c('respid', 'party'))
