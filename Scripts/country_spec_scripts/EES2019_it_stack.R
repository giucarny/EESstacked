# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-08-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 italian sample # ===================================================================

EES2019_it <- 
  EES2019 %>% 
  filter(countrycode==1380)


# Filter the codebook data # ===========================================================================

# EP2019_it <- 
#   EP2019 %>% 
#   filter(countryshort=='IT')
# 
# 
# EES2019_cdbk_it <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='IT')

EES2019_cdbk_it <-
  EES2019_cdbk %>%
  filter(countryshort=='IT')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_it$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_it %>% 
  dplyr::select(partyname, Q10_PTV) 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_it %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_it %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Italian EES 2019 SDM # ====================================================================

EES2019_it_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_it$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Clean the environment # ==============================================================================
  
rm(list=ls(pattern='_it$|_crit$'))  
rm(list = c('respid', 'party'))