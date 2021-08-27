# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Germany Sample) 
# Author: W.Haeussling
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Germany sample # ===================================================================

EES2019_de <- 
  EES2019 %>% 
  filter(countrycode==1276)


# Filter the codebook data # ==========================================================

#EP2019_de <- 
#  EP2019 %>% 
#  filter(countryshort=='DE')


EES2019_cdbk_de <- 
  EES2019_cdbk %>% 
  filter(countryshort=='DE')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_de$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_de %>% 
  dplyr::select(partyname, Q10_PTV) 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_de %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats) 

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_de %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the German EES 2019 SDM # ====================================================================

EES2019_de_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_de$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_de$|_crit$'))  
rm(list = c('respid', 'party'))