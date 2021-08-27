# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Lithuania Sample) 
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1440
# countryshort = 'LT'

# Keep the EES 2019 Lithuania sample # ===================================================================
EES2019_lt <- 
  EES2019 %>% 
  filter(countrycode==1440)

# Filter the codebook data # ==========================================================

# EP2019_lt <- 
#   EP2019 %>% 
#   filter(countryshort=='LT')
# 
# 
# EES2019_cdbk_lt <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='LT')

EES2019_cdbk_lt <-
  EES2019_cdbk %>%
  filter(countryshort=='LT')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_lt$respid %>% 
  as.numeric()

#class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_lt %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit


# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_lt %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)
 
# votes_crit

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



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$|_crit$'))  
rm(list = c('respid', 'party'))