# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Poland Sample) 
# Author: J.Leiser
# last update: 2021-08-12
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# countrycode = 1616
# countryshort = 'PL'

# Keep the EES 2019 Poland sample # ===================================================================
EES2019_pl <- 
  EES2019 %>% 
  filter(countrycode==1616)

# Filter the codebook data # ==========================================================

# EP2019_pl <- 
#   EP2019 %>% 
#   filter(countryshort=='PL')
# 
# 
# EES2019_cdbk_pl <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='PL')

EES2019_cdbk_pl <-
  EES2019_cdbk %>%
  filter(countryshort=='PL')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_pl$respid %>% 
  as.numeric()

#class(respid) #check class

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_pl %>% 
  dplyr::select(partyname, Q10_PTV) 

# ptv_crit



# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_pl %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# votes_crit


# All parties with PTV variable are in vote_crit either independently or as part of a 
# coalition

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_pl %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

# We keep KE which contains PO, PSL and SLD parties. These 3 parties also lack values in Q7.

# party


# Create the Poland EES 2019 SDM # ====================================================================

EES2019_pl_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_pl$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pl$|_crit$'))  
rm(list = c('respid', 'party'))
