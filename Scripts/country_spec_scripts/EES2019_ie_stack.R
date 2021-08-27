# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Ireland Sample) 
# Author: M.Koernig, G.Carteny
# last update: 2021-08-27
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 ireland sample # ===================================================================

EES2019_ie <- 
  EES2019 %>% 
  filter(countrycode==1372)


# Filter the codebook data # ===========================================================================

# EP2019_ie <- 
#   EP2019 %>% 
#   filter(countryshort=='IE')
# 
# 
# EES2019_cdbk_ie <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='IE')

EES2019_cdbk_ie <-
  EES2019_cdbk %>%
  filter(countryshort=='IE')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_ie$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_ie %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 6 parties
#parties: FG, LAB, FF, GP, SF, Solidarity

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_ie %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

#votes_crit: 8 parties
#parties: FF, FG, LAB, GP, SF, Ind., SD, I4C
#The party Solidarity shows a PTV variable, but did not obtain a seat in the EP
#All other parties with PTV variable obtained at least one seat in the EP
#Thus, the party Solidarity needs to be filtered out

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_ie %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>%
  filter(partyname!='Solidarity - People Before Profit/') %>%
  .$Q7

#Only 5 parties remain in the selection without Solidarity

# Create the Ireland EES 2019 SDM # ====================================================================

EES2019_ie_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_ie$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

  #checking plausibility of the stack
  #sapply(EES2019_ie_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ie$|_crit$'))  
rm(list = c('respid', 'party'))