# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Portugal Sample) 
# Author: G.Carteny, M.Koernig
# last update: 2021-08-27
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 portugal sample # ===================================================================

EES2019_pt <- 
  EES2019 %>% 
  filter(countrycode==1620)


# Filter the codebook data # ============================================================================

# EP2019_pt <- 
#   EP2019 %>% 
#   filter(countryshort=='PT')
# 
# 
# EES2019_cdbk_pt <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='PT')

EES2019_cdbk_pt <-
  EES2019_cdbk %>%
  filter(countryshort=='PT')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_pt$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_pt %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 6 parties and coalitions
#parties and coalitions: PSD, CDS-PP, PS, CDU (coalition), BE, PAN

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_pt %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

#votes_crit: 7 parties and coalitions
#parties and coalitions: BE, CDU, PS, PSD CDS-PP, PAN, A
#all parties/coalitions with PTV variable obtained at least one seat in the EP

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_pt %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Portugal EES 2019 SDM # ====================================================================

EES2019_pt_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_pt$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_pt_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pt$|_crit$'))  
rm(list = c('respid', 'party'))