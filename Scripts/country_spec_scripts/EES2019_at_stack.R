# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Austrian Sample) 
# Author: G.Carteny, Matthias Körnig(Ger529)
# last update: 2021-08-27
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 austrian sample # ===================================================================

EES2019_at <- 
  EES2019 %>% 
  filter(countrycode==1040)


# Filter the codebook data # ============================================================================

# EP2019_at <- 
#   EP2019 %>% 
#   filter(countryshort=='AT')
# 
# 
# EES2019_cdbk_at <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='AT')

EES2019_cdbk_at <-
  EES2019_cdbk %>%
  filter(countryshort=='AT')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_at$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_at %>% 
  dplyr::select(partyname, Q10_PTV) 

  #ptv_crit: 6 parties
  #parties: ÖVP, SPÖ, NEOS, Grüne, FPÖ, Jetzt

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_at %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

  #votes_crit: 6 parties
  #parties: SPÖ, ÖVP, FPÖ, Grüne, NEOS, Jetzt
  #all parties with PTV variable obtained a seat in the EP. Thus, all 6 parties going into the selection

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_at %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Austrian EES 2019 SDM # ====================================================================

EES2019_at_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_at$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

  #checking plausibility of the stack
  #sapply(EES2019_at_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_at$|_crit$'))  
rm(list = c('respid', 'party'))