# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Dutch Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Dutch sample # =====================================================================

EES2019_nl <- 
  EES2019 %>% 
  filter(countrycode==1528)


# Filter the codebook and EP elections data # ==========================================================

# EP2019_nl <- 
#   EP2019 %>% 
#   filter(countryshort=='NL')
# 
# 
# EES2019_cdbk_nl <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='NL')

EES2019_cdbk_nl <- 
  EES2019_cdbk %>% 
  filter(countryshort=='NL')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_nl$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_nl %>% 
  dplyr::select(partyname_eng, Q10_PTV) 

#ptv_crit: 9 parties

# Check the seats obtained by each party - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

seats_crit <- 
  EES2019_cdbk_nl %>% 
  dplyr::select(partyname, seats) %>% 
  mutate(seats = case_when(seats==0 ~ NA_integer_, T~seats))

#votes_crit: 9 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_nl %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Dutch EES 2019 SDM # ======================================================================

EES2019_nl_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_nl$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_nl_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_nl$|_crit$'))  
rm(list = c('respid', 'party'))