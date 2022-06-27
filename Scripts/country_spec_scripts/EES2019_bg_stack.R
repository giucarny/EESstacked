# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Bulgarian Sample) 
# Author: G.Carteny
# last update: 2021-08-30
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Bulgarian sample # ===================================================================

EES2019_bg <- 
  EES2019 %>% 
  filter(countrycode==1110)


# Filter the codebook and EP elections data # ==========================================================

# EP2019_bg <- 
#   EP2019 %>% 
#   filter(countryshort=='BG') %>% 
#   dplyr::select(countryshort, partyname_eng, votesh) %>% 
#   mutate(partyname_eng = partyname_eng %>% gsub('/(.*)','',.))
# 
# 
# EES2019_cdbk_bg <- 
#   EES2019_cdbk %>% 
#   filter(countryshort=='BG')


EES2019_cdbk_bg <- 
  EES2019_cdbk %>% 
  filter(countryshort=='BG')

# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_bg$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_bg %>% 
  dplyr::select(partyname_eng, Q10_PTV) 

# ptv_crit: 7 parties 

# Check the seats obtained by each party - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# seats_crit <- 
#   EES2019_cdbk_bg %>% 
#   dplyr::select(partyname_eng, seats)

votes_crit <- 
  EES2019_cdbk_bg %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# seats_crit: 5 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_bg %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Bulgarian EES 2019 SDM # ====================================================================

EES2019_bg_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_bg$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_bg_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_bg$|_crit$'))  
rm(list = c('respid', 'party'))