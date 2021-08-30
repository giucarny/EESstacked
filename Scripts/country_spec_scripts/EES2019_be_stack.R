# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Belgian Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Belgian sample # ===================================================================

EES2019_be <- 
  EES2019 %>% 
  filter(countrycode==1056)


# Filter the codebook and EP elections data # ==========================================================

EES2019_cdbk_be <- 
  EES2019_cdbk %>% 
  filter(countryshort=='BE')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_be$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_be %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 14 parties (7 Flanders, 7 Wallonia)

# Check the seats obtained by each party - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  seats_crit <- 
  EES2019_cdbk_be %>% 
  dplyr::select(partyname, seats) %>% 
  mutate(seats = case_when(seats==0 ~ NA_integer_, T~seats))

#votes_crit: 19 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_be %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Belgian EES 2019 SDM # ====================================================================

EES2019_be_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_be$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_be_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$|_crit$'))  
rm(list = c('respid', 'party'))