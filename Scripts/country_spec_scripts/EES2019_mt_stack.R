# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Malta Sample) 
# Author: W.Haeussling
# last update: 2021-08-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Malta sample # ===================================================================

EES2019_mt <- 
  EES2019 %>% 
  filter(countrycode==1470)


# Filter the codebook data # ===========================================================================

#EP2019_mt <- 
#  EP2019 %>% 
#  filter(countryshort=='MT')


EES2019_cdbk_mt <- 
  EES2019_cdbk %>% 
  filter(countryshort=='MT')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_mt$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_mt %>% 
  dplyr::select(partyname, Q10_PTV) 

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EES2019_cdbk_mt %>%
  mutate(seats = case_when(seats==as.integer(0) ~ NA_integer_, T~seats)) %>% 
  dplyr::select(partyname, votesh, seats)

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_mt %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Maltese EES 2019 SDM # ====================================================================

EES2019_mt_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_mt$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#The parties Alternattiva Demokratika (1903), Partit Demokratiku (1904) and 
#Imperu Ewropew (1905) don't have a seat but are included.

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_mt$|_crit$'))  
rm(list = c('respid', 'party'))