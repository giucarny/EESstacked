# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Cypriot Sample) 
# Author: G.Carteny
# last update: 2021-08-18
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 Cypriot sample # ===================================================================

EES2019_cy <- 
  EES2019 %>% 
  filter(countrycode==1196)


# Filter the codebook and EP elections data # ==========================================================

EP2019_cy <- 
  EP2019 %>% 
  filter(countryshort=='CY') %>% 
  dplyr::select(countryshort, partyname_eng, votesh) %>% 
  mutate(partyname_eng = partyname_eng %>% gsub('.*/','',.) %>% str_trim())  
  

EES2019_cdbk_cy <- 
  EES2019_cdbk %>% 
  filter(countryshort=='CY') %>% 
  mutate(partyname_eng = partyname_eng %>% gsub('.*/','',.) %>% str_trim())  


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_cy$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_cy %>% 
  dplyr::select(partyname_eng, Q10_PTV, Q7) 

#ptv_crit: 6 parties

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_cy %>% 
  filter(partyname_eng!='Other parties') 

#votes_crit: 7 parties

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_cy %>%
  mutate(Q7=case_when(Q10_PTV=='Q10_5' ~ as.integer(505), T~Q7)) %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7


# Create the Cypriot EES 2019 SDM # ====================================================================

EES2019_cy_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_cy$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_cy_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_cy$|_crit$'))  
rm(list = c('respid', 'party'))