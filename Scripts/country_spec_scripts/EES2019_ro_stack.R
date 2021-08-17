# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Romanian Sample) 
# Author: G.Carteny, M.Koernig
# last update: 2021-08-12
# Based on the template from the Italy stack. Just countrycode and names exchanged
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Keep the EES 2019 romanian sample # ===================================================================

EES2019_ro <- 
  EES2019 %>% 
  filter(countrycode==1642)


# Filter the codebook and EP elections data # ==========================================================

EP2019_ro <- 
  EP2019 %>% 
  filter(countryshort=='RO')


EES2019_cdbk_ro <- 
  EES2019_cdbk %>% 
  filter(countryshort=='RO')


# Get the respondent ID codes # ========================================================================

respid <- 
  EES2019_ro$respid %>% 
  as.numeric()

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <-
  EES2019_cdbk_ro %>% 
  dplyr::select(partyname, Q10_PTV) 

#ptv_crit: 9 parties and coalitions
#parties/coalitions: PSD, USR, ALDE, PPR, PNL, UDMR, PMP, +Plus, Alliance (coalition)
#note: 2 of the 9 parties (USR, +Plus) form the Aliance Coalition

# Check the vote shares of parties that obtained at least one seat in the EP # - - - - - - - - - - - - -

votes_crit <- 
  EP2019_ro %>% 
  filter(party_name!='Other parties') 

#votes_crit: 7 parties and coalitions
#party/coalition: PSD, ALDE, PNL, UDMR, PMP, Pro Romania, Coal. Alliance
#note: Pro Romania is PPR in ptv_crit

#all parties with PTV variable obtained at least one seat in the EP. However the two of them
#(USR, +Plus) are listed uniquely and in the coalition

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- 
  EES2019_cdbk_ro %>% 
  dplyr::select(partyname, Q10_PTV, Q7) %>%   
  na.omit() %>% 
  .$Q7

#Only the coalition Alliance of USR and +Plus listed, not USR, +Plus individually
#Thus, only 7 parties/coalitions listed

# Create the Romanian EES 2019 SDM # ====================================================================

EES2019_ro_stack <- 
  expand_grid(respid, party) %>% 
  mutate(countrycode=EES2019_ro$countrycode %>% unique,
         stack = paste0(respid, '-', party)) %>%
  dplyr::select(countrycode, respid, party, stack)

#checking plausibility of the stack
#sapply(EES2019_ro_stack, function(x) length(unique(x)))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ro$|_crit$'))  
rm(list = c('respid', 'party'))