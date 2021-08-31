# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-09-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'IT'

EES2019_it <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_it <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_it <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values 
# EES2019_stckd_it %>% dplyr::select(Q7) %>% distinct


EES2019_it_stack <- 
  cbind(EES2019_stckd_it,  
        gendic.fun(data = EES2019_stckd_it, 
                   var = 'Q7', 
                   stack_var = 'party')) %>% 
  as_tibble()



# Check the dataset 

# EES2019_it_stack %>% 
#   dplyr::select(respid, party, Q7, Q7_gen) %>% 
#   print(n=100)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_it$'))  




