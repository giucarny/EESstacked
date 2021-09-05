# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Luxembourgian Sample) 
# Author: W. Haeussling
# last update: 2021-09-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'LU'

EES2019_lu <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lu <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lu <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
#lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#       function(vrbl) {
#         EES2019_stckd_lu %>%
#           dplyr::select(all_of(vrbl)) %>%
#           mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#           distinct})
#
# EES2019_stckd_lu %>%
#  dplyr::select(Q7) %>%
#  val_labels()

#Q2 also has values 10 (other parties) and 11 (none of the parties).
#Q7 also has values 90 (other parties).
#Q9_rec and Q25_rec also have value 90 (other parties).
#Given that these values don't refer to parties, I made no additional changes 
#to the code. 

EES2019_lu_stack <- 
  cbind(EES2019_stckd_lu,  
        lapply(data = EES2019_stckd_lu, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Check the dataset 

#checkdataset.fun <- 
#  function(vrbl) {
#    
#    orivar <- vrbl
#    genvar <- paste0(vrbl, '_gen')
#    
#    EES2019_lu_stack %>%
#      dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#      print(n=100)
#    
#  }
# checkdataset.fun('Q2')

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lu$|fun'))  


