# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Irish Sample) 
# Author: M.Koernig
# last update: 2021-09-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'IE'

EES2019_ie <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_ie <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_ie <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_ie %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_ie %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_ie_stack <- 
  cbind(EES2019_stckd_ie,  
        lapply(data = EES2019_stckd_ie, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Check the dataset 

# checkdataset.fun <- 
#   function(vrbl) {
#     
#     orivar <- vrbl
#     genvar <- paste0(vrbl, '_gen')
#     
#     EES2019_ie_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ie$'))  



