# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Finland Sample) 
# Author: J.Leiser
# last update: 2021-09-03
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'FI'

EES2019_fi <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_fi <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_fi <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)


# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_fi %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
#
# EES2019_stckd_fi %>%
#   dplyr::select(Q2) %>%
#   val_labels()

EES2019_fi_stack <- 
  cbind(EES2019_stckd_fi,  
        lapply(data = EES2019_stckd_fi, 
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
#     EES2019_fi_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
# 
#   }
# 
# checkdataset.fun('Q2')
# checkdataset.fun('Q7')
# checkdataset.fun('Q9_rec')
# checkdataset.fun('Q25_rec')

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_fi$|fun'))  
