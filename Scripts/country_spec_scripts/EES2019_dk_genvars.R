# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Danish Sample) 
# Author: W. Haeussling
# last update: 2021-09-06
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'DK'

EES2019_dk <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_dk <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_dk <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#             function(vrbl) {
#               EES2019_stckd_dk %>%
#                 dplyr::select(all_of(vrbl)) %>%
#                 mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#                 distinct})
#
# EES2019_stckd_dk %>%
#  dplyr::select(Q25_rec) %>%
#  val_labels()

#Q2 , Q7, Q9_rec and Q25_rec also have value 90 (other parties). This 
#corresponds with a 0 in Q2_gen, Q7_gen, Q9_rec_gen and Q25_rec_gen.
#Given that 90 does not refer to parties, I made no additional changes 
#to the code. 
#Q9_rec and Q25_rec also have value NA. This corresponds with a 0 in Q9_rec_gen 
#and Q25_rec_gen. Since NA does not refer to parties, I made no additional 
#changes to the code. 

EES2019_dk_stack <- 
  cbind(EES2019_stckd_dk,  
        lapply(data = EES2019_stckd_dk, 
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
#    EES2019_dk_stack %>%
#      dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#      print(n=100)
#    
#  }
# checkdataset.fun('Q9_rec') %>% filter(is.na(Q9_rec), Q9_rec_gen!=0)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_dk$'))  