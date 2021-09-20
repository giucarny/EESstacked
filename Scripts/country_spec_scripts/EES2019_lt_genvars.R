# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Lithuania Sample) 
# Author: J.Leiser
# last update: 2021-09-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'LT'

EES2019_lt <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lt <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lt <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_lt %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_lt %>%
#   dplyr::select(Q2) %>%
#   val_labels()

# Q9_rec NA
# EES2019_stckd_lt %>% dplyr::select(Q9, Q9_rec) %>% distinct

EES2019_lt_stack <- 
  cbind(EES2019_stckd_lt,  
        lapply(data = EES2019_stckd_lt, 
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
#     EES2019_lt_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
# 
#   }

# checkdataset.fun('Q2')
# checkdataset.fun('Q7')
# checkdataset.fun('Q9_rec')
# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_lt_stack %<>%
  cbind(.,
        lapply(data = EES2019_lt,
               cdbk = EES2019_cdbk_lt,
               stack = EES2019_lt_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$'))  
