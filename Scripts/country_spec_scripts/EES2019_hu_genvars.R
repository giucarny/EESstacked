# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Hungary Sample) 
# Author: J.Leiser
# last update: 2021-09-13
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'HU'

EES2019_hu <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_hu <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_hu <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_hu %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
#
# EES2019_stckd_hu %>%
#   dplyr::select(Q2) %>%
#   val_labels()

EES2019_hu_stack <-
  cbind(EES2019_stckd_hu,
        lapply(data = EES2019_stckd_hu,
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
#     EES2019_hu_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#
#   }

# checkdataset.fun('Q2')
# checkdataset.fun('Q7')
# checkdataset.fun('Q9_rec')
# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

# checking the variables
# x <-
#   gendis.fun(data = EES2019_hu,
#              cdbk = EES2019_cdbk_hu,
#              vrbl = 'Q10',
#              crit = 'average',
#              rescale = T,
#              check = T,
#              keep_id = T)
# print(x[[1]], n = 100)
# print(x[[2]], n =100)
# 
# x <-
#   gendis.fun(data = EES2019_hu,
#              cdbk = EES2019_cdbk_hu,
#              vrbl = 'Q11',
#              crit = 'average',
#              rescale = T,
#              check = T,
#              keep_id = T)
# 
# print(x[[1]], n = 100)
# print(x[[2]], n =100)
# 
# x <-
#   gendis.fun(data = EES2019_hu,
#              cdbk = EES2019_cdbk_hu,
#              vrbl = 'Q23',
#              crit = 'average',
#              rescale = T,
#              check = T,
#              keep_id = T)
# print(x[[1]], n = 100)
# print(x[[2]], n =100)

EES2019_hu_stack %<>%
  cbind(.,
        lapply(data = EES2019_hu,
               cdbk = EES2019_cdbk_hu,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_hu$'))  

