# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Romanian Sample) 
# Author: M.Koernig
# last update: 2021-09-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'RO'

EES2019_ro <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_ro <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_ro <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Change idiosyncratic values of the codebook # ========================================================

EES2019_cdbk_ro %<>%
  mutate(Q10_PTV        = case_when(Q7==2302 ~ 'Q10_8', T~Q10_PTV),
         Q13_left_right = case_when(Q7==2302 ~ 'Q13_8', T~Q13_left_right),
         Q24_EU         = case_when(Q7==2302 ~ 'Q24_8', T~Q24_EU)) 

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_ro %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_ro %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_ro_stack <- 
  cbind(EES2019_stckd_ro,  
        lapply(data = EES2019_stckd_ro, 
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
#     EES2019_ro_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_ro_stack %<>%
  cbind(.,
        lapply(data = EES2019_ro,
               cdbk = EES2019_cdbk_ro,
               stack = EES2019_ro_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# error occurs:  Error: Can't subset columns that don't exist.
# x Column `mean (q10_2, q10_8)` doesn't exist.
# Run `rlang::last_error()` to see where the error occurred. 
# Same occurs when run for the other variables run on their own. (q13_2, q13_8, q24_2, q24_8)

# EES2019_ro_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ro$'))  




