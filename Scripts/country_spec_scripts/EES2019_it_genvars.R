# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Italian Sample) 
# Author: G.Carteny
# last update: 2021-10-23
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'IT'

EES2019_it <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_it <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_it <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_it %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_it %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_it_stack <- 
  cbind(EES2019_stckd_it,  
        lapply(data = EES2019_stckd_it, 
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
#     EES2019_it_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_it_stack %<>%
  cbind(.,
        lapply(data = EES2019_it,
               cdbk = EES2019_cdbk_it,
               stack = EES2019_it_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_it_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fit_lst <-
#   gensyn.fun(data = EES2019_cy_stack,
#              depvar = 'Q10_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #'D6_une', 'D6_rec', 'D9_rec'
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)

# lapply(fit_lst, summary)
# lapply(fit_lst, car::vif)
# 
# fit_lst <-
#   gensyn.fun(data = EES2019_cy_stack,
#              depvar = 'Q7_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #, 'D6_une' 'D6_rec', 'D9_rec'
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)

# lapply(fit_lst, summary)
# lapply(fit_lst, car::vif)


# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_it_stack %<>%
  left_join(.,
        lapply(data = EES2019_it_stack,
               cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec'), 
               cont.indvar =  c('D4_age', 'D10_rec'),
               yhat.name = 'socdem_synt',
               regsum = F,
               X = list('Q10_gen','Q7_gen'),
               FUN = gensyn.fun) %>% 
          do.call('left_join',.),
        by = c('respid', 'party')) %>% 
  as_tibble()

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_it$|yhat|fit'))  




