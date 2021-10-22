# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Austrian Sample) 
# Author: M.Koernig
# last update: 2021-10-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'AT'

EES2019_at <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_at <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_at <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_at %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_at %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_at_stack <- 
  cbind(EES2019_stckd_at,  
        lapply(data = EES2019_stckd_at, 
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
#     EES2019_at_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_at_stack %<>%
  cbind(.,
        lapply(data = EES2019_at,
               cdbk = EES2019_cdbk_at,
               crit = 'average',
               stack = EES2019_at_stack,
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()


# EES2019_at_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fit_lst <-
#   gensyn.fun(data = EES2019_at_stack,
#              depvar = 'Q10_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D6_une'), # 'D6_rec', 'D9_rec'
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)
# 
# fit_lst[[3]] %>% summary
#
# fit_lst[[3]] %>% car::vif(.)


# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_at_stack %<>%
  left_join(.,
            lapply(data = EES2019_at_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), # 'D6_rec', 'D9_rec'
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()


# prediction for party 105 created w/ a different model

# Found disruptive elements for party 105, with high std. errors in variables: D8_rec, D1_rec
# Excluding those two variables in a partial model got rid of the above probelem. However, a Chi-Squared 
# test between the full/unconstrained one and partial/constrained one rejected the H0, that the constrained 
# model fits better at p<0.
# Resulting in the choice to keep the full/unconstrained model6 with high std. errors


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_at$'))  




