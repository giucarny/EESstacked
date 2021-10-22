# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Slovenian Sample) 
# Author: M.Koernig
# last update: 2021-10-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'SI'

EES2019_si <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_si <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_si <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_si %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_si %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_si_stack <- 
  cbind(EES2019_stckd_si,  
        lapply(data = EES2019_stckd_si, 
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
#     EES2019_si_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_si_stack %<>%
  cbind(.,
        lapply(data = EES2019_si,
               cdbk = EES2019_cdbk_si,
               stack = EES2019_si_stack,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_si_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fit_lst <-
#   gensyn.fun(data = EES2019_si_stack,
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

EES2019_si_stack %<>%
  left_join(.,
            lapply(data = EES2019_si_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), # 'D6_rec', 'D9_rec'
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()


# prediction for party 2405, 2408 created w/ a different model

pred_2405_2408_si <- 
  gensyn.fun(data        = EES2019_si_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D8_rec', 'D5_rec', 'D1_rec', 'D7_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = c('2405', '2408')
  )

EES2019_si_stack <-   
  left_join(EES2019_si_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_si_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=c(2405, 2408)) %>% 
              rbind(pred_2405_2408_si),
            by = c('respid','party'))

# Note to party 2405
# Next to EDU_rec another disruptive element in party 2405 with high std. error is: D7_rec
# However, a Chi-Squared test between the full/unconstrained one and partial/constrained, excluding both 
# EDU_rec and D7_rec, rejects the H0, that the constrained model fits better at p<001.
# When just excluding EDU_rec, H0 can only be rejected at p<0.1. 
# Resulting in the choice to just exclude EDU_rec to get rid of at least a part of the disruptive elements



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_si$'))  




