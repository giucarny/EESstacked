# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Slovakia Sample) 
# Author: J.Leiser
# last update: 2021-11-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'SK'

EES2019_sk <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_sk <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_sk <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_sk %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_sk %>%
#   dplyr::select(Q2) %>%
#   val_labels()

EES2019_sk_stack <- 
  cbind(EES2019_stckd_sk,  
        lapply(data = EES2019_stckd_sk, 
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
#     EES2019_sk_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
# 
#   }

# checkdataset.fun('Q2')
# checkdataset.fun('Q7')
# checkdataset.fun('Q9_rec')
# checkdataset.fun('Q25_rec')


# Generic distance/proximity variables estimation # ====================================================

EES2019_sk_stack %<>%
  cbind(.,
        lapply(data = EES2019_sk,
               cdbk = EES2019_cdbk_sk,
               stack = EES2019_sk_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fit_lst <-
#   gensyn.fun(data = EES2019_sk_stack,
#              depvar = 'Q7_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'),
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)
# fit_lst[[3]] %>% summary # converged
# fit_lst[[3]] %>% car::vif(.) #ok
# 
# fit_lst <-
#   gensyn.fun(data = EES2019_sk_stack,
#              depvar = 'Q10_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'),
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)
# fit_lst[[3]] %>% summary
# fit_lst[[3]] %>% car::vif(.) #ok

EES2019_sk_stack %<>%
  left_join(.,
            lapply(data = EES2019_sk_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec', 'D6_une'),
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()

# Warning message:
#   glm.fit: fitted probabilities numerically 0 or 1 occurred 

# prediction for party 2507 with different model
# remove: EDU_rec, D1_rec, D6_une

pred_2507_sk <- 
  gensyn.fun(data        = EES2019_sk_stack,
             depvar      = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'D7_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '2507'
  )

EES2019_sk_stack <-   
  left_join(EES2019_sk_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_sk_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=2507) %>% 
              rbind(pred_2507_sk),
            by = c('respid','party'))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_sk$|yhat|fit'))  
