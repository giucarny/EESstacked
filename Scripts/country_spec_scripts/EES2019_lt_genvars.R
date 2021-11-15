# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Lithuania Sample) 
# Author: J.Leiser
# last update: 2021-11-08
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

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fit_lst <-
#   gensyn.fun(data = EES2019_lt_stack,
#              depvar = 'Q7_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'),
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)
# fit_lst[[3]] %>% summary # converged
# fit_lst[[3]] %>% car::vif(.) #ok
# 
# fit_lst <-
#   gensyn.fun(data = EES2019_lt_stack,
#              depvar = 'Q10_gen',
#              cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'),
#              cont.indvar =  c('D4_age', 'D10_rec'),
#              yhat.name = 'socdem',
#              regsum = T)
# fit_lst[[3]] %>% summary
# fit_lst[[3]] %>% car::vif(.) #ok

EES2019_lt_stack %<>%
  left_join(.,
            lapply(data = EES2019_lt_stack,
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

# prediction for party 1706, 1702 and 1707 with different models

# party 1706: remove EDU_rec
pred_1706_lt <- 
  gensyn.fun(data        = EES2019_lt_stack,
             depvar      = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'D1_rec', 'D7_rec', 'D6_une'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '1706'
  )

# party 1702: remove EDU_rec
pred_1702_lt <- 
  gensyn.fun(data        = EES2019_lt_stack,
             depvar      = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'D1_rec', 'D7_rec', 'D6_une'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '1702'
  )

# party 1707: remove EDU_rec, D6_une, D7_rec
pred_1707_lt <- 
  gensyn.fun(data        = EES2019_lt_stack,
             depvar      = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'D1_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '1707'
  )

EES2019_lt_stack <-   
  left_join(EES2019_lt_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_lt_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=1706, party!=1702, party!=1707) %>% 
              rbind(pred_1707_lt, pred_1702_lt, pred_1706_lt),
            by = c('respid','party'))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$|yhat|fit'))  
