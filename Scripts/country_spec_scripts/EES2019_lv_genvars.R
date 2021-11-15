# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Latvian Sample) 
# Author: M.Koernig
# last update: 2021-10-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'LV'

EES2019_lv <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lv <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lv <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#          EES2019_stckd_lv %>%
#            dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_lv %>%
#   dplyr::select(Q2) %>%
#   val_labels()


EES2019_lv_stack <- 
  cbind(EES2019_stckd_lv,  
        lapply(data = EES2019_stckd_lv, 
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
#     EES2019_lv_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#     
#   }

# checkdataset.fun('Q25_rec')

# Generic distance/proximity variables estimation # ====================================================

EES2019_lv_stack %<>%
  cbind(.,
        lapply(data = EES2019_lv,
               cdbk = EES2019_cdbk_lv,
               stack = EES2019_lv_stack,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_lv_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fit_lst <-
#   gensyn.fun(data = EES2019_lv_stack,
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

EES2019_lv_stack %<>%
  left_join(.,
            lapply(data = EES2019_lv_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec', 'D6_une'), 
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()


# prediction for party 1611, 1610, 1604 created w/ a different model

pred_1611_1610_1604_lv <- 
  gensyn.fun(data        = EES2019_lv_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D8_rec', 'D5_rec', 'D1_rec', 'D7_rec', 'D6_une'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = c('1611', '1610', '1604')
  )

EES2019_lv_stack <-   
  left_join(EES2019_lv_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_lv_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=c(1611, 1610, 1604)) %>% 
              rbind(pred_1611_1610_1604_lv),
            by = c('respid','party'))


# prediction for party 1608 created w/ a different model

# Found disruptive element in party 1605, with high std. errors in variable D6_une
# However the didnt inc´fluence the constant


# prediction for party 1605 created w/ a different model

# Found disruptive element in party 1605, with high std. errors in variable D5_rec
# However the didnt inc´fluence the constant


# prediction for party 1616 created w/ a different model

# Party 1616 also shows inflated SE at the constant through EDU_rec and D6_rec. However, here we step 
# back to implement a partial model because a constrained model without those variables was rejected at 
# p<0.001 by a Chi-squared test in favor of the unconstrained model


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lv$'))  




