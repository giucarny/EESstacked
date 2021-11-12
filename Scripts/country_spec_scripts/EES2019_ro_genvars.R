# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Romanian Sample) 
# Author: M.Koernig
# last update: 2021-10-21
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

# EES2019_ro_stack %>% 
#   dplyr::select(respid, party, ends_with('gen'))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# fit_lst <-
#   gensyn.fun(data = EES2019_ro_stack,
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

EES2019_ro_stack %<>%
  left_join(.,
            lapply(data = EES2019_ro_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec', 'D6_une'), 
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()


# prediction for party 2307 created w/ a different model

pred_2307_ro <- 
  gensyn.fun(data        = EES2019_ro_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D8_rec', 'D5_rec', 'D1_rec', 'D7_rec', 'D6_une'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '2307'
  )

EES2019_ro_stack <-   
  left_join(EES2019_ro_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_ro_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=2307) %>% 
              rbind(pred_2307_ro),
            by = c('respid','party'))


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ro$'))  




