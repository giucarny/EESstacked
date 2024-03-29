# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Estonia Sample) 
# Author: W. Haeussling
# last update: 2021-11-14
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'EE'

EES2019_ee <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_ee <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_ee <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#        function(vrbl) {
#         EES2019_stckd_ee %>%
#           dplyr::select(all_of(vrbl)) %>%
#            mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#            distinct})
# 
# EES2019_stckd_ee %>%
#   dplyr::select(Q9_rec) %>%
#   val_labels()

#Q2 , Q7, Q9_rec and Q25_rec also have value 90 (other parties). This 
#corresponds with a 0 in Q2_gen, Q7_gen, Q9_rec_gen and Q25_rec_gen.
#Given that 90 does not refer to parties, I made no additional changes 
#to the code. 
#Q9_rec and Q25_rec also have value NA. This corresponds with a 0 in Q9_rec_gen 
#and Q25_rec_gen. Since NA does not refer to parties, I made no additional 
#changes to the code. 

EES2019_ee_stack <- 
  cbind(EES2019_stckd_ee,  
        lapply(data = EES2019_stckd_ee, 
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
#    EES2019_ee_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#   
#   }
# checkdataset.fun('Q25_rec')%>% filter(is.na(Q25_rec), Q25_rec_gen!=0)

# Generic distance/proximity variables estimation # ====================================================

EES2019_ee_stack %<>%
  cbind(.,
        lapply(data = EES2019_ee,
               cdbk = EES2019_cdbk_ee,
               stack = EES2019_ee_stack,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_ee_stack %>% 
#  dplyr::select(respid, party, ends_with('gen')) %>% 
#  filter((abs(Q10_gen)>1 & Q10_gen!=98) | (abs(Q11_Q13_gen)>1 & Q11_Q13_gen!=98) |
#           (abs(Q23_Q24_gen)>1 & Q23_Q24_gen!=98))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#fit_lst <-
#  gensyn.fun(data = EES2019_ee_stack,
#             depvar = 'Q10_gen',
#             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D6_une'), 
#             cont.indvar =  c('D4_age', 'D10_rec'),
#             yhat.name = 'socdem',
#             regsum = T)
#check_fun1<- function(l) {
#  l%>%summary
#}
#check_fun2<- function(l) {
#  l%>%car::vif(.)
#}
#check1<-lapply(X=fit_lst[],FUN=check_fun1)
#check2<-lapply(X=fit_lst[],FUN=check_fun2)
#for (i in 1:length(fit_lst)) {
#  print('-----------------------------Number---------------------------------')
#  print(i)
#  print(check1[[i]])
#  print(check2[[i]])
#  qqnorm(fit_lst[[i]]$residuals)
#  qqline(fit_lst[[i]]$residuals)
#}

#For Q7_gen: residuals have mostly normal distribution, but outliers at the end 
#   of the qqnorm plots. Furthermore: For the second list entry the coefficient 
#   estimate and std. error values for D6_une1 were unusually high.
#   For the sixth list entry the coefficient estimate and std. error values for 
#   EDU_rec2 and EDU_rec3 were also unusually high.
#   For the seventh list entry the coefficient estimate and std. error values 
#   for D5_rec1, EDU_rec2, EDU_rec3 and D6_une1 were also unusually high.
#For Q10_gen: residuals have mostly normal distribution, but outliers at the 
#   begining and end of the qqnorm plots in regard to the qqline.

# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_ee_stack %<>%
  left_join(.,
            lapply(data = EES2019_ee_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec'), 
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()

# predictions for parties 906 and 907 created w/ a different model


#Model 6
pred_906_ee <- 
  gensyn.fun(data        = EES2019_ee_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D8_rec',  'D5_rec', 'D1_rec', 'D7_rec','D6_une'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '906'
  )


EES2019_ee_stack <-   
  left_join(EES2019_ee_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_ee_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=906) %>% 
              rbind(pred_906_ee),
            by = c('respid','party'))

#Model 7
pred_907_ee <- 
  gensyn.fun(data        = EES2019_ee_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D8_rec',  'D5_rec', 'D7_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '907'
  )


EES2019_ee_stack <-   
  left_join(EES2019_ee_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_ee_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=907) %>% 
              rbind(pred_907_ee),
            by = c('respid','party'))


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ee$'))  
