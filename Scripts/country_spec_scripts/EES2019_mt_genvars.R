# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Malta Sample) 
# Author: W. Haeussling
# last update: 2021-11-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'MT'

EES2019_mt <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_mt <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_mt <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#       function(vrbl) {
#         EES2019_stckd_mt %>%
#           dplyr::select(all_of(vrbl)) %>%
#           mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#           distinct})
#
# EES2019_stckd_mt %>%
#  dplyr::select(Q2) %>%
#  val_labels()

#Q2 , Q7, Q9_rec and Q25_rec also have value 90 (other parties). This 
#corresponds with a 0 in Q2_gen, Q7_gen, Q9_rec_gen and Q25_rec_gen.
#Given that 90 does not refer to parties, I made no additional changes 
#to the code. 
#Q9_rec also has value NA. This corresponds with a 0 in Q9_rec_gen. Since NA 
#does not refer to parties, I made no additional changes to the code. 

EES2019_mt_stack <- 
  cbind(EES2019_stckd_mt,  
        lapply(data = EES2019_stckd_mt, 
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
#    EES2019_mt_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#   
#   }
# checkdataset.fun('Q9_rec')%>% filter(is.na(Q9_rec)  & Q9_rec_gen!=0)

# Generic distance/proximity variables estimation # ====================================================

EES2019_mt_stack %<>%
  cbind(.,
        lapply(data = EES2019_mt,
               cdbk = EES2019_cdbk_mt,
               stack = EES2019_mt_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_mt_stack %>% 
#  dplyr::select(respid, party, ends_with('gen')) %>% 
#  filter((abs(Q10_gen)>1 & Q10_gen!=98) | (abs(Q11_Q13_gen)>1 & Q11_Q13_gen!=98) |
#           (abs(Q23_Q24_gen)>1 & Q23_Q24_gen!=98))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#fit_lst <-
#  gensyn.fun(data = EES2019_mt_stack,
#             depvar = 'Q7_gen',
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
#   of the qqnorm plots. The plots for list entry 1 has a jump in the middle and 
#   list entries 1 and 2 have outliers at the start of the qqplot, too.
#    Furthermore: For the third and fourth list entries the coefficient 
#   estimate and std. error values for D6_une1, EDU_rec2 and EDU_rec3 were 
#   unusually high.
#   For the fifth list entries the coefficient estimate and std. error values 
#   for EDU_rec3 and D6_une1 were also unusually high.
#For Q10_gen: residuals have mostly normal distribution, but outliers at the 
#   begining and end of the qqnorm plots in regard to the qqline. The plots for 
#   list entries 3,4 and 5 have stronger outliers at the end than 1 and 2.

# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_mt_stack %<>%
  left_join(.,
            lapply(data = EES2019_mt_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 'D1_rec', 'D7_rec'), 
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()

# prediction for party 1904 and 1905 created w/ a different model

#Party 1904
pred_1904_mt <- 
  gensyn.fun(data        = EES2019_mt_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D5_rec', 'EDU_rec', 'D1_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '1904'
  )

EES2019_mt_stack <-   
  left_join(EES2019_mt_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_mt_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=1904) %>% 
              rbind(pred_1904_mt),
            by = c('respid','party'))

#Party 1905
pred_1905_mt <- 
  gensyn.fun(data        = EES2019_mt_stack,
             depvar      = 'Q7_gen',
             cat.indvar  = c('D3_rec', 'D5_rec', 'D1_rec'),
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name   = 'socdem_synt',
             regsum      = F,
             stack_party = '1905'
  )

EES2019_mt_stack <-   
  left_join(EES2019_mt_stack %>% dplyr::select(-c(socdem_synt_vc)),
            EES2019_mt_stack %>% 
              dplyr::select(respid, party, socdem_synt_vc) %>% 
              filter(party!=1905) %>% 
              rbind(pred_1905_mt),
            by = c('respid','party'))


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_mt$'))  
