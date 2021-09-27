# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Luxembourgian Sample) 
# Author: W. Haeussling
# last update: 2021-09-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'LU'

EES2019_lu <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lu <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lu <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#       function(vrbl) {
#         EES2019_stckd_lu %>%
#           dplyr::select(all_of(vrbl)) %>%
#           mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#           distinct})

# EES2019_stckd_lu %>%
#  dplyr::select(Q7) %>%
#  val_labels()

#Q2 , Q7, Q9_rec and Q25_rec also have value 90 (other parties). This 
#corresponds with a 0 in Q2_gen, Q7_gen, Q9_rec_gen and Q25_rec_gen.
#Given that 90 does not refer to parties, I made no additional changes 
#to the code. 
#Q9_rec also has value NA. This corresponds with a 0 in Q9_rec_gen. Since NA 
#does not refer to parties, I made no additional changes to the code. 

EES2019_lu_stack <- 
  cbind(EES2019_stckd_lu,  
        lapply(data = EES2019_stckd_lu, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Check the dataset 

#checkdataset.fun <- 
#  function(vrbl) {
#    
#    orivar <- vrbl
#    genvar <- paste0(vrbl, '_gen')
#    
#    EES2019_lu_stack %>%
#      dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#      print(n=100)
#    
#  }
# checkdataset.fun('Q9_rec')%>%filter(is.na(Q9_rec), Q9_rec_gen!=0)

# Generic distance/proximity variables estimation # ====================================================

EES2019_lu_stack %<>%
  cbind(.,
        lapply(data = EES2019_lu,
               cdbk = EES2019_cdbk_lu,
               stack = EES2019_lu_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_lu_stack %>% 
#  dplyr::select(respid, party, ends_with('gen')) %>% 
#  filter((abs(Q10_gen)>1 & Q10_gen!=98) | (abs(Q11_Q13_gen)>1 & Q11_Q13_gen!=98) |
#           (abs(Q23_Q24_gen)>1 & Q23_Q24_gen!=98))

# Synthetic variables estimation # =====================================================================

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#fit_lst <-
#  gensyn.fun(data = EES2019_lu_stack,
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
#   of the qqnorm plots. The plots for list entry 12 and 14 have outliers at the begining of the 
#   qqplots, too.
#    Furthermore: For the first, second, third and fifth list entries the coefficient 
#   estimate and std. error values for D6_une1 were unusually high.
#   For the eigth and fourteenth list entries the coefficient estimate 
#   and std. error values for EDU_rec2, EDU_rec3 and D6_rec1 were also unusually 
#   high with rather low residual deviance.
#    For the ninth list entry the coefficient estimate and std. error values for 
#   D8_rec1, EDU_rec2, EDU_rec3 and D6_une1 were also unusually high with rather 
#   low residual deviance. 
#   For the tenth list entry the coefficient estimate and std. error values for 
#   EDU_rec2 and EDU_rec3 were also unusually.
#     For the eleventh list entry the coefficient estimate and std. error values for 
#   D3_rec2, D8_rec1, EDU_rec2, EDU_rec3, D6_une1 and D10_rec were also unusually high
#   and the std. error values for D5_rec1 were also unusually high, but with rather 
#   low residual deviance. Also the VIF around 2 for D3_rec and EDU_rec 
#   and ~2.79 for D8_rec hint at moderate correlation.
#     For the twelfth list entry the coefficient estimate and std. error values for 
#   D8_rec1, EDU_rec2, EDU_rec3 were also unusually high and the std. error 
#   value for D6_une1 was also unusually high, but with rather low residual 
#   deviance. Also the VIF between 2 qnd 5 for D3_rec, D5_rec and D4_rec and 
#   D10_rec with 1.8 hint at moderate correlation, and the EDU_rec VIF value of
#   ~8.895 and for D6_une of ~7.8 hint at high correlation.
#    For the thirteenth list entry the coefficient estimate and std. error values for 
#   D5_rec1, EDU_rec2, EDU_rec3 and D6_une1 were also unusually high with rather 
#   low residual deviance. 
#For Q10_gen: residuals have mostly normal distribution, but outliers at the 
#   begining and end of the qqnorm plots. The plots for list entries 5 ,
#   6 , 7, 12 and 13 have a kind of cyclical deviation from the qqline. 

# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_lu_stack %<>%
  left_join(.,
            lapply(data = EES2019_lu_stack,
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), 
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem_synt',
                   regsum = F,
                   X = list('Q10_gen','Q7_gen'),
                   FUN = gensyn.fun) %>% 
              do.call('left_join',.),
            by = c('respid', 'party')) %>% 
  as_tibble()

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lu$'))  
