# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Belgian Sample) 
# Author: G.Carteny
# last update: 2021-09-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# N.B: The Belgian sample is splitted according to the two electoral colleges of Belgium, namely the 
# Dutch and the French electoral college. 

# Belgian electoral colleges # - - -

el_coll_be <- list('DU-el', 'FR-el')


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'BE'

EES2019_be <- 
  EES2019 %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(meta_lang_be  == 1 ~ 'DU-el',
                                meta_lang_be  == 2 ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

EES2019_stckd_be <- 
  EES2019_stckd %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(meta_lang_be  == 1 ~ 'DU-el',
                                meta_lang_be  == 2 ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

EES2019_cdbk_be <-
  EES2019_cdbk %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(Region  == 'Flanders' ~ 'DU-el',
                                Region  == 'Wallonia' ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

EES2019_be <-  
  lapply(EES2019_be, 
         function(df) {df %<>% mutate(Q25_rec = case_when(is.na(Q25_rec) ~ as.integer(90), T ~ Q25_rec))})

EES2019_stckd_be <- 
  lapply(EES2019_stckd_be, 
         function(df) {df %<>% mutate(Q25_rec = case_when(is.na(Q25_rec) ~ as.integer(90), T ~ Q25_rec))})


EES2019_be_stack <- 
  lapply(EES2019_stckd_be, 
         function(df) {
           df_stack <- 
             cbind(df,  
                   lapply(data = df, 
                          X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
                          stack_var = 'party',
                          FUN = gendic.fun) %>% 
                     do.call('cbind',.)) %>% 
             as_tibble()
         })

  
# Check the dataset 

# EES2019_be_stack %>% 
#   do.call('rbind',.) %>% 
#   dplyr::select(party, contains('gen'))

# Generic distance/proximity variables estimation # ====================================================


EES2019_be_stack <- 
  lapply(el_coll_be, 
         function(x) {
           df_stack <- 
             cbind(EES2019_be_stack[[x]],  
                   lapply(data = EES2019_be[[x]],
                          cdbk = EES2019_cdbk_be[[x]],
                          stack = EES2019_be_stack[[x]],
                          crit = 'average',
                          rescale = T,
                          check = F,
                          keep_id = F,
                          X = list('Q10','Q11','Q23'),
                          FUN = gendis.fun) %>% 
                     do.call('cbind',.)) %>% 
             as_tibble()
         })

names(EES2019_be_stack) <- unlist(el_coll_be)


# Synthetic variables estimation # =====================================================================

gensyn.fun_be <- function(x, depvar, regsum) {
  lst <- gensyn.fun(data = EES2019_be_stack[[x]],
             depvar = depvar,
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #'D6_une', 'D6_rec', 'D9_rec'
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name = 'socdem_synt',
             regsum = regsum)
  
  return(lst)
}



# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# fit_lst <- lapply(el_coll_be, depvar = 'Q10_gen', regsum = T, gensyn.fun_be)

# for(i in 1:length(fit_lst)) {lapply(fit_lst[[i]], summary) %>% print}; rm(i)
# for(i in 1:length(fit_lst)) {lapply(fit_lst[[i]], car::vif) %>% print}; rm(i)

# fit_lst <- lapply(el_coll_be, depvar = 'Q7_gen', regsum = T, gensyn.fun_be)

# for(i in 1:length(fit_lst)) {lapply(fit_lst[[i]], summary) %>% print}; rm(i)
# for(i in 1:length(fit_lst)) {lapply(fit_lst[[i]], car::vif) %>% print}; rm(i)


# If results are fine # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_be_stack <- 
  lapply(el_coll_be,
         function(x){
           left_join(EES2019_be_stack[[x]],
                     gensyn.fun_be(x,depvar='Q10_gen',regsum=F)) %>% 
             left_join(.,
                       gensyn.fun_be(x,depvar='Q7_gen',regsum=F))
         })

names(EES2019_be_stack) <- unlist(el_coll_be)
  

# Clean the environment # ==============================================================================


EES2019_be_stack %<>% 
  do.call('rbind',.) %>% 
  dplyr::select(-c(el_coll_be))

rm(list=ls(pattern='_be$|fit'))  




