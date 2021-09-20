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


EES2019_be_stack %<>% 
  do.call('rbind',.) %>% 
  dplyr::select(-c(el_coll_be))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$'))  




