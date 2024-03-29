# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 party identification variable (Q25) recoding
# Author: G.Carteny
# last update: 2021-09-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  

Q25_recoded <-
  EES2019_cdbk %>%
  mutate(Q25 = as.integer(q25),
         Q25_rec = case_when( is.na(Q25) ~ NA_integer_,
                              !is.na(Q25) ~ Q7)) %>%
  dplyr::select(Q25, Q25_rec) %>%
  distinct() %>% 
  na.omit()

Q25_labs <- val_labels(EES2019$Q25)

EES2019 %<>%
  mutate(Q25 = as.integer(Q25)) %>%
  left_join(., Q25_recoded, by = c('Q25')) %>%
  group_by(countrycode) %>% 
  mutate(Q25_aux = case_when(Q25 < 100 ~ NA_integer_, 
                             T ~ Q25),
         Q25_aux = case_when(Q25_aux==min(Q25_aux, na.rm=T) ~ as.integer(0), 
                             T ~ as.integer(1))) %>% 
  mutate(Q25_rec = case_when(Q25_aux==0 ~ as.integer(0),
                             Q25 < 100 ~ Q25, 
                             T ~ Q25_rec)) %>% 
  dplyr::select(-c(Q25_aux)) %>% 
  ungroup()

val_labels(EES2019$Q25) <- Q25_labs

rm(list=ls(pattern='Q25|labs'))