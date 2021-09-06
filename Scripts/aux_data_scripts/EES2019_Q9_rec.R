# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 national elections vote choice variable (Q9) recoding
# Author: G.Carteny
# last update: 2021-09-02
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Q9_recoded <-
  EES2019_cdbk %>%
  mutate(Q9_rec = case_when( is.na(Q9) ~ NA_integer_,
                            !is.na(Q9) ~ Q7)) %>%
  dplyr::select(Q9, Q9_rec) %>%
  distinct() %>% 
  na.omit()

Q9_labs <- val_labels(EES2019$Q9)

EES2019 %<>%
  mutate(Q9 = as.integer(Q9)) %>%
  left_join(., Q9_recoded, by = c('Q9')) %>% 
  mutate(Q9_rec = case_when(Q9<100 ~ Q9, T ~ Q9_rec))

val_labels(EES2019$Q9) <- Q9_labs

rm(list=ls(pattern='Q9|labs'))