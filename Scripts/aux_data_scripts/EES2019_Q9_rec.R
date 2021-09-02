# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 national elections vote choice variable (Q9) recoding
# Author: G.Carteny
# last update: 2021-09-02
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Q9 should be re-labeled, but for now we leave it w/o labs

# Q9_rec_labs <- EES2019$Q7 %>% val_labels()
# attr(Q9_rec_labs[1], 'names') <- 'does not feel close to any party'


Q9_recoded <-
  EES2019_cdbk %>%
  mutate(Q9_rec = case_when( is.na(Q9) ~ NA_integer_,
                            !is.na(Q9) ~ Q7)) %>%
  dplyr::select(Q9, Q9_rec) %>%
  distinct() %>% 
  na.omit()


EES2019 %<>%
  mutate(Q9 = as.integer(Q9)) %>%
  left_join(., Q9_recoded, by = c('Q9')) %>% 
  mutate(Q9 = case_when(Q9==as.integer(97) ~ as.integer(0), T ~ Q9),
         Q9_rec = case_when(Q9<100 ~ Q9, T ~ Q9_rec))


rm(list=ls(pattern='Q9'))