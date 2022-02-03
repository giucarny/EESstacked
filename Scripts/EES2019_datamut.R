# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Mutating the original EES2019 voter study
# Author: G.Carteny
# last update: 2022-02-03
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Notes # 

# List of vars for yhats: 
# as.factor('D4_age', 'D3_rec', 'D8_rec', 'D9_rec', 'D5_rec', 'D6_rec', 'EDU_rec') 
# as.numeric('D10_rec')

# Create a 'countryname' variable # ====================================================================

EES2019 <-
  EES2019 %>% 
  left_join(.,
            data.frame(countrycode = EES2019$countrycode %>% val_labels() %>% as.numeric,
                       countryname = EES2019$countrycode %>% val_labels() %>% attr(.,'names')),
            by='countrycode')

# Create a 'countryshort' variable # ===================================================================

EES2019 %<>% 
  mutate(countryshort = region_NUTS1 %>% str_extract(pattern = '^.{0,2}')) %>% 
  mutate(countryshort = case_when(countrycode==1250 ~ 'FR',
                                  countrycode==1428 ~ 'LV',
                                  countrycode==1442 ~ 'LU',
                                  countrycode==1470 ~ 'MT',
                                  T ~ countryshort))

# Change missing values for Q2, Q7, Q9,... # ===========================================================

Q2_labs  <- val_labels(EES2019$Q2)
Q7_labs  <- val_labels(EES2019$Q7)
Q9_labs  <- val_labels(EES2019$Q9)
Q26_labs <- val_labels(EES2019$Q26)

EES2019 %<>% 
  mutate(across(c(Q2, Q7, Q9, Q26), ~as.integer(.)),
         Q2  = case_when(Q2==10 ~ as.integer(90), 
                         Q2==11 ~ as.integer(0),
                         Q2==9999 ~ as.integer(96), 
                         T ~ Q2),
         Q7  = case_when(Q7==as.integer(9999) ~ as.integer(0), 
                         T ~ Q7),
         Q9  = case_when(Q9==as.integer(97) ~ as.integer(0), 
                         T ~ Q9),
         Q26 = case_when(Q26==999 ~ as.integer(0),
                         T ~ Q26))

Q2_labs[Q2_labs %in% c(10,11,9999)] <- c(90,0,96)
Q7_labs                             <- Q7_labs[-length(Q7_labs)]
Q9_labs[Q9_labs %in% c(97)]         <- c(0)
Q26_labs                            <- Q26_labs[-length(Q26_labs)]

val_labels(EES2019$Q2)  <- Q2_labs
val_labels(EES2019$Q7)  <- Q7_labs
val_labels(EES2019$Q9)  <- Q9_labs
val_labels(EES2019$Q26) <- Q26_labs

rm(list=ls(pattern='labs'))

# Create an 'age' variable # ===========================================================================

EES2019 %<>% mutate(D4_age = 2019 - D4_1) 

# Mutate gender/sex # =================================================================================

# gndr_summ <- data.frame(gndr     = EES2019$D3 %>% val_labels %>% unique(),
#                         gndr_lab = EES2019$D3 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D3_rec = as.numeric(D3),
         D3_rec = case_when(D3_rec==3 ~ NA_real_, T ~ D3_rec))

# Mutate urban rural # =================================================================================

# urbrur_summ <- data.frame(urbrur     = EES2019$D8 %>% val_labels %>% unique(),
#                           urbrur_lab = EES2019$D8 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D8_rec = as.numeric(D8),
         D8_rec = case_when(D8_rec==1 ~ 0,
                            D8_rec==2 | D8_rec==3 ~ 1, 
                            T ~ D8_rec))  

# Mutate religious denomination and invert religiosity # ===============================================

# D9_summ <- data.frame(D9 = EES2019$D9 %>% val_labels %>% unique(),
#                            D9_labs = EES2019$D9 %>% val_labels %>% attr(., 'names'))


EES2019 %<>% 
  mutate(D9_rec  = as.numeric(D9),
         D10_rec = as.numeric(D10)) %>%
  mutate(D9_rec = case_when(D9_rec==1 ~ 1, # 'Catholic', 
                            D9_rec==2 ~ 2, # 'Orthodox',  
                            D9_rec==3 ~ 3, # 'Protestant',  
                            D9_rec==4 ~ 4, # 'Other Christian',  
                            D9_rec>=5 & D9_rec<10 ~ 5, # 'Other', 
                            D9_rec==10 | D9_rec==11 ~ 0, # 'Non-believer',
                            D9_rec==12 ~ 6, # 'Other'
                            D9_rec>12  ~ NA_real_)) %>%
  mutate(D10_rec = case_when(D10_rec>8 ~ NA_real_, T~D10_rec),
         D10_rec = abs(D10_rec-8),
         D10_rec = case_when(D9_rec==0 & D10==as.numeric(96)  ~ 0,
                             T            ~ D10_rec))
# ,
#          D10_rec = case_when(D9_rec==0    ~ 0, 
#                              T            ~ D10_rec),
#          D10_rec = case_when(D10 > as.numeric(96) ~ NA_real_, 
#                              T                    ~ D10_rec),
#          ) 

# Mutate marital status # ==============================================================================

# mrtlst_summ <- data.frame(mrtlst     = EES2019$D5 %>% val_labels %>% unique(),
#                           mrtlst_lab = EES2019$D5 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D5_rec = as.numeric(D5),
         D5_rec = case_when(D5_rec>14 ~ NA_real_,
                            D5_rec>=1 & D5_rec<=8 ~ 1, 
                            D5_rec>=9 & D5_rec<=14 ~ 0))


# Mutate occupation # ==================================================================================

# occptn_summ <- data.frame(occptn     = EES2019$D6 %>% val_labels %>% unique(),
#                           occptn_lab = EES2019$D6 %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(D6_rec = as.numeric(D6)) %>% 
  mutate(D6_rec = case_when(D6_rec > 7 ~ NA_real_, T~D6_rec),
         D6_std = case_when(D6_rec==3 ~ 1, is.na(D6_rec) ~ NA_real_, T~0),
         D6_une = case_when(D6_rec==6 ~ 1, is.na(D6_rec) ~ NA_real_, T~0))

# Mutate education # ===================================================================================

# edctn_summ <- data.frame(edctn     = EES2019$EDU %>% val_labels %>% unique(),
#                          edctn_lab = EES2019$EDU %>% val_labels %>% attr(., 'names'))

EES2019 %<>%
  mutate(EDU_rec = as.numeric(EDU)) %>%
  # mutate(EDU_rec = case_when(EDU_rec > 3 ~ NA_real_, T ~ EDU_rec))
  mutate(EDU_rec = case_when(EDU_rec==97 ~ 0, EDU_rec %in% c(4,99) ~ NA_real_, T ~ EDU_rec),
         EDU_0_rec = case_when(EDU_rec==0 ~ D4_age-6, T~NA_real_),
         EDU_rec = case_when(EDU_0_rec <= 15 ~ 1,
                             EDU_0_rec > 15 &  EDU_0_rec <= 19 ~ 2,
                             EDU_0_rec > 19 ~ 3,
                             T ~ EDU_rec)) %>%
  dplyr::select(-c(EDU_0_rec))


# Mutate trade union membership # ======================================================================

EES2019 %<>% 
  mutate(D1_rec = case_when(as.numeric(D1) <= 3 ~ 1, 
                            as.numeric(D1) == 4 ~ 0,
                            as.numeric(D1) >  4 ~ NA_real_)) %>% 
  mutate(D1_rec = as.factor(D1_rec))
#                            T ~ as.numeric(D1)))


# Mutate subjective social class # =====================================================================

EES2019 %<>%
  mutate(D7_rec = case_when(as.numeric(D7) <= 2 ~ 0, 
                            as.numeric(D7) == 3 ~ 1,
                            as.numeric(D7) == 4 | as.numeric(D7) == 5 ~ 2,
                            as.numeric(D7) >  5 ~ NA_real_)) %>%
  mutate(D7_rec = as.factor(D7_rec))
 
#                             as.numeric(D7) == 6 | as.numeric(D7) == 7 ~ 97,
#                             T ~ as.numeric(D7)))




