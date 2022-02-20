# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking - Additional variables directly computed on the SDM
# Author: G.Carteny
# last update: 2022-02-20
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Party identification/Partisanship strength # =========================================================

EES2019_stckd %<>% 
  mutate(Q26_rec = case_when(Q26>3 ~ 99,
                             T     ~ abs(Q26-3)),
         Q26_gen = case_when(as.numeric(Q25_rec_gen)==1 ~ Q26_rec+1,
                             as.numeric(Q25_rec_gen)==0 ~ 0,
                             T                      ~ 99),
         Q26_gen = case_when(Q26_rec>90 ~ Q26_rec, 
                             T          ~ Q26_gen)) 
