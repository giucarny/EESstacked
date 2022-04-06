# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking Script 
# Author: G.Carteny
# last update: 2022-03-08
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr", "rlang", "car",
         "caret", "labelled", "janitor")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())
# rm(list= ls()[!(ls() %in% c('keepThis','andThis'))]) # useful for further implementations

# Load & Mutate EES data # =============================================================================

# Load the full version of the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <- read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

# Mutate the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

source(here('Scripts', 'EES2019_datamut.R'))


# Source the auxiliary data set # ======================================================================

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk_enh.R'))


# Harmonize Q25 and Q9 values # ========================================================================

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q25_rec.R'))

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q9_rec.R'))


# Load auxiliary functions # ===========================================================================

source(here('Scripts', 'EES2019_stack_funs.R'))


# Stack observations # =================================================================================

invisible(
  lapply(paste0(here('Scripts', 'country_spec_scripts'),'/', 
                here('Scripts', 'country_spec_scripts') %>% list.files(pattern = '_stack')),
         source)  
)

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
rm(list=ls(pattern='_stack'))


# Stack the original EES2019 variables # ===============================================================

EES2019_stckd %<>% left_join(., EES2019, by=c('countrycode', 'respid'))

# Estimate the generic variables # =====================================================================

set.seed(3369)

invisible(
  lapply(paste0(here('Scripts', 'country_spec_scripts'),'/',
                here('Scripts', 'country_spec_scripts') %>% list.files(pattern = '_genvars')),
         source)
)

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
rm(list=ls(pattern='_stack'))

# Compute additional variables on the SDM # ============================================================

source(here('Scripts','EES2019_stack_vrbls.R'))

# Reshape the dataset before saving # ==================================================================

oldcols <- 
  EES2019_stckd[ , c(1:131)] %>% 
  dplyr::select(-c(party, stack)) # %>% distinct

x <- EES2019_stckd[ , c('party','stack')]
y <- EES2019_stckd[ , c(132:length(EES2019_stckd))]

newcols <- 
  cbind(x,y) %>% 
  rename(Q9_gen      = Q9_rec_gen, 
         Q25_gen     = Q25_rec_gen,
         D4_1_rec    = D4_age,
         D6_std_rec  = D6_std,
         D6_une_rec  = D6_une) 



EES2019_stckd <- 
  cbind(oldcols, newcols) %>% 
  as_tibble %>% 
  mutate(D1_rec      = as.numeric(D1_rec)-1,
         D1_rec      = case_when(D1==as.numeric(98) ~ 98,
                                 D1==as.numeric(99) ~ 99,
                                 T                  ~ D1_rec),
         D3_rec      = case_when(is.na(D3_rec)~3,  T~D3_rec),
         D5_rec      = case_when(D5==as.numeric(98) ~ 98,
                                 D5==as.numeric(99) ~ 99,
                                 T                  ~ D5_rec),
         D6_rec      = case_when(D6==as.numeric(99) ~ 99, T ~ D6_rec),
         D6_std_rec  = case_when(D6==as.numeric(99) ~ 99, T ~ D6_std_rec),
         D6_une_rec  = case_when(D6==as.numeric(99) ~ 99, T ~ D6_une_rec),
         D7_rec      = as.numeric(D7_rec)-1,
         D7_rec      = case_when(D7 %in% c(as.numeric(97), as.numeric(6)) ~ 97,
                                 D7==as.numeric(98) ~ 98,
                                 D7==as.numeric(99) ~ 99,
                                 T                  ~ D7_rec),
         D9_rec      = case_when(is.na(D9_rec)~99, T~D9_rec),
         D10_rec     = case_when(is.na(D10_rec) & D10==as.numeric(96) ~ 99,
                                 D10==as.numeric(98)                  ~ 98,
                                 D10==as.numeric(99)                  ~ 99,
                                 T                                    ~ D10_rec),
         EDU_rec     = case_when(EDU==as.numeric(99) ~ 99, T ~ EDU_rec),
         Q25_rec     = case_when(is.na(Q25_rec) ~ as.integer(90), T ~ Q25_rec),
         socdem_synt_ptv = case_when(socdem_synt_ptv<0      ~ 0,
                                     is.na(socdem_synt_ptv) ~ 99,
                                     T                      ~ socdem_synt_ptv)
         )

rm(x, y, oldcols, newcols)

EES2019_stckd %<>% 
  relocate('Q26_rec', .after = 'Q25_rec') %>% 
  relocate('Q26_gen', .after = 'Q25_gen') 


# Value labels # =======================================================================================

source(here('Scripts', 'EES2019_stack_lbls.R'))


# Save the dataset # ===================================================================================

write_sav(EES2019_stckd, here('Output', 'EES2019_stckd.sav'))
write_dta(EES2019_stckd, here('Output', 'EES2019_stckd.dta'))
fwrite(EES2019_stckd, here('Output', 'EES2019_stckd.csv'))