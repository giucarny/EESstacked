# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking Script 
# Authors: G.Carteny
# last update: 2021-09-09
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())
# rm(list= ls()[!(ls() %in% c('keepThis','andThis'))]) # useful for further implementations

# Load & Mutate EES data # =============================================================================

# Load the full version of the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <- read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

# Create a 'countryname' variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <-
  EES2019 %>% 
  left_join(.,
            data.frame(countrycode = EES2019$countrycode %>% val_labels() %>% as.numeric,
                       countryname = EES2019$countrycode %>% val_labels() %>% attr(.,'names')),
            by='countrycode')

# Create a 'countryshort' variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 %<>% 
  mutate(countryshort = region_NUTS1 %>% str_extract(pattern = '^.{0,2}')) %>% 
  mutate(countryshort = case_when(countrycode==1250 ~ 'FR',
                                  countrycode==1470 ~ 'MT',
                                  T ~ countryshort))

# Change missing values for Q2, Q7, Q9,... # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Q2_labs <- val_labels(EES2019$Q2)
Q7_labs <- val_labels(EES2019$Q7)
Q9_labs <- val_labels(EES2019$Q9)

EES2019 %<>% 
  mutate(across(c(Q2, Q7, Q9), ~as.integer(.)),
         Q2 = case_when(Q2==10 ~ as.integer(90), 
                        Q2==11 ~ as.integer(0),
                        Q2==9999 ~ as.integer(96), 
                        T ~ Q2),
         Q7 = case_when(Q7==as.integer(9999) ~ as.integer(0), 
                        T ~ Q7),
         Q9 = case_when(Q9==as.integer(97) ~ as.integer(0), 
                        T ~ Q9))

Q2_labs[Q2_labs %in% c(10,11,9999)] <- c(90,0,96)
Q7_labs                             <- Q7_labs[-length(Q7_labs)]
Q9_labs[Q9_labs %in% c(97)]         <- c(0)

val_labels(EES2019$Q2) <- Q2_labs
val_labels(EES2019$Q7) <- Q7_labs
val_labels(EES2019$Q9) <- Q9_labs

rm(list=ls(pattern='labs'))


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

invisible(
  lapply(paste0(here('Scripts', 'country_spec_scripts'),'/',
                here('Scripts', 'country_spec_scripts') %>% list.files(pattern = '_genvars')),
         source)
)

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
rm(list=ls(pattern='_stack'))
