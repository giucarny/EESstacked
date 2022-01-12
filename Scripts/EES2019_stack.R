# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking Script 
# Authors: G.Carteny
# last update: 2022-01-11
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr", "rlang", "car",
         "caret")
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


# Reshape the dataset before saving # ==================================================================

oldcols <- 
  EES2019_stckd[ , c(1:131)] %>% 
  dplyr::select(-c(party, stack)) # %>% distinct

x <- EES2019_stckd[ , c('party','stack')]
y <- EES2019_stckd[ , c(132:length(EES2019_stckd))]

newcols <- 
  cbind(x,y) %>% 
  rename(D4_1_rec   = D4_age,
         D6_std_rec = D6_std,
         D6_une_rec = D6_une)

EES2019_stckd <- 
  cbind(oldcols, newcols) %>% 
  as_tibble

rm(x, y, oldcols, newcols)


# Save the dataset # ===================================================================================

write_sav(EES2019_stckd, here('Output', 'EES2019_stckd.sav'))
fwrite(EES2019_stckd, here('Output', 'EES2019_stckd.csv'))