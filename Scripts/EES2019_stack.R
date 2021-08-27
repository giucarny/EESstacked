# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking Script 
# Authors: G.Carteny
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "miceadds", "stringr")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())
# rm(list= ls()[!(ls() %in% c('keepThis','andThis'))]) # useful for further implementations

# Criteria # ===========================================================================================

part <- T

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
  mutate(countryshort = case_when(countrycode==1250 ~ 'FR', T ~ countryshort))


# Source the auxiliary data set # ======================================================================

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk_enh.R'))


# Stack observations # =================================================================================

if (part) {
  
  source(here('Scripts', 'EES2019_stack_part.R'))
  EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
  rm(list=ls(pattern='_stack'))
  
} else {
  
  source.all(here('Scripts', 'country_spec_scripts'))
  EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
  rm(list=ls(pattern='_stack'))
}

rm(part)


# Stack original EES2019 # =============================================================================

EES2019_stckd %<>% left_join(., EES2019, by=c('countrycode', 'respid'))
