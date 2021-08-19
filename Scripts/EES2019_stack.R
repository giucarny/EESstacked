# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 Stacking Script 
# Authors: G.Carteny
# last update: 2021-08-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "miceadds", "stringr")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)
rm(list = ls())


# Load & Mutate EES data # =============================================================================

# Load the full version of the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <- haven::read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

# Create a 'countryname' variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <-
  EES2019 %>% 
  left_join(.,
            data.frame(countrycode = EES2019$countrycode %>% val_labels() %>% as.numeric,
                       countryname = EES2019$countrycode %>% val_labels() %>% attr(.,'names')),
            by='countrycode')

# Create a 'countryshort' variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 %<>% 
  mutate(countryshort = region_NUTS1 %>% str_extract(pattern = '^.{0,2}'))


# Load/Source & Mutate auxiliary data sets # ===========================================================

# Source the 2019 European Parliament election results # - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EP2019_res.R'))

# Source the EES 2019 voter study codebook # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk.R'))


# Stack observations # =================================================================================

source.all(here('Scripts', 'country_spec_scripts'))

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)

rm(list=ls(pattern='_stack'))


# Stack original EES2019 # =============================================================================

EES2019_stckd %<>% left_join(., EES2019, by=c('countrycode', 'respid'))
