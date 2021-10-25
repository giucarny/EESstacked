# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Synthetic Variables Estimation (Partial general workflow)
# Author: G.Carteny
# last update: 2021-10-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# General workflow # ===================================================================================

# Load the full version of the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <- read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

# Mutate the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

source(here('Scripts', 'EES2019_datamut.R'))


# Source the auxiliary data set # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk_enh.R'))


# Harmonize Q25 and Q9 values # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q25_rec.R'))

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q9_rec.R'))


# Load auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'EES2019_stack_funs.R'))


# Stack observations # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

invisible(
  lapply(paste0(here('Scripts', 'country_spec_scripts'),'/', 
                here('Scripts', 'country_spec_scripts') %>% list.files(pattern = '_stack')),
         source)  
)

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
rm(list=ls(pattern='_stack'))


# Stack the original EES2019 variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_stckd %<>% left_join(., EES2019, by=c('countrycode', 'respid'))

