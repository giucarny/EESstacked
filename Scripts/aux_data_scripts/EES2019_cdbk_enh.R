# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook
# Author: G.Carteny
# last update: 2021-08-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load/Source & Mutate auxiliary data sets # ===========================================================

# Source the 2019 European Parliament election results # - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EP2019_res.R'))

# Source the EES 2019 voter study codebook # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk.R'))


# Source country-specific script # =====================================================================

#despite invisible() this command produces a tibble, which shows in the R Markdown codebooks. 
#Therefore we need to employ "results='hide'" in the Markdown scripts
invisible(
  lapply(
    paste0(here('Scripts', 'aux_data_scripts', 'country_spec_aux_scripts'), '/',
           list.files(here('Scripts', 'aux_data_scripts', 'country_spec_aux_scripts'))),
    source)
)


EES2019_cdbk <- mget(ls(pattern = '_enhcdbk')) %>% do.call('rbind',.)


rm(list=ls(pattern='_enhcdbk|EP2019'))
