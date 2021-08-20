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

# source.all(here('Scripts', 'aux_data_scripts', 'country_spec_aux_scripts'))
