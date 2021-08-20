# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Italian sample)
# Author: G.Carteny
# last update: 2021-08-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Italian codebook and EP results # =========================================================


EES2019_cdbk_it <-
  EES2019_cdbk %>%
  filter(countryshort=='IT')

EP2019_it <-
  EP2019 %>%
  filter(countryshort=='IT')


# Create a common variable for merging datasets # ======================================================


EES2019_cdbk_it %>%
  dplyr::select(partyname)

EP2019_it %>%
  dplyr::select(partyname)

EP2019_it %>%
  mutate(partyname = case_when(partyname=='LN' ~ 'Lega Salvini Premier',
                               partyname=='PD' ~ 'Partito Democratico (PD)',
                               partyname=='FI' ~ 'Forza Italia (Fi)', ))
