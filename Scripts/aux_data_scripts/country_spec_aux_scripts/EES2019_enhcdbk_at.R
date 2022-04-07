# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Austrian sample)
# Author: M.Koernig
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Austrian codebook and EP results # =========================================================


EES2019_cdbk_at <-
  EES2019_cdbk %>%
  filter(countryshort=='AT')

EP2019_at <-
  EP2019 %>%
  filter(countryshort=='AT')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_at %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_at %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_at %<>%
  filter(partyid!='AT90') %>% 
  mutate(Q7 = case_when(partyid=='AT01' ~ as.integer(102),
                        partyid=='AT02' ~ as.integer(101),
                        partyid=='AT03' ~ as.integer(103), 
                        partyid=='AT04' ~ as.integer(106),
                        partyid=='AT05' ~ as.integer(104),
                        partyid=='AT06' ~ as.integer(105),
                        T~NA_integer_))


EES2019_at_enhcdbk <- 
  left_join(EES2019_cdbk_at,
            EP2019_at %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_at_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_at_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==102 ~ 42320,
      Q7==101 ~ 42520,
      Q7==104 ~ 42422,
      Q7==106 ~ 42110,
      Q7==103 ~ 42420,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_at_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_at$')) 