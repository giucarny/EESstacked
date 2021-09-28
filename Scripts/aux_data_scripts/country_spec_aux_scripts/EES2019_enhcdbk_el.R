# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Greek sample)
# Author: G.Carteny
# last update: 2021-09-28
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Greek codebook and EP results # ===========================================================

EES2019_cdbk_el <-
  EES2019_cdbk %>%
  filter(countryshort=='GR') %>% 
  mutate(across(c('Q10_PTV', 'Q13_left_right', 'Q24_EU'), 
                ~case_when(Q7 %in% c(1206, 1207) ~ NA_character_, T~.))) # *

# * Party scores for such variables are actually not available in the EES2019 data set

EP2019_el <-
  EP2019 %>%
  filter(countryshort=='EL') %>% 
  mutate(partyname = partyname %>% gsub('^(.*[/])','',.) %>% str_trim(),
         partyname_eng = partyname_eng %>% gsub('^(.*[/])','',.) %>% str_trim()) 

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_el %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_el %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.)) 

EP2019_el %<>%
  filter(partyid!='EL90') %>% 
  mutate(Q7 = case_when(partyid=='EL01' ~ as.integer(1202),
                        partyid=='EL02' ~ as.integer(1204),
                        partyid=='EL03' ~ as.integer(1205), 
                        partyid=='EL04' ~ as.integer(1201),
                        partyid=='EL05' ~ as.integer(1203),
                        partyid=='EL06' ~ as.integer(1207),
                        partyid=='EL07' ~ as.integer(1208),
                        partyid=='EL08' ~ as.integer(1209),
                        partyid=='EL09' ~ as.integer(1211),
                        partyid=='EL10' ~ as.integer(1206),
                        partyid=='EL11' ~ NA_integer_,
                        partyid=='EL12' ~ NA_integer_,
                        partyid=='EL13' ~ as.integer(1212),
                        partyid=='EL14' ~ as.integer(1210),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_el_enhcdbk <- 
  left_join(EES2019_cdbk_el,
            EP2019_el %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7') %>% 
  mutate(countryshort='EL')

# Check the new dataset 

# EES2019_el_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   print(., n=nrow(.))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_el$')) 