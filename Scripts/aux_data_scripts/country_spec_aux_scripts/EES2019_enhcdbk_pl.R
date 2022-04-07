# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Poland sample)
# Author: J.Leiser, M.Koernig
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Poland codebook and EP results # =========================================================

EES2019_cdbk_pl <-
  EES2019_cdbk %>%
  filter(countryshort=='PL')

EP2019_pl <-
  EP2019 %>%
  filter(countryshort=='PL')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_pl %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_pl %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_pl %<>%
  filter(partyid!='PL90') %>% 
  mutate(Q7 = case_when(partyid=='PL01' ~ as.integer(2104), #PiS
                        partyid=='PL02' ~ as.integer(2101), #Konfederacja
                        partyid=='PL03' ~ as.integer(2106), #Kukiz '15
                        partyid=='PL04' ~ as.integer(2102), #Wiosna
                        partyid=='PL05' ~ as.integer(2103), #KE
                        partyid=='PL08' ~ as.integer(2107), #Polska Fair Play
                        partyid=='PL09' ~ as.integer(2105), #Lewica Razem
                        T~NA_integer_))

EES2019_pl_enhcdbk <- 
  left_join(EES2019_cdbk_pl,
            EP2019_pl %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 
# 
# EES2019_pl_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# only KE, not its constituent parties have votesh and seats

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_pl_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2104 ~ 92436,
      Q7==2102 ~ 92302,
      Q7==2103 ~ 92951,
      Q7==2101 ~ 92701,
      Q7==2105 ~ 92301,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_pl_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_pl$')) 
