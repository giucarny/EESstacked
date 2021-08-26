# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Slovakia sample)
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Slovakia codebook and EP results # =========================================================

EES2019_cdbk_sk <-
  EES2019_cdbk %>%
  filter(countryshort=='SK')

EP2019_sk <-
  EP2019 %>%
  filter(countryshort=='SK')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_sk %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_sk %>%
#   dplyr::select(partyname, partyname_eng, partyid)

# Drop the #SIE? party, as otherwise left_joining will create spurious results

EES2019_cdbk_sk <- EES2019_cdbk_sk %>% filter(partyname != "#SIE?")


EP2019_sk %<>%
  filter(partyid!='SK90') %>% 
  mutate(Q7 = case_when(partyid=='SK01' ~ as.integer(2510), #KDH
                        partyid=='SK02' ~ as.integer(2505), #SaS
                        partyid=='SK03' ~ as.integer(2504), #SNS
                        partyid=='SK04' ~ as.integer(2503), #SMER-SD
                        partyid=='SK05' ~ as.integer(2502), #SMK-MKP
                        partyid=='SK06' ~ as.integer(2506), #OL'aNO
                        partyid=='SK07' ~ as.integer(2507), #MOST-HID
                        partyid=='SK08' ~ as.integer(2501), #LSNS
                        partyid=='SK09' ~ as.integer(2509), #Sme Rodina
                        partyid=='SK10' ~ as.integer(2508), #SP-SPOLU
                        T~NA_integer_))

EES2019_sk_enhcdbk <- 
  left_join(EES2019_cdbk_sk,
            EP2019_sk %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_sk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Drop one of the duplicated SPOLU + PS Rows
EES2019_sk_enhcdbk <- EES2019_sk_enhcdbk %>% distinct()

# EES2019_sk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)
# now 11 unique parties as expected

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_sk$')) 
