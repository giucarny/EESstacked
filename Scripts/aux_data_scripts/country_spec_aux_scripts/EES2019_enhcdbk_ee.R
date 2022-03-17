# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Estonia sample)
# Author: W. Haeussling, M.Koernig
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the estonian codebook and EP results # =========================================================


EES2019_cdbk_ee <-
  EES2019_cdbk %>%
  filter(countryshort=='EE')

EP2019_ee <-
  EP2019 %>%
  filter(countryshort=='EE')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_ee %>%
#   dplyr::select(partyname, partyname_eng, Q7)
 
# EP2019_ee %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_ee %<>%
  filter(partyid!='EE90') %>% 
  mutate(Q7 = case_when(partyid=='EE01' ~ as.integer(901),
                        partyid=='EE02' ~ as.integer(902),
                        partyid=='EE03' ~ as.integer(904), 
                        partyid=='EE04' ~ as.integer(905),
                        partyid=='EE05' ~ as.integer(907),
                        partyid=='EE06' ~ as.integer(903),
                        partyid=='EE07' ~ as.integer(906),
                        partyid=='EE08' ~ as.integer(908),
                        T~NA_integer_))

EES2019_ee_enhcdbk <- 
  left_join(EES2019_cdbk_ee,
            EP2019_ee %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7') 


# Check the new dataset 

# EES2019_ee_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_ee_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==901 ~ 83430,
      Q7==902 ~ 83411,
      Q7==903 ~ 83630,
      Q7==904 ~ 83601,  #here 83720 could also be fitting, however 83601 is the succesor-party-name from 2018 onwards
      Q7==905 ~ 83410,
      Q7==906 ~ 83401,
      Q7==907 ~ 83102,
      Q7==908 ~ 83101,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

#Problem: There are two possible Euromanifesto candidates for Q7=904. Telling from the same estonian name 83601 is most fitting. However the translated english name suggests 83720 as most fitting

# Check the new dataset 

# EES2019_ee_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_ee$')) 