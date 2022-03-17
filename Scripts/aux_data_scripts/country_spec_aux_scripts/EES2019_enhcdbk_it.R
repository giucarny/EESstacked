# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Italian sample)
# Author: G.Carteny, M.Koernig
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Italian codebook and EP results # =========================================================


EES2019_cdbk_it <-
  EES2019_cdbk %>%
  filter(countryshort=='IT')

EP2019_it <-
  EP2019 %>%
  filter(countryshort=='IT')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_it %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_it %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_it %<>%
  filter(partyid!='IT90') %>% 
  mutate(Q7 = case_when(partyid=='IT01' ~ as.integer(1503),
                        partyid=='IT02' ~ as.integer(1501),
                        partyid=='IT03' ~ as.integer(1502), 
                        partyid=='IT04' ~ as.integer(1507),
                        partyid=='IT05' ~ as.integer(1504),
                        partyid=='IT06' ~ as.integer(1506),
                        partyid=='IT07' ~ as.integer(1505),
                        partyid=='IT08' ~ as.integer(1509),
                        partyid=='IT09' ~ as.integer(1508),
                        T~NA_integer_))


EES2019_it_enhcdbk <- 
  left_join(EES2019_cdbk_it,
            EP2019_it %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_it_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_it_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1501 ~ 32331,
      Q7==1502 ~ 32630,
      Q7==1503 ~ 32720,
      Q7==1504 ~ 32010,
      Q7==1505 ~ 32201,
      Q7==1506 ~ 32401,
      Q7==1507 ~ 32701,
      Q7==1509 ~ 32912,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_it_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_it$')) 