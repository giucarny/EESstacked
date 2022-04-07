# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Luxembourg sample)
# Author: W. Haeussling, M.Koernig
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Luxembourgian codebook and EP results # =========================================================


EES2019_cdbk_lu <-
  EES2019_cdbk %>%
  filter(countryshort=='LU')

EP2019_lu <-
  EP2019 %>%
  filter(countryshort=='LU')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_lu %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_lu %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_lu %<>%
  filter(partyid!='LU90') %>% 
  mutate(Q7 = case_when(partyid=='LU01' ~ as.integer(1804),
                        partyid=='LU02' ~ as.integer(1802),
                        partyid=='LU03' ~ as.integer(1803), 
                        partyid=='LU04' ~ as.integer(1805),
                        partyid=='LU05' ~ as.integer(1801),
                        partyid=='LU06' ~ as.integer(1806),
                        partyid=='LU07' ~ as.integer(1807),
                        partyid=='LU09' ~ as.integer(1808),
                        partyid=='LU10' ~ as.integer(1809),
                        T~NA_integer_))

#LU08 is the party 'Kommunistesch Partei Letzebuerg'. This party is also in 
#EES2019_lu but has no partycode there as well as another party. This will 
#lead to a mix-up in the data through the function left_join.
#I will therefore additionally adapt some values manually .

EES2019_lu_enhcdbk <- 
  left_join(EES2019_cdbk_lu,
            EP2019_lu %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7') 
EES2019_lu_enhcdbk [11, 20] <- NA
EES2019_lu_enhcdbk [11, 21] <- NA




# Check the new dataset 

# EES2019_lu_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_lu_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1801 ~ 23520,
      Q7==1802 ~ 23320,
      Q7==1803 ~ 23420,
      Q7==1804 ~ 23113,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_lu_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lu$')) 