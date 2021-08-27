# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Lithuania sample)
# Author: J.Leiser
# last update: 2021-08-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Lituania codebook and EP results # =========================================================

EES2019_cdbk_lt <-
  EES2019_cdbk %>%
  filter(countryshort=='LT')

EP2019_lt <-
  EP2019 %>%
  filter(countryshort=='LT')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_lt %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_lt %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_lt %<>%
  filter(partyid!='LT90') %>% 
  mutate(Q7 = case_when(partyid=='LT01' ~ as.integer(1705), #DP
                        partyid=='LT02' ~ as.integer(1707), #LLRA-KSS
                        partyid=='LT03' ~ as.integer(1706), #LS/LRLS
                        partyid=='LT04' ~ as.integer(1703), #LSDP
                        partyid=='LT05' ~ as.integer(1702), #LVZS
                        partyid=='LT06' ~ as.integer(1704), #TT
                        partyid=='LT07' ~ as.integer(1701), #TS-LKD
                        partyid=='LT08' ~ as.integer(1708), #LCP
                        partyid=='LT09' ~ as.integer(1709), #LSDDP
                        partyid=='LT10' ~ as.integer(1711), #LZP
                        partyid=='LT11' ~ as.integer(1710), #LLS
                        partyid=='LT12' ~ as.integer(1713), #VKM-AMT
                        partyid=='LT13' ~ as.integer(1712), #VKM-PRPJ
                        partyid=='LT14' ~ as.integer(1714), #VKM-VRSV
                        T~NA_integer_))

EES2019_lt_enhcdbk <- 
  left_join(EES2019_cdbk_lt,
            EP2019_lt %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_lt_enhcdbk %>%
#  dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$')) 
