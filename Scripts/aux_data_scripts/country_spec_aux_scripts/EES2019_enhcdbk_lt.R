# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Lithuania sample)
# Author: J.Leiser, M.Koernig
# last update: 2022-03-16
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

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_lt_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==1701 ~ 88621,
      Q7==1703 ~ 88320,
      Q7==1706 ~ 88423,
      Q7==1705 ~ 88322,
      Q7==1704 ~ 88522,
      Q7==1702 ~ 88524,
      Q7==1713 ~ 88001,
      Q7==1710 ~ 88433, #in Euromanifesto it is called "Liberalų ir centro sąjunga" which later fused in 2014 with TAIP to Lietuvos laisvės sąjunga (liberalai), which is the name used in ESS2019
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )
#possible Problem: "Valdemar Tomasevski´s Block-Coalition of Christion Families and Russion Alliance cant be found in EES2019 data

# Check the new dataset 

# EES2019_lt_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)
 

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_lt$')) 
