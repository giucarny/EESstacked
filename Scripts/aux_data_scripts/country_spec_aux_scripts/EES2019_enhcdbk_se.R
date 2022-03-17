# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Sweden sample)
# Author: J.Leiser, M.Koernig
# last update: 2022-03-17
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Sweden codebook and EP results # =========================================================

EES2019_cdbk_se <-
  EES2019_cdbk %>%
  filter(countryshort=='SE')

EP2019_se <-
  EP2019 %>%
  filter(countryshort=='SE')

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

EES2019_cdbk_se %>%
  dplyr::select(partyname, partyname_eng, Q7)

EP2019_se %>%
  dplyr::select(partyname, partyname_eng, partyid)

EP2019_se %<>%
  filter(partyid!='SE90') %>% 
  mutate(Q7 = case_when(partyid=='SE01' ~ as.integer(2701), #V (VP)
                        partyid=='SE02' ~ as.integer(2702), #S
                        partyid=='SE03' ~ as.integer(2703), #C
                        partyid=='SE04' ~ as.integer(2704), #L
                        partyid=='SE05' ~ as.integer(2705), #M
                        partyid=='SE06' ~ as.integer(2706), #KD
                        partyid=='SE07' ~ as.integer(2707), #MP
                        partyid=='SE08' ~ as.integer(2708), #SD
                        partyid=='SE09' ~ as.integer(2709), #FI
                        T~NA_integer_))

EES2019_se_enhcdbk <- 
  left_join(EES2019_cdbk_se,
            EP2019_se %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_se_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_se_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2702 ~ 11320,
      Q7==2705 ~ 11620,
      Q7==2707 ~ 11110,
      Q7==2704 ~ 11420,
      Q7==2703 ~ 11810,
      Q7==2708 ~ 11710,
      Q7==2706 ~ 11520,
      Q7==2701 ~ 11220,
      Q7==2709 ~ 11951,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_se_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_se$')) 
