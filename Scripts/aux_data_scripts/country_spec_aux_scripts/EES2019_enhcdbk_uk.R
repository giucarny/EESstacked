# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (United Kingdom sample)
# Author: W.Haeussling & J.Leiser
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the United Kingdom codebook and EP results # =========================================================


EES2019_cdbk_uk <-
  EES2019_cdbk %>%
  filter(countryshort=='UK')

EP2019_uk <-
  EP2019 %>%
  filter(countryshort=='UK')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_uk %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_uk %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_uk %<>%
  filter(partyid!='UK90', partyid!='UK91') %>% 
  mutate(Q7 = case_when(partyid=='UK01' ~ as.integer(2810),
                        partyid=='UK02' ~ as.integer(2811),
                        partyid=='UK03' ~ as.integer(2812), 
                        partyid=='UK04' ~ as.integer(2813),
                        partyid=='UK07' ~ as.integer(2801),
                        partyid=='UK08' ~ as.integer(2802),
                        partyid=='UK09' ~ as.integer(2806),
                        partyid=='UK10' ~ as.integer(2805),
                        partyid=='UK11' ~ as.integer(2809),
                        partyid=='UK12' ~ as.integer(2803),
                        partyid=='UK13' ~ as.integer(2804),
                        partyid=='UK14' ~ as.integer(2808),
                        partyid=='UK15' ~ as.integer(2807),
                        T~NA_integer_))


EES2019_uk_enhcdbk <- 
  left_join(EES2019_cdbk_uk,
            EP2019_uk %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

#From those parties dropped by the left_join function one party, namely the 
#'Alliance Party', received one seat, the other none.

# Check the new dataset 

# EES2019_uk_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)


# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_uk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_uk_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2801 ~ 51620,  # Conservative  (Con)  
      Q7==2802 ~ 51320,  # Labour Party (Lab)
      Q7==2803 ~ 51421,  # Liberal Democrats (LD)
      Q7==2804 ~ 51110,  # Green Party (GP)
      Q7==2805 ~ 51902,  # Scottish National Party (SNP) 
      Q7==2806 ~ 51951,  # United Kingdom Independence Party (UKIP)
      Q7==2807 ~ 51702,  # The Brexit Party  
      Q7==2808 ~ NA_real_,  # Change UK - The Independent Group 
      Q7==2809 ~ 51901,  # Plaid Cymru (Plaid) 
      Q7==2810 ~ 51953,  # Sinn Fein (SF) 
      Q7==2811 ~ 51903,  # Democratic Unionist Party (DUP)
      Q7==2812 ~ 51904,  # Ulster Unionist Party
      Q7==2813 ~ NA_real_,  # Social Democratic & Labour Party (SDLP)
      Q7==2814 ~ NA_real_,  # Independent
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_uk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_uk$')) 