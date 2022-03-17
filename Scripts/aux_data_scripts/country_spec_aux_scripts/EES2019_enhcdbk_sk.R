# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Slovakia sample)
# Author: J.Leiser
# last update: 2022-03-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Select the Slovakia codebook and EP results # =========================================================

EES2019_cdbk_sk <-
  EES2019_cdbk %>%
  filter(countryshort=='SK') %>% 
  mutate(Q9 = case_when(Q7==as.integer(2502) ~ as.integer(2510), T ~ Q9))

EP2019_sk <-
  EP2019 %>%
  filter(countryshort=='SK') %>% 
  distinct() # Drop duplicated rows (partyid==SK10)

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

# EES2019_sk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)
# now 11 unique parties as expected

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_sk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_sk_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2510 ~ 96521,  # Kres?anskodemokraticke hnutie  (KDH) 
      Q7==2501 ~ NA_real_,  # ?udova strana- Nase Slovensko (?SNS)
      Q7==2509 ~ 96601,  # Sme rodina
      Q7==2503 ~ 96423,  # Smer-socialna demokracia  (Smer-SD)
      Q7==2505 ~ 96610,  # Sloboda a Solidarita (SaS)   
      Q7==2506 ~ 96630,  # Oby?ajni ?udia a nezavisle osobnosti  (O?aNO)  
      Q7==2508 ~ 96402,  # Progresivne Slovensko a SPOLU 
      Q7==2504 ~ NA_real_,  # Slovenska narodna strana (SNS)  
      Q7==2507 ~ 96410,  # Most-Hid 
      Q7==2502 ~ 96955,  # Strana mad'arskej koalicie - Magyar Koalicio Partja (SMK-MKP) 
      Q7==2511 ~ NA_real_,  # Komunisticka strana Slovenska (KSS)
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_sk_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_sk$')) 
