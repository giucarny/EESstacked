# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Bulgarian sample)
# Author: G.Carteny & J.Leiser
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Bulgarian codebook and EP results # =========================================================

EES2019_cdbk_bg <-
  EES2019_cdbk %>%
  filter(countryshort=='BG') %>% 
  mutate(Q7 = case_when(Q2==as.integer(306) ~ as.integer(306), T~Q7),
         Q9 = case_when(Q7==as.integer(306) ~ as.integer(306), T~Q9))

EP2019_bg <-
  EP2019 %>%
  filter(countryshort=='BG') %>% 
  mutate(partyname = partyname %>% gsub('[^/]+$', '',.) %>% gsub('/','',.),
         partyname_eng = partyname_eng %>% gsub('[^/]+$', '',.) %>% gsub('/','',.)) %>% 
  mutate(partyname_eng = case_when(partyid=='BG04' ~ 'Alternativa za balgarsko vazrazhd',
                                   T ~ partyname_eng))



# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_bg %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>% 
#   as_tibble()
# 
# EP2019_bg %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>% 
#   as_tibble()

EP2019_bg %<>%
  filter(partyid!='BG90') %>% 
  mutate(Q7 = case_when(partyid=='BG01' ~ as.integer(302),
                        partyid=='BG02' ~ as.integer(303),
                        partyid=='BG03' ~ as.integer(301), 
                        partyid=='BG04' ~ as.integer(309),
                        partyid=='BG05' ~ as.integer(306),
                        partyid=='BG06' ~ as.integer(305),
                        partyid=='BG07' ~ as.integer(307),
                        partyid=='BG08' ~ as.integer(304),
                        partyid=='BG09' ~ as.integer(308),
                        T~NA_integer_))


EES2019_bg_enhcdbk <- 
  left_join(EES2019_cdbk_bg,
            EP2019_bg %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_bg_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_bg_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_bg_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==301 ~ 80620,  # Citzizens for European Development of Bulgaria (GERB) 
      Q7==302 ~ NA_real_,  # Coalition for Bulgaria (KB)
      Q7==303 ~ 80420,  # Movements for Rights and Freedoms (DPS)
      Q7==304 ~ NA_real_,  # IMRO – Bulgarian National Movement
      Q7==305 ~ 80602,  # Democratic Bulgaria  
      Q7==306 ~ NA_real_,  # Will
      Q7==307 ~ 80710,  # National Union Attack (ATAKA/ATA)
      Q7==308 ~ NA_real_,  # National Front for the Salvation of Bulgaria
      Q7==309 ~ NA_real_,  # Alternative for Bulgarian Revival 
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_bg_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_bg$')) 