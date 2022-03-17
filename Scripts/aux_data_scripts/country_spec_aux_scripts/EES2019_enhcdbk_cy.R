# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Cypriot sample)
# Author: G.Carteny, M.Koernig
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Cypriot codebook and EP results # =========================================================

EES2019_cdbk_cy <-
  EES2019_cdbk %>%
  filter(countryshort=='CY') 

EP2019_cy <-
  EP2019 %>%
  filter(countryshort=='CY') %>% 
  mutate(partyname = partyname %>% gsub('^(.*[/])','',.) %>% str_trim(),
         partyname_eng = partyname_eng %>% gsub('^(.*[/])','',.) %>% str_trim()) 

# Apply a specific change for the Cypriot data # =======================================================

EES2019_cdbk_cy %<>% 
  mutate(Q7 = case_when(Q7n==as.integer(9) ~ NA_integer_,
                        Q2==as.integer(505) ~ as.integer(505),
                        T ~ Q7),
         Q13_left_right = case_when(Q7n==as.integer(9) ~ NA_character_, T ~ Q13_left_right),
         Q24_EU         = case_when(Q7n==as.integer(9) ~ NA_character_, T ~ Q24_EU))

# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_cy %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_cy %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.))

EP2019_cy %<>%
  filter(partyid!='CY90') %>% 
  mutate(Q7 = case_when(partyid=='CY01' ~ as.integer(502),
                        partyid=='CY02' ~ as.integer(501),
                        partyid=='CY03' ~ as.integer(503), 
                        partyid=='CY04' ~ as.integer(504),
                        partyid=='CY05' ~ as.integer(507),
                        partyid=='CY06' ~ as.integer(505),
                        partyid=='CY07' ~ as.integer(506),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_cy_enhcdbk <- 
  left_join(EES2019_cdbk_cy,
            EP2019_cy %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_cy_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   print(., n=nrow(.))

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_cy_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==501 ~ 36220,
      Q7==502 ~ 36510,
      Q7==503 ~ 36420,
      Q7==504 ~ 36322,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_cy_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_cy$')) 