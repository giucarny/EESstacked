# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Belgian sample)
# Author: G.Carteny & J.Leiser
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Belgian codebook and EP results # ===========================================================

EES2019_cdbk_be <-
  EES2019_cdbk %>%
  filter(countryshort=='BE') 

EP2019_be <-
  EP2019 %>%
  filter(countryshort=='BE') 


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_be %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_be %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.))

EP2019_be %<>%
  filter(partyid!='BE90') %>% 
  mutate(Q7 = case_when(partyid=='BE01' ~ as.integer(201),
                        partyid=='BE02' ~ as.integer(202),
                        partyid=='BE03' ~ as.integer(203), 
                        partyid=='BE04' ~ as.integer(204),
                        partyid=='BE05' ~ as.integer(205),
                        partyid=='BE06' ~ as.integer(206),
                        partyid=='BE07' ~ as.integer(207),
                        partyid=='BE08' ~ as.integer(208),
                        partyid=='BE09' ~ as.integer(209),
                        partyid=='BE10' ~ as.integer(210),
                        partyid=='BE11' ~ as.integer(211),
                        partyid=='BE12' ~ as.integer(213),
                        partyid=='BE13' ~ as.integer(214),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_be_enhcdbk <- 
  left_join(EES2019_cdbk_be,
            EP2019_be %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

# Check the new dataset 

# EES2019_be_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   print(., n=nrow(.))

# Create a common variable for merging the codebook w/ EMCS # ==========================================

# EES2019_be_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   print(., n=nrow(.))

EES2019_be_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==207 ~ 21201,  # Workers Party of Belgium 
      Q7==201 ~ 21521,  # Christian Democratic and Flemish Party
      Q7==204 ~ 21321,  # Socialist Party Different
      Q7==206 ~ 21421,  # Open Flemish Liberals and Democrats
      Q7==203 ~ 21913,  # New Flemish Alliance  
      Q7==202 ~ 21112,  # Green
      Q7==205 ~ 21914,  # Flemish Interest 
      Q7==208 ~ 21322,  # Francophone Socialist Party 
      Q7==209 ~ 21427,  # Reform Movement
      Q7==210 ~ 21522,  # Humanist Democratic Centre 
      Q7==211 ~ 21111,  # Ecologists
      Q7==212 ~ NA_real_,  # National Front (Belgium)
      Q7==213 ~ 21201,  # Workers Party of Belgium
      Q7==214 ~ NA_real_,  #  Francophone Democratic Federalists
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_be_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$')) 
