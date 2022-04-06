# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Germany sample)
# Author: W. Haeussling & J.Leiser
# last update: 2022-03-15
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the German codebook and EP results # =========================================================


EES2019_cdbk_de <-
  EES2019_cdbk %>%
  filter(countryshort=='DE')

EP2019_de <-
  EP2019 %>%
  filter(countryshort=='DE')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_de %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_de %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_de %<>%
  filter(partyid!='DE90') %>% 
  mutate(Q7 = case_when(partyid=='DE01' ~ as.integer(801),
                        partyid=='DE02' ~ as.integer(802),
                        partyid=='DE03' ~ as.integer(803), 
                        partyid=='DE04' ~ as.integer(804),
                        partyid=='DE05' ~ as.integer(807),
                        partyid=='DE06' ~ as.integer(805),
                        partyid=='DE08' ~ as.integer(806),
                        T~NA_integer_))

EES2019_de_enhcdbk <- 
  left_join(EES2019_cdbk_de,
            EP2019_de %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7')

#The parties 'FREIE WÄHLER', 'Tierschutzpartei', 'FAMILIE', 'ÖDP', 'Die Partei' 
#and 'VOLT' (by partyname in EP2019_de) all gained a seat in the election.
#Because they don't have a party specific code, they are not going to appear in 
#EES2019_de_enhcdbk.


# Check the new dataset 

#EES2019_de_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)


# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_de_enhcdbk %>%
  dplyr::select(partyname, partyname_eng, Q7) %>%
  print(., n=nrow(.))

EES2019_de_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==801 ~ 41501,  # Christian Democratic Union / Christian Social Unio
      Q7==802 ~ 41320,  # Sozialdemokratische Partei Deutschlands (SPD) 
      Q7==805 ~ 41420,  # Free Democratic Party
      Q7==803 ~ 41113,  # Alliance 90 / The Greens  
      Q7==804 ~ 41221,  # The Left 
      Q7==807 ~ 41950,  # Alternative for Germany
      Q7==806 ~ 41951,  # Pirates 
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_de_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='_de$')) 