# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Denmark sample)
# Author: W. Haeussling
# last update: 2021-08-27
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the danish codebook and EP results # =========================================================


EES2019_cdbk_dk <-
  EES2019_cdbk %>%
  filter(countryshort=='DK')

EP2019_dk <-
  EP2019 %>%
  filter(countryshort=='DK')


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_dk %>%
#   dplyr::select(partyname, partyname_eng, Q7)
# 
# EP2019_dk %>%
#   dplyr::select(partyname, partyname_eng, partyid)

EP2019_dk %<>%
  filter(partyid!='DK90') %>% 
  mutate(Q7 = case_when(partyid=='DK01' ~ as.integer(701),
                        partyid=='DK02' ~ as.integer(704),
                        partyid=='DK03' ~ as.integer(707), 
                        partyid=='DK04' ~ as.integer(705),
                        partyid=='DK05' ~ as.integer(708),
                        partyid=='DK06' ~ as.integer(709),
                        partyid=='DK07' ~ as.integer(703),
                        partyid=='DK08' ~ as.integer(702),
                        partyid=='DK09' ~ as.integer(706),
                        partyid=='DK10' ~ as.integer(000),
                        T~NA_integer_))

#EES_cdbk_dk includes the party 'Kristendemokraterne' which isn't 
#included in EP2019_dk. This party has the Q7-value NA.
#Vice versa EP2019_dk includes the Party 'Alternativet' which isn't included in
#EES_cdbk_dk.
#'Alternativet'-Q7_data is changed to 000 in the EP_2019_dk column 
#Q7 so that in the left join no data is mixed. 

#I will change EES2019_dk_enhcdbk manually for 'Alternativet' so that the 
#votesh and seats data are included for 'Alternativet'.

EES2019_dk_enhcdbk <- 
  left_join(EES2019_cdbk_dk,
            EP2019_dk %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7') %>%
  add_row(countryshort=as.character('DK'), countrycode=1208, partyname='Alternativet', 
          votesh=0.0337, seats=0 )




# Check the new dataset 

#EES2019_dk_enhcdbk %>% 
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats)

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_dk$')) 