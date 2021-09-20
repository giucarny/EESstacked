# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Stacking Observations (EES 2019 Voter Study, Belgian Sample) 
# Author: G.Carteny
# last update: 2021-09-16
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# N.B: The Belgian sample is splitted according to the two electoral colleges of Belgium, namely the 
# Dutch and the French electoral college. 

# Belgian electoral colleges # - - -

el_coll_be <- list('DU-el', 'FR-el')


# Keep the EES 2019 Belgian sample # ===================================================================

EES2019_be <- 
  EES2019 %>% 
  filter(countrycode==1056) %>% 
  mutate(el_coll_be = case_when(meta_lang_be  == 1 ~ 'DU-el',
                                meta_lang_be  == 2 ~ 'FR-el')) %>%  
  split(.$el_coll_be) 


# Filter the codebook and EP elections data # ==========================================================

EES2019_cdbk_be <- 
  EES2019_cdbk %>% 
  filter(countryshort=='BE') %>%
  mutate(el_coll_be = case_when(Region == 'Flanders' ~ 'DU-el',
                                Region == 'Wallonia' ~ 'FR-el')) %>% 
  split(.$el_coll_be)


# Get the respondent ID codes # ========================================================================

respid <- lapply(el_coll_be, 
                 function(x) {
                   EES2019_be[[x]] %>%  .$respid %>% as.numeric
                 })

# Choose the relevant parties # ========================================================================


# Check the parties about which we have the PTV variable # - - - - - - - - - - - - - - - - - - - - - - -

ptv_crit <- lapply(el_coll_be, 
                   function(x) {
                     EES2019_cdbk_be[[x]] %>%  dplyr::select(partyname, Q10_PTV)
                   })

#ptv_crit: 11 parties (7 in DU-el, 7 in FR-el)

# Check the seats obtained by each party - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

seats_crit <- lapply(el_coll_be, 
                     function(x) {
                       EES2019_cdbk_be[[x]] %>%  
                         dplyr::select(partyname, seats) %>% 
                         mutate(seats = case_when(seats==0 ~ NA_integer_, T~seats))
                     })

#seats_crit: 11 parties (6 in DU-el, 5 in FR-el)

# Select the relevant parties # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

party <- lapply(el_coll_be, 
                function(x) {
                  EES2019_cdbk_be[[x]] %>%  
                    dplyr::select(partyname, Q10_PTV, Q7) %>% 
                    na.omit() %>% 
                    .$Q7
                })


# Create the Belgian EES 2019 SDM # ====================================================================

names(respid) <- el_coll_be
names(party) <- el_coll_be

EES2019_be_stack <- lapply(el_coll_be,
                           function(x) {
                             expand_grid(respid[[x]], party[[x]]) %>% 
                               mutate(respid = `respid[[x]]`, 
                                      party = `party[[x]]`) %>% 
                               mutate(countrycode = EES2019_be[[x]]$countrycode %>% unique,
                                      stack = paste0(respid, '-', party)) %>% 
                               dplyr::select(countrycode, respid, party, stack) %>% 
                               .[order(.$respid, .$party),]
                           })

EES2019_be_stack %<>% do.call('rbind',.)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_be$|_crit$'))  
rm(list = c('respid', 'party'))