# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: European Parliament 2019 election results 
# Author: G.Carteny
# last update: 2021-08-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get the EP election results files path # ============================================================
votes <- 
  list.files(path = here('Data','EP2019', 'votes'), pattern = '^votes.*csv$') %>% 
  paste0(here('Data', 'EP2019', 'votes'), '/', .)

seats <- 
  list.files(path = here('Data','EP2019', 'seats'), pattern = '^seats.*csv$') %>% 
  paste0(here('Data', 'EP2019', 'seats'), '/', .)


# Function for importing and reshaping original data # =================================================

getvotes.fun <- function(res) {
  
  div <- 
    res %>% 
    gsub('.csv', '',.) %>% stringi::stri_extract(regex = '[^-]+$') %>% 
    toupper(.)
  
  if(div=='ZLT') {div = 'LT'} # ad hoc solution for lithuanian results file
  
  x <- fread(res, sep=';', encoding = 'UTF-8')
  y <- fread(here('Data', 'EP2019', 'parties.csv'), sep=';', encoding = 'UTF-8')
  
  names(y)[names(y)=='ID'] <- 'PARTY_ID'

  z <-
    left_join(x,y, by=c('PARTY_ID', 'TYPE')) %>%
    dplyr::filter(DIVISION_ID==div) %>% as_tibble %>%
    dplyr::select('DIVISION_ID', 'PARTY_ID', 'ACRONYM', 'LABEL', 'VOTES_PERCENT')

  z %<>%
    mutate(countryshort = DIVISION_ID,
           votesh = VOTES_PERCENT/100,
           partyid = PARTY_ID,
           partyname_eng = LABEL,
           partyname = ACRONYM) %>%
    dplyr::select(countryshort, partyid, partyname, partyname_eng, votesh)

  return(z)
}

getseats.fun <- function(res) {
  
  div <- 
    res %>% 
    gsub('.csv', '',.) %>% stringi::stri_extract(regex = '[^-]+$') %>% 
    toupper(.)
  
  x <- fread(res, sep=';', encoding = 'UTF-8')
  y <- fread(here('Data', 'EP2019', 'parties.csv'), sep=';', encoding = 'UTF-8')
  
  names(y)[names(y)=='ID'] <- 'PARTY_ID'
  
  z <-
    left_join(x,y, by=c('PARTY_ID', 'TYPE')) %>%
    dplyr::filter(DIVISION_ID==div) %>% as_tibble %>%
    dplyr::select('DIVISION_ID', 'PARTY_ID', 'ACRONYM', 'LABEL', 'SEATS_TOTAL')
  
  z %<>%
    mutate(countryshort = DIVISION_ID,
           seats = SEATS_TOTAL,
           partyid = PARTY_ID,
           partyname_eng = LABEL,
           partyname = ACRONYM) %>%
    dplyr::select(countryshort, partyid, partyname, partyname_eng, seats)
  
  return(z)
  
}


# Create a data frame w/ the 2019 EP election results # ================================================

EP2019_votes <- lapply(votes, getvotes.fun) %>% do.call('rbind',.)

EP2019_seats <- lapply(seats, getseats.fun) %>% do.call('rbind',.)

# Party 'Coalition Unidas Podemos Cambiar Europa' and 'Coalition Ahora Repúblicas' appear more than once
# in the data due to different seat quantities, with the latter determined by split of the elected 
# members of the parliament when joining the European Parliament political groups. Since this info is
# not relevant, we aggregate the duplicated rows, in terms of seats. Votes data do not suffer such 
# issue

# Correct the Spanish case # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EP2019_seats %<>% 
  mutate(seats = case_when(partyid=='ES03' ~ as.integer(6),
                           partyid=='ES08' ~ as.integer(3), 
                           T ~ seats)) %>% 
  distinct()


# Join votes and seats # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EP2019 <- 
  left_join(EP2019_votes, EP2019_seats, 
            by=c("countryshort", "partyid", "partyname", "partyname_eng")) %>% 
  mutate(seats = case_when(is.na(seats)~as.integer(0), T~seats))



rm(list=ls(pattern='fun|seats|votes'))