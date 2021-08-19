# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: European Parliament 2019 election results 
# Author: G.Carteny
# last update: 2021-08-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get the EP election results files path # ============================================================
results <- 
  list.files(path = here('Data', 'EP2019'), pattern = '^results.*csv$') %>% 
  paste0(here('Data', 'EP2019'), '/', .)


# Function for importing and reshaping original data # =================================================
getres.fun <- function(res) {
  
  div <- 
    res %>% 
    gsub('.csv', '',.) %>% stringi::stri_extract(regex = '[^-]+$') %>% 
    toupper(.)
  
  if(div=='ZLT') {div = 'LT'} # ad hoc solution for lithuanian results file
  
  x <- fread(res, sep=';', encoding = 'UTF-8')
  y <- fread(here('Data', 'EP2019', 'parties.csv'), sep=';', encoding = 'UTF-8')
  
  names(y)[names(y)=='ID'] <- 'PARTY_ID'
  
  z <- 
    left_join(x,y, by='PARTY_ID', 'TYPE') %>% 
    dplyr::filter(DIVISION_ID==div) %>% as_tibble %>% 
    dplyr::select('DIVISION_ID', 'PARTY_ID', 'ACRONYM', 'LABEL', 'VOTES_PERCENT')
  
  z %<>%
    mutate(countryshort = DIVISION_ID, 
           votesh = VOTES_PERCENT/100, 
           partyname_eng = LABEL, 
           partyname = ACRONYM) %>%
    dplyr::select(countryshort, partyname, partyname_eng, votesh)
  
  return(z)
}

# Create a data frame w/ the 2019 EP election results # ================================================

EP2019 <- lapply(results, getres.fun) %>% do.call('rbind',.)

rm(results)
rm(list=ls(pattern='fun'))