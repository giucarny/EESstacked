# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 codebook
# Author: G.Carteny
# last update: 2021-08-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load the EES Codebook # ==============================================================================

EES2019_cdbk <- 
  fread(here('Data', 'EES2019', 'ZA7581_codebook.csv')) %>% 
  as_tibble() 

# Substitute empty cells w/ NA by vector type # ========================================================

cdbk.fun <- 
  function(data, clmn) {
    if (class(data[[clmn]])=='integer') {
      natype <- NA_integer_
    } 
    else if (class(data[[clmn]])=='numeric') {
      natype <- NA_real_
    } 
    else if (class(data[[clmn]])=='character') {
      natype <- NA_character_
    }
    
    new_clmn <- 
      data %>% 
      mutate(across(all_of(clmn), ~case_when(.=='' ~ natype, T~.))) %>% 
      dplyr::select(all_of(clmn))
    
    
    return(new_clmn)
  }


EES2019_cdbk <- 
  lapply(X=names(EES2019_cdbk), data=EES2019_cdbk, FUN=cdbk.fun) %>% 
  do.call('cbind',.) %>% 
  as_tibble()


# Mutate additional vrbl names and select vrbls ========================================================

EES2019_cdbk %<>% 
  mutate(countryshort = Coutnry_short,
         partyname = Party_name_questionnaire,
         partyname_eng = `English name`,
         region = Region) %>% 
  dplyr::select(countryshort, countrycode, Region, partyname, partyname_eng, 
                Q10_PTV, Q13_left_right, Q24_EU, Q7_EES, Q7, Q7n, Q2, Q2_EES, Q2n,
                Q9_Q25_EES, Q9, Q9n, q25, Q25n)


EES2019_cdbk$countrycode[EES2019_cdbk$countrycode==1119] <- 1191

rm(list=ls(pattern='fun'))