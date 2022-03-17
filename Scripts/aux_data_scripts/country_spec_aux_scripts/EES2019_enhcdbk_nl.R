# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 enhanced codebook (Dutch sample)
# Author: G.Carteny
# last update: 2021-08-30
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Select the Dutch codebook and EP results # ===========================================================

EES2019_cdbk_nl <-
  EES2019_cdbk %>%
  filter(countryshort=='NL') %>% 
  mutate(Q9 = case_when(Q7==as.integer(2011) ~ as.integer(2012), T ~ Q9))

EP2019_nl <-
  EP2019 %>%
  filter(countryshort=='NL') 


# Create a common variable for merging datasets # ======================================================

# Print the two country-specific auxiliary dataframes for coding purposes, 
# but mute them once the coding process is completed.

# EES2019_cdbk_nl %>%
#   dplyr::select(partyname, partyname_eng, Q7) %>%
#   as_tibble() %>% print(.,n=nrow(.))
# 
# EP2019_nl %>%
#   dplyr::select(partyname, partyname_eng, partyid) %>%
#   as_tibble() %>% print(.,n=nrow(.))

EP2019_nl %<>%
  filter(partyid!='NL90') %>% 
  mutate(Q7 = case_when(partyid=='NL01' ~ as.integer(2003),
                        partyid=='NL02' ~ as.integer(2007),
                        partyid=='NL03' ~ as.integer(2006), 
                        partyid=='NL04' ~ as.integer(2001),
                        partyid=='NL05' ~ as.integer(2005),
                        partyid=='NL06' ~ as.integer(2002),
                        partyid=='NL07' ~ as.integer(2004),
                        partyid=='NL08' ~ as.integer(2009),
                        partyid=='NL09' ~ as.integer(2008),
                        partyid=='NL10' ~ as.integer(2010),
                        partyid=='NL11' ~ as.integer(2011),
                        partyid=='NL12' ~ as.integer(2012),
                        partyid=='NL13' ~ as.integer(2013),
                        T~NA_integer_)) %>% 
  na.omit()


EES2019_nl_enhcdbk <- 
  left_join(EES2019_cdbk_nl,
            EP2019_nl %>% dplyr::select(Q7, votesh, seats),
            by = 'Q7') %>% 
  mutate(q25 = as.integer(q25))

# Check the new dataset 

# EES2019_nl_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, votesh, seats) %>%
#   print(., n=nrow(.))

# Create a common variable for merging the codebook w/ EMCS # ==========================================

EES2019_nl_enhcdbk %<>% 
  mutate(
    emcs = case_when(
      Q7==2001 ~ 22420,
      Q7==2002 ~ 22721,
      Q7==2003 ~ 22521,
      Q7==2004 ~ 22330,
      Q7==2005 ~ 22110,
      Q7==2006 ~ 22210,
      Q7==2007 ~ 22320,
      Q7==2008 ~ 22225, #Coalition of 2008 and Reformed political party in Euromanifesto
      Q7==2012 ~ 22701,
      Q7==2009 ~ 22120,
      Q7==2010 ~ 22952,
      T       ~ NA_real_
    ),
    emcs = as.integer(emcs)
  )

# Check the new dataset 

# EES2019_nl_enhcdbk %>%
#   dplyr::select(partyname, partyname_eng, Q7, emcs)


# Clean the environment # ==============================================================================

rm(list=ls(pattern='_nl$')) 