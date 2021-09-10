EES2019_be_lst <- 
  EES2019_be %>% 
  mutate(Q7 = as.numeric(Q7),
         el_coll = case_when(Q7 %in% 201:207 | Q2 %in% 201:207 | Q25_rec %in% 201:207 |
                               region_NUTS1=='BE2' ~ 'Flanders',
                             Q7 %in% 208:214 | Q2 %in% 208:214 | Q25_rec %in% 208:214 |
                               region_NUTS1=='BE3' ~ 'Wallonia',
                             T ~ 'Undefined')) %>% 
  split(.$el_coll)

EES2019_be_stack %<>% 
  mutate(Q7 = as.numeric(Q7),
         el_coll = case_when(Q7 %in% 201:207 | Q2 %in% 201:207 | Q25_rec %in% 201:207 |
                               region_NUTS1=='BE2' ~ 'Flanders',
                             Q7 %in% 208:214 | Q2 %in% 208:214 | Q25_rec %in% 208:214 |
                               region_NUTS1=='BE3' ~ 'Wallonia',
                             T ~ 'Undefined'))


EES2019_cdbk_be_lst <- 
  EES2019_cdbk_be %>%
  split(.$Region)


# Wallonia # 

EES2019_be_stack_wl <- 
  lapply(data = EES2019_be_lst$Wallonia,
         cdbk = EES2019_cdbk_be_lst$Wallonia,
         crit = 'average',
         rescale = T,
         check = F,
         keep_id = T,
         X = list('Q10','Q11','Q23'),
         FUN = gendis.fun) 

EES2019_be_stack_wl <- 
  left_join(EES2019_be_stack_wl[[1]], 
            EES2019_be_stack_wl[[2]], 
            by=c('respid', 'party')) %>% 
  left_join(., 
            EES2019_be_stack_wl[[3]],
            by=c('respid', 'party'))


# Flanders # 

EES2019_be_stack_fl <- 
  lapply(data = EES2019_be_lst$Flanders,
         cdbk = EES2019_cdbk_be_lst$Flanders,
         crit = 'average',
         rescale = T,
         check = F,
         keep_id = T,
         X = list('Q10','Q11','Q23'),
         FUN = gendis.fun) 

EES2019_be_stack_fl <- 
  left_join(EES2019_be_stack_fl[[1]], 
            EES2019_be_stack_fl[[2]], 
            by=c('respid', 'party')) %>% 
  left_join(., 
            EES2019_be_stack_fl[[3]],
            by=c('respid', 'party'))


# Undefined values 


EES2019_be_stack_und <- 
  EES2019_be_stack %>% 
  dplyr::select(respid, party, el_coll) %>% 
  filter(el_coll=='Undefined') %>% 
  mutate(Q10_gen = NA_real_,
         Q11_Q13_gen = NA_real_,
         Q23_Q24_gen = NA_real_) %>% 
  dplyr::select(-c(el_coll))


rbind(EES2019_be_stack_fl, EES2019_be_stack_wl) %>% 
  rbind(., EES2019_be_stack_und)


  cbind(EES2019_be_stack,
        lapply(data = EES2019_be_lst$Wallonia,
               cdbk = EES2019_cdbk_be_lst$Wallonia,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()



# Previous code for identifying the problem # ===========================================================

EES2019_cdbk_be %>%
  dplyr::select(Region,Q7, q25, Q10_PTV, Q13_left_right,Q24_EU)


voting_regions <- 
    EES2019_be %>% 
  dplyr::select(respid, Q7, Q2, Q9, Q25_rec, region_NUTS1, 
                starts_with('q13'), starts_with('q24')) %>%
  # distinct %>% 
  mutate(Q7 = as.numeric(Q7),
         electoral_college = case_when(Q7 %in% 201:207 | Q2 %in% 201:207 | Q25_rec %in% 201:207 |
                                     region_NUTS1=='BE2' ~ 'Flanders',
                                   Q7 %in% 208:214 | Q2 %in% 208:214 | Q25_rec %in% 208:214 |
                                     region_NUTS1=='BE3' ~ 'Wallonia',
                                   T ~ 'Undefined')) %>% 
  split(.$voting_region)
  

xfun <- function(votreg) {
  x <- 
    votreg %>% 
    dplyr::select(starts_with('q13'), starts_with('q24')) %>% 
    mutate(across(names(.), ~as.numeric(.))) %>% 
    mutate(across(names(.), ~case_when(.>10 ~ NA_real_, T~.))) %>% 
    mutate(across(names(.), ~mean(., na.rm=T))) %>% 
    distinct()
  
  return(x)
}

lapply(voting_regions, xfun)