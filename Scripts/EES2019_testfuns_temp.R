
# Functions ####

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_complexity <- function(model) {
  length(coef(model)) - 1
}

test.fun <- function(data, depvar, cat.indvar, cont.indvar, regsum, yhat.name) { 
  
  if (depvar=='Q7_gen' | depvar=='vote choice' | depvar=='Q7') {
    depvar <- 'Q7_gen'
  } else if (depvar=='Q10' | depvar=='PTV' | depvar=='Q10_gen') {
    depvar <- 'Q10_gen'
  }
  
  if (depvar=='Q7_gen' | depvar=='vote choice' | depvar=='Q7') {
    yhat.name <- paste0(yhat.name, '_vc')
  } else if (depvar=='Q10' | depvar=='PTV' | depvar=='Q10_gen') {
    yhat.name <- paste0(yhat.name, '_ptv')
  }
  
  
  
  indep_df <- 
    data %>% 
    dplyr::select(respid, all_of(cat.indvar), all_of(cont.indvar)) %>% 
    distinct() %>% 
    mutate(across(all_of(cat.indvar), ~as.factor(.))) %>% 
    mutate(across(all_of(cont.indvar), ~as.numeric(.))) 
  
  dep_df <- 
    data %>% 
    dplyr::select(respid, party, all_of(depvar)) %>% 
    pivot_wider(id_cols = c('respid'), 
                names_from = 'party', names_prefix = 'stack_',
                values_from = all_of(depvar)) 
  
  if (depvar=='Q7_gen' | depvar=='vote choice' | depvar=='Q7') {
    dep_df %<>% 
      mutate(across(starts_with('stack_'), ~as.factor(case_when(.>1 ~ NA_integer_, T~.))))
  } else if (depvar=='Q10' | depvar=='PTV' | depvar=='Q10_gen') {
    dep_df %<>% 
      mutate(across(starts_with('stack_'), ~as.numeric(case_when(.>10 ~ NA_real_, T~.)))) 
  }
  
  depvars <- 
    dep_df %>% dplyr::select(starts_with('stack')) %>% names(.)
  
  frmla_lst <- 
    lapply(depvars, function(y) {
      formula(paste(y, paste(c(cat.indvar, cont.indvar), collapse = " + "), sep = " ~ "))
    })
  
  
  df_lst <- 
    lapply(depvars, function(x) {
      df <- dep_df %>% dplyr::select(respid, all_of(x)) 
      df <- left_join(df, indep_df, by='respid')
    })
  
  return(df_lst)
  
}

# Test ####

df_lst <- test.fun(data = EES2019_hr_stack,
                   depvar = 'Q7_gen',
                   cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), # 'D6_une', 'D6_rec', 'D9_rec'
                   cont.indvar =  c('D4_age', 'D10_rec'),
                   yhat.name = 'socdem',
                   regsum = T)



df <- df_lst[[6]]

df %<>% dplyr::select(-c(respid))
y <- df[,1] %>% names() 
xs <- df[,-1] %>% names()
# df %<>% na.omit()

df %<>% 
  mutate(D6_une = as.numeric(D6_une),
         D6_une = case_when(stack_413==1 & D6_une!=0 ~ .5, T ~ D6_une ))

tabs_lst <-
  lapply(xs,
         function(x) {
           tab <-    
             table(df[[toString(y)]], df[[toString(x)]], useNA = 'ifany') %>% 
             as.data.frame.matrix()  
           # names(tab)[1] <- y
           # names(tab)[2] <- x
           
           return(tab)
         }) 

