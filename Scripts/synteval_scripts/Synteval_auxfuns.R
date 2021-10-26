# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Auxiliary functions (Synthetic variables evaluation)
# Authors: G.Carteny
# last update: 2021-10-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Inverse %in% operator # ==============================================================================

'%!in%' <- function(x,y)!('%in%'(x,y))

# Logit to probs # =====================================================================================

logit2prob.auxfun <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Functions for estimating rmse of OLS models # ========================================================


rmse.auxfun = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm=T))
}


get_rmse.auxfun = function(model, data, regmod) {
  
  if (regmod=='OLS') {
    prdctd <- predict(model, data)
  } else if (regmod=='logit') {
    prdctd <- predict(model, data,type='response') %>% rbinom(length(.),1,.)
  }
  
  response = names(data)[2]
  rmse.auxfun(actual = subset(data, select = response, drop = TRUE) %>% as.numeric,
              predicted = prdctd)
  
}


get_complexity.auxfun <- function(model) {
  length(coef(model)) - 1
}


# Function for estimating the null model # =============================================================

null_mod.auxfun <- function(df, regmod) {
  y <- names(df)[2]
  frml <- as.formula(paste0(y, ' ~ ', '1'))
  
  df %<>% na.omit()
  
  if (regmod=='OLS') {
    fit <- lm(data = df, formula = frml)
  } else if (regmod=='logit') {
    fit <- glm(data = df, formula = frml, family = binomial)
  }
  
  return(fit)
}


# Function for creating the syntvars regression models data frames # ===================================

regdf.auxfun <- function(data, depvar, cat.indvar, cont.indvar) { 
  
  if (depvar=='Q7_gen' | depvar=='vote choice' | depvar=='Q7') {
    depvar <- 'Q7_gen'
  } else if (depvar=='Q10' | depvar=='PTV' | depvar=='Q10_gen') {
    depvar <- 'Q10_gen'
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


# y - xi cross-tabs # ==================================================================================

tab.auxfun <- function(data, y, x, na = T, perc = F, which_perc = 'all') {
  df %<>% dplyr::select(all_of(y), all_of(x)) 
  
  if (na==F) {
    df %<>% na.omit
  }
  
  tab <- table(df[[toString(y)]], df[[toString(x)]], useNA = 'ifany') %>% as.data.frame()
  names(tab) <- c(y, x, 'Freq')
  tab %<>% pivot_wider(id_cols = all_of(y), names_from = c(all_of(x)), values_from = 'Freq')
  names(tab)[[1]] <- paste0(y,'/',x)
  
  if (perc) {
    tab %<>% 
      adorn_totals(where = c('row','col')) %>%
      adorn_percentages(which_perc) %>%
      adorn_pct_formatting() %>%
      adorn_ns(position = 'front')
  } else {
    tab %<>% 
      adorn_totals(where = c('row','col')) 
  }
  
  tab %<>% as_tibble
  
  return(tab)
}



# Anova tables # =======================================================================================

anova.auxfun <- 
  function(mdl_lst1, mdl_lst2, table = F) {
    
    if(length(mdl_lst1)!=length(mdl_lst2)) {
      stop("Model lists have different length.")
    } else {
      
      anova_lst <- lapply(1:length(mdl_lst1), 
                          function(x){
                            anova(mdl_lst1[[x]], 
                                  mdl_lst2[[x]], 
                                  test='Chisq')
                          })
      if (table==T) {
        anova_lst %<>% 
          lapply(., function(x) {
            x %<>% 
              as_tibble %>% 
              mutate('Model' = c('Partial', 'Full')) %>% 
              dplyr::select(length(.), 1:(length(.)-1))
            })
      }
      
      return(anova_lst)
      }
  }


# %>% 
#   as_tibble() %>% 
#   mutate('Depvar' = partmod_lst[[x]]$terms %>% attr(.,'variables') %>% as.character() %>% .[2]) %>% 
#   mutate('Model'  = c('Partial', 'Full')) %>% 
#   dplyr::select(Depvar, Model, `Resid. Df`, `Resid. Dev`, Df, Deviance, `Pr(>Chi)`)