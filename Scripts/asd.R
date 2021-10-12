
# New f(x) # ===========================================================================================

gensyn.fun <- function(data, depvar, cat.indvar, cont.indvar, regsum, yhat.name, stack_party) { 
  
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
  
  if (!is_missing(stack_party)) {
    stackparties <- paste0('stack_', stack_party)
    dep_df %<>% dplyr::select(respid, all_of(stackparties))
  }

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

  reg_lst <- list()
  for (i in 1:length(df_lst)) {
    reg_lst[[i]] <- list(data = df_lst[[i]],
                         frml = frmla_lst[[i]])
  }
  rm(i)
  
  fit_lst <-
    lapply(reg_lst, function(x) {
      if (depvar=='Q7_gen' | depvar=='vote choice' | depvar=='Q7') {
        fit <- glm(formula = x$frml, data = x$data, family = binomial())
      } else if (depvar=='Q10' | depvar=='PTV' | depvar=='Q10_gen') {
        fit <- lm(formula = x$frml, data = x$data)
      }

      if (regsum) {
        return(fit)
      } else {
        pred_fit <- predict(fit)
        return(pred_fit)
      }
    })


  if (regsum==F) {

    pred_df_lst <- list()
    for (i in 1:length(fit_lst)) {
      pred_df1 <- df_lst[[i]] %>%  mutate(id = rownames(.))
      pred_df2 <- fit_lst[[i]] %>% data.frame(yhat = ., id = attr(., 'names')) %>% as_tibble()
      pred_df_lst[[i]] <- left_join(pred_df1, pred_df2, by='id')
      
      }
    
    pred_df_lst <-
      lapply(pred_df_lst, function(x) {
        x %<>%
          mutate(party = names(.)[startsWith(names(.), 'stack_')]
                 %>% gsub('stack_', '', .) %>%
                   as.integer) %>%
          dplyr::select(respid, party, yhat)
      })

    pred_df <- do.call('rbind',pred_df_lst) %>% .[order(data$respid),]

    names(pred_df)[names(pred_df)=='yhat'] <- yhat.name
    
    pred_df %<>% distinct() %>% .[!is.na(.$respid),]

    return(pred_df)

  } else {

    return(fit_lst)

  }
}


# Full models # ========================================================================================

part.syntvars_vrbls <- list('dep'   = list('OLS'     = 'Q10_gen', 
                                           'logit'   = 'Q7_gen'),
                            'stack_party'    = c('501','502','503','504','505','507'),
                            'indep'  = list('ctgrcl' = c('D3_rec'),
                                            'cntns'  =  c('D4_age', 'D10_rec')))

preds_full <- 
  gensyn.fun(data        = csdf_lst$SDM,
             depvar      = syntvars_vrbls$dep$logit,
             cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
             cont.indvar = syntvars_vrbls$indep$cntns,
             yhat.name   = 'socdem_synt',
             regsum      = F)




preds_505 <- 
  gensyn.fun(data             = csdf_lst$SDM,
           depvar           = part.syntvars_vrbls$dep$logit,
           cat.indvar       = part.syntvars_vrbls$indep$ctgrcl, 
           cont.indvar      = part.syntvars_vrbls$indep$cntns,
           yhat.name        = 'socdem_synt',
           regsum           = F,
           stack_party      = c('505'))



preds_full_505 <- 
  preds_full %>% 
  filter(party==505) 


preds_full_505$respid

preds_505[!is.na(preds_505$respid),]

%>% 
  rbind(., preds_505)


 <- 
  left_join(preds_full, preds_505, by = c('respid','party'))



