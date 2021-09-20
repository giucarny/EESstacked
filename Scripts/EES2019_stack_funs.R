# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Auxiliary Functions for the Stacking Procedure
# Author: G.Carteny
# last update: 2021-09-20
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generic dichotomous variables estimation # ===========================================================

gendic.fun <- function(data, var, stack_var) {
  
  mainexp <- paste0(var, '>=96 & ', var,'<100', ' ~ as.integer(', var,'), ')
  addexp <- ''
  
  if (var == 'Q9_rec') {
    mainexp <- paste0(var, '>96 & ', var,'<100', ' ~ as.integer(', var,'), ')
    addexp <- paste0('is.na(',var,') ~ as.integer(0),')
  } else if (var == 'Q7') {
    mainexp <- paste0(var, '>96 & ', var,'<100', ' ~ as.integer(', var,'), ')
  } 
  
  data[[var]] <- data[[var]] %>% as.integer
  
  newvar <- paste0(var, '_gen')
  exprss <- paste0('case_when(', mainexp,
                              var, ' == ', stack_var, ' ~ as.integer(1),',
                              addexp,
                              'T ~ as.integer(0))')
  
  q <- quote(mutate(data, !! newvar := exprss))
  
  df <-
    eval(parse(text=sub("exprss", exprss, deparse(q)))) %>%
    dplyr::select(all_of(newvar)) %>%
    as_tibble() %>%
    zap_labels()
  
  return(df)
}


# Generic Distance/Proximity variables estimation # ====================================================


gendis.fun <- 
  function(data, cdbk, stack, vrbl, crit, rescale, check, keep_id) {
    if (vrbl=='Q11') {
      cdbk_vrbl = c('Q7', 'Q13_left_right')
    } else if (vrbl=='Q23') {
      cdbk_vrbl = c('Q7', 'Q24_EU')
    } else if (vrbl=='Q10') {
      cdbk_vrbl = c('Q7', 'Q10_PTV')
    }
    
    if (is_missing(stack)) {
      party <- 
        get(paste0(deparse(substitute(data)), '_stack')) %>% 
        as.data.frame() %>% 
        .[['party']] %>% 
        unique
    } else {
      party <-
        stack[['party']] %>% 
        unique
    }
    
    
    # if (str_detect(deparse(substitute(data)), pattern = '_be')==F) {
    #     
    # } else {
    #   print('ciao')
    # }
    
        
    corr_tab <- 
      cdbk %>% 
      dplyr::select(all_of(cdbk_vrbl)) %>%
      filter(Q7 %in% party) %>% 
      mutate(across(all_of(cdbk_vrbl), ~tolower(.))) %>% 
      na.omit() 
    
    trgt_vrbls <- 
      corr_tab %>% 
      .[[cdbk_vrbl[cdbk_vrbl!='Q7']]]
    
    df <- data
    
    if (vrbl=='Q11' | vrbl=='Q23') {
      exprss0 <- 'dplyr::select(.data = df, respid, all_of(vrbl), all_of(trgt_vrbls))'  
    } else if (vrbl=='Q10') {
      exprss0 <- 'dplyr::select(.data = df, respid, all_of(trgt_vrbls))'
    }
    
    df <- 
      eval(parse(text = exprss0))
    
    df %<>% mutate(across(names(.), ~as.numeric(.)))
    
    
    if (vrbl=='Q11' | vrbl=='Q23') {
      
      cond <- '.>10'  
      
      exprss1 <- 
        paste0('mutate(.data=df, across(c(all_of(trgt_vrbls), all_of(vrbl)),', 
               '~case_when(', cond, '~ NA_real_, T~.)))')
      
      exprss2_head <- 'mutate(.data=df, across(all_of(trgt_vrbls),'
      exprss2_tail <- '))'
      
      if (crit == 'idiosyncratic') {
        exprss2_mid <- paste0('~abs(', vrbl, '-.)')
      } else if (crit == 'average') {
        exprss2_mid <- paste0('~abs(', vrbl, '-mean(., na.rm=T))')
      }
      
      exprss2 <- paste0(exprss2_head, exprss2_mid, exprss2_tail)
      
      
      df <- eval(parse(text = exprss1))  
      df_check = df
      df <- eval(parse(text = exprss2))
      
      df %<>% 
        dplyr::select(-c(all_of(vrbl)))
      
    } else if (vrbl=='Q10') {
      
      cond <- '.>10'  
      
      exprss1 <- 
        paste0('mutate(.data=df, across(c(all_of(trgt_vrbls)),', 
               '~case_when(', cond, '~ NA_real_, T~.)))')
      
      df <- eval(parse(text = exprss1))
      df_check = df
      
    }
    
    names(df)[names(df) %in% trgt_vrbls] <- corr_tab[['Q7']]  
    
    if (vrbl=='Q11') {
      newvar <- 'Q11_Q13_gen'
    } else if (vrbl=='Q23') {
      newvar <- 'Q23_Q24_gen'
    } else if (vrbl=='Q10') {
      newvar <- 'Q10_gen'
    }
    
    df_stack  <- 
      df %>% 
      pivot_longer(cols = names(.)[str_detect(names(.),'^[0-9]')], 
                   names_to = 'party',
                   values_to = newvar) 
    
    if (rescale) {
      
      if (vrbl=='Q10' | vrbl=='Q11' | vrbl=='Q23') {res_fact = '10'}
      
      df_stack <- 
        eval(
          parse(
            text = paste0('mutate(.data=df_stack,', newvar, '=', newvar,'/', res_fact,')')
          )
        )
    }
    
    df_stack %<>%
      mutate(across(all_of(newvar), ~case_when(is.na(.) ~ as.numeric(98), T~.)))
    
    if (keep_id==F) {
      df_stack %<>% dplyr::select(-c(respid, party))
    } 
    
    
    if (check) {
      df_lst <- 
        list(df_check, df_stack)
      return(df_lst)
    } else {
      return(df_stack)
    }
  }



# Synthetic variables estimation # =====================================================================


gensyn.fun <- function(data, depvar, cat.indvar, cont.indvar, regsum, yhat.name) { 
  
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
    
    return(pred_df)
    
  } else {
    
    return(fit_lst)
    
  }
}