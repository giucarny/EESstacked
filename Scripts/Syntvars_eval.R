# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Synthetic variables evaluation Script 
# Authors: G.Carteny
# last update: 2021-09-29
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Additional functions/operators # =====================================================================


# Inverse %in% operator # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

'%!in%' <- function(x,y)!('%in%'(x,y))

# Functions for estimating rmse values for OLS models # - - - - - - - - - - - - - - - - - - - - - - - -

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

get_complexity <- function(model) {
  length(coef(model)) - 1
}


# Function for creating the syntvars regression models data frames # - - - - - - - - - - - - - - - - - -

regdf.fun <- function(data, depvar, cat.indvar, cont.indvar, regsum, yhat.name) { 
  
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

# Check the full models' results # =====================================================================

# (1) run the EES2019_stack.R script until the 'Stack the original EES2019 variables' section and 
# (2) run the the countryspecific script until the 'Generic distance/proximity variables' estimation

# Check the results # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

stckd_data <- EES2019_hr_stack

fit_lst <-
  gensyn.fun(data = stckd_data,
             depvar = 'Q10_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #'D6_une', 'D6_rec', 'D9_rec'
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name = 'socdem',
             regsum = T)

# lapply(fit_lst, summary)
# lapply(fit_lst, car::vif)

fit_lst <-
  gensyn.fun(data = stckd_data,
             depvar = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #, 'D6_une' 'D6_rec', 'D9_rec'
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name = 'socdem',
             regsum = T)

# lapply(fit_lst, summary)
# lapply(fit_lst, car::vif)


# Get the regression models dataframes # ===============================================================


df_lst <- regdf.fun(data = stckd_data,
                    depvar = 'Q7_gen',
                    cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), # 'D6_une', 'D6_rec', 'D9_rec'
                    cont.indvar =  c('D4_age', 'D10_rec'),
                    yhat.name = 'socdem',
                    regsum = T)


# Evaluate problematic models # ========================================================================


# Get y-xi contingency tables for possible empty cells # - - - - - - - - - - - - - - - - - - - - - - - -

tabs_lst <-
  lapply(df_lst, function(df) {
    df %<>% 
      dplyr::select(-c(respid)) 
    
    y <- df[,1] %>% names() 
    xs <- df[,-1] %>% names()
    
  })


df <- df_lst[[1]]



# %>% 
#   mutate(EDU_rec = as.numeric(EDU_rec),
#          EDU_rec2 = case_when(EDU_rec==1 | EDU_rec==2 ~ 0,
#                               EDU_rec==3 ~ 1,
#                               EDU_rec > 1 ~ NA_real_,
#                               T ~ EDU_rec),
#          EDU_rec = as.factor(EDU_rec),
#          EDU_rec2 = as.factor(EDU_rec2))



tabs_lst <-
  lapply(xs,
         function(x) {
           tab <-    
             table(df[[toString(y)]], df[[toString(x)]], useNA = 'ifany') %>% 
             as.data.frame()  
           names(tab)[1] <- y
           names(tab)[2] <- x
           
           return(tab)
           }) 

df %<>% na.omit()


frml_edu1 <- formula(paste(y, paste(xs[xs!='EDU_rec2'], collapse = " + "), sep = " ~ "))
frml_edu2 <- formula(paste(y, paste(xs[xs!='EDU_rec'], collapse = " + "), sep = " ~ "))
frml_drop <- formula(paste(y, paste(xs[xs %!in% c('EDU_rec', 'EDU_rec2', 'D5_rec')], collapse = " + "), sep = " ~ "))


fit_null <- glm(data = df, formula = stack_412 ~ 1, family = binomial)
fit_edu1 <- glm(data = df, formula = frml_edu1, family = binomial)
fit_edu2 <- glm(data = df, formula = frml_edu2, family = binomial)
fit_drop <- glm(data = df, formula = frml_drop, family = binomial)

# summary(fit_null)
# summary(fit_edu1)
# summary(fit_edu2)
# summary(fit_drop)

anova(fit_null, fit_drop, test='Chisq')


# https://daviddalpiaz.github.io/r4sl/logistic-regression.html
# For ConfusionMatrix (sensitivity, specificty, etc.)

predclass_null <- ifelse(predict(fit_null, type='response') > 0.5, 1, 0)
predclass_edu1 <- ifelse(predict(fit_edu1, type='response') > 0.5, 1, 0)
predclass_drop <- ifelse(predict(fit_drop, type='response') > 0.5, 1, 0)

tab_null <- table(predicted = predclass_null, actual = df$stack_412)
tab_edu1 <- table(predicted = predclass_edu1, actual = df$stack_412)
tab_drop <- table(predicted = predclass_drop, actual = df$stack_412)
caret::confusionMatrix(tab_drop, positive='Yes')


