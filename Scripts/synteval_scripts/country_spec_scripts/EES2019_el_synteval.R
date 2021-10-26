# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Greece Sample) 
# Author: J.Leiser
# last update: 2021-10-26
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr", "rlang", "car",
         "caret", "DescTools", "stargazer", "kableExtra")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())

# Source the general workflow # ========================================================================

source(here('Scripts', 'synteval_scripts', 'Synteval_gen.R'))

# Country-spec workflow # ==============================================================================

cntry = 'EL'

EES2019_el <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_el <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_el <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_el_stack <- 
  cbind(EES2019_stckd_el,  
        lapply(data = EES2019_stckd_el, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_el_stack %<>%
  cbind(.,
        lapply(data = EES2019_el,
               cdbk = EES2019_cdbk_el,
               stack = EES2019_el_stack, 
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Syntvars evaluation: Functions, variables and data frames # ==========================================

# Source auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'synteval_scripts', 'Synteval_auxfuns.R'))

# Country-specific data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

csdf_lst <- list('std'  = EES2019_el,
                 'cdbk' = EES2019_cdbk_el,
                 'SDM'  = EES2019_el_stack)


# Synthetic variables estimation variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

syntvars_vrbls <- list('dep'   = list('OLS'     = 'Q10_gen', 
                                      'logit'   = 'Q7_gen'),
                       'indep' = list('ctgrcl' = c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 
                                                   'D1_rec', 'D7_rec'),
                                      'cntns'  =  c('D4_age', 'D10_rec')))


# Synthetic variables estimation data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


regdf_lst  <- list('OLS'   = regdf.auxfun(data        = csdf_lst$SDM,
                                          depvar      = syntvars_vrbls$dep$OLS,
                                          cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                          cont.indvar = syntvars_vrbls$indep$cntns),
                   'logit' = regdf.auxfun(data        = csdf_lst$SDM,
                                          depvar      = syntvars_vrbls$dep$logit,
                                          cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                          cont.indvar = syntvars_vrbls$indep$cntns))


# Relevant parties data frame # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

relprty_df <- 
  tibble('depvar'    = 
           lapply(1:length(regdf_lst$OLS), function(x){names(regdf_lst$OLS[[x]]) %>% .[2]}) %>% 
           unlist,
         'partycode' =
           lapply(1:length(regdf_lst$OLS), function(x){names(regdf_lst$OLS[[x]]) %>% .[2]}) %>% 
           unlist %>% 
           gsub('stack_','',.) %>% 
           as.numeric)  

relprty_df %<>% 
  mutate('partyname_eng' = 
           csdf_lst$cdbk %>% 
           dplyr::select(partyname_eng, Q7) %>% 
           filter(Q7 %in% relprty_df[['partycode']]) %>% 
           .[['partyname_eng']])


# Syntvars evaluation: Null and full regression models # ===============================================

set.seed(123)

fullmod_lst <- list('OLS'   = gensyn.fun(data        = csdf_lst$SDM,
                                         depvar      = syntvars_vrbls$dep$OLS,
                                         cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                         cont.indvar = syntvars_vrbls$indep$cntns,
                                         yhat.name   = 'socdem_synt',
                                         regsum      = T),
                    'logit' = gensyn.fun(data        = csdf_lst$SDM,
                                         depvar      = syntvars_vrbls$dep$logit,
                                         cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                         cont.indvar = syntvars_vrbls$indep$cntns,
                                         yhat.name   = 'socdem_synt',
                                         regsum      = T))

nullmod_lst <- list('OLS'   = lapply(X = regdf_lst$OLS,   regmod = 'OLS',   null_mod.auxfun),
                    'logit' = lapply(X = regdf_lst$logit, regmod = 'logit', null_mod.auxfun))


# fullmod_lst$OLS %>% lapply(.,summary)
# fullmod_lst$logit %>% lapply(.,summary)

# Syntvars evaluation: OLS models summary # ============================================================

# stargazer::stargazer(fullmod_lst$OLS, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'PTV',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# no unusual estimates or SEs 


# Syntvars evaluation: logit models summary # ==========================================================

# stargazer::stargazer(fullmod_lst$logit, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# Models 1, 2, 5: no issues
# Model 3: parameter estimates of EDU_rec2, EDU_rec3. D1_rec1 and constant
# unusually high
# Model 4: Estimates + SE of D7_rec2  unusually high, estimate of constant relatively high.


# Syntvars evaluation: OLS models fit stats # ==========================================================

# RMSE and Rsq # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


ols_df <- 
  tibble(
    'depvar'  = lapply(1:length(regdf_lst$OLS), 
                       function(x){
                         names(regdf_lst$OLS[[x]]) %>% .[2]
                       }) %>% unlist,
    'model'   = rep('full',length(regdf_lst$OLS)),
    'Rsq'     = lapply(1:length(fullmod_lst$OLS),
                       function(x) {
                         fullmod_lst$OLS[[x]] %>% summary %>% .$r.squared %>% round(., 3)
                       }) %>% unlist,
    'Adj_Rsq' = lapply(1:length(fullmod_lst$OLS),
                       function(x) {
                         fullmod_lst$OLS[[x]] %>% summary %>% .$adj.r.squared %>% round(., 3)
                       }) %>% unlist,
    'AIC'     = lapply(1:length(fullmod_lst$OLS),
                       function(x) {
                         fullmod_lst$OLS[[x]] %>% AIC
                       }) %>% unlist) %>% 
  rbind(.,
        tibble(
          'depvar'  = lapply(1:length(regdf_lst$OLS), 
                             function(x){
                               names(regdf_lst$OLS[[x]]) %>% .[2]
                             }) %>% unlist,
          'model'   = rep('null',length(regdf_lst$OLS)),
          'Rsq'     = lapply(1:length(nullmod_lst$OLS),
                             function(x) {
                               nullmod_lst$OLS[[x]] %>% summary %>% .$r.squared %>% round(., 3)
                             }) %>% unlist,
          'Adj_Rsq' = lapply(1:length(nullmod_lst$OLS),
                             function(x) {
                               nullmod_lst$OLS[[x]] %>% summary %>% .$adj.r.squared %>% round(., 3)
                             }) %>% unlist,
          'AIC'     = lapply(1:length(fullmod_lst$OLS),
                             function(x) {
                               nullmod_lst$OLS[[x]] %>% AIC
                             }) %>% unlist))

ols_df %<>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Rsq, Adj_Rsq, AIC)



# Syntvars evaluation: logit models fit stats # ========================================================


fulllogit_df <- 
  tibble(
    'depvar'     = lapply(1:length(regdf_lst$logit), 
                          function(x){
                            names(regdf_lst$OLS[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('full',length(regdf_lst$logit)),
    'Ps_Rsq'     = lapply(1:length(fullmod_lst$logit),
                          function(x){
                            DescTools::PseudoR2(fullmod_lst$logit[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(fullmod_lst$logit),
                          function(x){
                            DescTools::PseudoR2(fullmod_lst$logit[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(fullmod_lst$logit),
                          function(x) {
                            fullmod_lst$logit[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)



nulllogit_df<- 
  tibble(
    'depvar'     = lapply(1:length(regdf_lst$logit), 
                          function(x){
                            names(regdf_lst$OLS[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('null',length(regdf_lst$logit)),
    'Ps_Rsq'     = lapply(1:length(nullmod_lst$logit),
                          function(x){
                            DescTools::PseudoR2(nullmod_lst$logit[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(nullmod_lst$logit),
                          function(x){
                            DescTools::PseudoR2(nullmod_lst$logit[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(fullmod_lst$logit),
                          function(x) {
                            nullmod_lst$logit[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Summary Evaluation Results ---------------------------------------------------
# ols_df
# fulllogit_df
# nulllogit_df

# OLS: no issues

# Logit:
# Models 1, 2, 5: no issues
# Model 3: parameter estimates of EDU_rec2, EDU_rec3. D1_rec1 and constant
# unusually high
# Model 4: Estimates + SE of D7_rec2  unusually high, estimate of constant relatively high.
# not all full logit models fit better than the null models


# Identifying Logit Misfit Source ----------------------------------------

# look at EDU_rec and D1_rec for model 3, i.e. stack_1203
# look at D7_rec for model 4, i.e. stack_1204

#Model 3
#looking at EDU_rec
df <- regdf_lst$logit[[3]]
table(df$stack_1203, df$EDU_rec) #empty cell

# looking at D1_rec
table(df$stack_1203, df$D1_rec) # almost empty cell (only 2 observations)

# Model 4
# looking at D7_rec
df <- regdf_lst$logit[[4]]
table(df$stack_1204, df$D7_rec) #empty cell

# to get rid of these issues:
# 1st estimate models without EDU_rec and D1_rec to see if this fixes the issue in model 3
# 2nd estimate models without D7_rec to see if thsi fixes the issue in model 4

# 1. estimate Partial Logit Model for model 3 --------------------------------------------
# remove EDU_rec and D1_rec
regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c( EDU_rec, D1_rec))})

partmod_lst <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

anova_lst <- 
  anova.auxfun(mdl_lst1 = partmod_lst,
               mdl_lst2 = fullmod_lst$logit,
               table = F)

# Results
# Model 1: Do not reject H0
# Model 2: Do not reject H0
# Model 3: Do reject H0 at 5% level!
# Model 4: Do reject H0 at 5% level!
# Model 5: Do not Reject H0


# look at new parameter estimates
# stargazer::stargazer(partmod_lst, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# model 3 does not exhibit unusual estimates anymore, but test indicates that model including the 
# problematic variables fits better

# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


partlogit_df <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst), 
                          function(x){
                            names(regdf_lst_part[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial',length(regdf_lst_part)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst),
                          function(x) {
                            partmod_lst[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Syntvars evaluation: New logit models fit stats # ====================================================

logit_df <-  
  fulllogit_df %>% 
  rbind(., partlogit_df) %>% 
  rbind(., nulllogit_df)

# filter(logit_df, model == "full" | model == "partial")
# for model 3: full model does indeed fit better than partial model


# Conclusion for model 3 --------------------------------------------------------------

#stack_1203 poses some unusual problem as the model with the problematic variables fits better than the
# one without them.

# maybe just removing EDU_rec suffices to solve the problem and increase fit.

# 1.1 estimate Adjusted Partial Logit Model for model 3 --------------------------------------------
# remove EDU_rec
regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c( EDU_rec))})

partmod_lst <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

anova_lst <- 
  anova.auxfun(mdl_lst1 = partmod_lst,
               mdl_lst2 = fullmod_lst$logit,
               table = F)

# Results
# Model 1: Do not reject H0
# Model 2: Do not reject H0
# Model 3: Do not reject H0
# Model 4: Do not reject H0
# Model 5: Do not Reject H0


# look at new parameter estimates
# stargazer::stargazer(partmod_lst, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

#model 3: constant is no longer unusually high, but D1_rec still shows very large SEs
# constant increases from ca. -19 to -3

# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


partlogit_df <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst), 
                          function(x){
                            names(regdf_lst_part[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial',length(regdf_lst_part)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst),
                          function(x) {
                            partmod_lst[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Syntvars evaluation: New logit models fit stats # ====================================================

logit_df <-  
  fulllogit_df %>% 
  rbind(., partlogit_df) %>% 
  rbind(., nulllogit_df)

# filter(logit_df, model == "full" | model == "partial")
# for model 3: partial model does fit slightly better


# Conclusion for model 3 --------------------------------------------------------------

# just removing EDU_rec results in better fit and removes the problematic effect on the constant
# but D1_rec still shows very large SEs




# 2. estimate Partial Logit Model for model 4 --------------------------------------------
# remove D7_rec
regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c(D7_rec))})

partmod_lst <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

anova_lst <- 
  anova.auxfun(mdl_lst1 = partmod_lst,
               mdl_lst2 = fullmod_lst$logit,
               table = F)

# Results
# Model 1: Do not reject H0
# Model 2: Reject H0
# Model 3: Do not reject H0
# Model 4: Do reject H0 at very low significance level
# Model 5: Do not Reject H0


# look at new parameter estimates
# stargazer::stargazer(partmod_lst, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# no unusual parameter estimates, but tests indicate that the full model fits better than the constraint one 
# for stack_1204

# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


partlogit_df <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst), 
                          function(x){
                            names(regdf_lst_part[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial',length(regdf_lst_part)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst),
                          function(x){
                            DescTools::PseudoR2(partmod_lst[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst),
                          function(x) {
                            partmod_lst[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Syntvars evaluation: New logit models fit stats # ====================================================

logit_df <-  
  fulllogit_df %>% 
  rbind(., partlogit_df) %>% 
  rbind(., nulllogit_df)


# filter(logit_df, model == "full" | model == "partial")
# for model 4: full model does fit better than partial model.
# but removing the problematic variable might be prudent.


# Overall Conclusion ------------------------------------------------------

# We remove the problematic variables for which LR tests indicate better fit if we remove them
# Model 3, i.e. stack_1203: remove EDU_rec
# Model 4, i.e. stack_1204: change nothing

# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))
