# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Greece Sample) 
# Author: J.Leiser
# last update: 2021-11-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
                                                   'D1_rec', 'D7_rec', 'D6_une'),
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



# Syntvars evaluation: logit models summary # ==========================================================

# stargazer::stargazer(fullmod_lst$logit, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')



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

logit_df <- 
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
  rbind(.,
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
        ))


logit_df %<>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)

# AIC data frames # ====================================================================================

# OLS AIC df 

ols_aic <- 
  ols_df %>%
  pivot_wider(id_cols = c('depvar', 'partycode', 'partyname_eng'), values_from = 'AIC',
              names_from = 'model') %>%
  mutate(diff = full - null) %>%
  mutate(across(c('full', 'null', 'diff'), ~round(.,3))) %>%
  dplyr::select(-c(partyname_eng))

# Logit AIC df 

logit_aic <- 
  logit_df %>%
  pivot_wider(id_cols = c('depvar', 'partycode', 'partyname_eng'), values_from = 'AIC',
              names_from = 'model') %>%
  mutate(diff = full - null) %>%
  mutate(across(c('full', 'null', 'diff'), ~round(.,3))) %>%
  dplyr::select(-c(partyname_eng))

# Full models evaluation # =============================================================================

# some logit models show inflated SE on some predictors, more specifically: 
# Model 3: EDU_rec (both categories), D1_rec
# Model 4: D7_rec (second category) 

# Constant term in model 4 is not affected by inflated SEs of predictors. 
# Model 3  constant is affected showing unusual values. We deal with model 3 as it is affected 
# by separation issue.

# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 3 #-----------------------------------------------------------------

mdl  <- 3
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec', 'D1_rec')

tabs <- lapply(data=df, y='stack_1203', na=T, X = cols, FUN = tab.auxfun)
# no respondent with low education voted for party 1203
# only 2 respondents who are union members voted for the party.


# Syntvars evaluation: partial logit models # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -
vrbls_2_drop <- c('EDU_rec', 'D1_rec')

regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop)))
  }) 

partmod_lst <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(3)

anova_lst <- 
  anova.auxfun(mdl_lst1 = partmod_lst[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

#LR test for model 3: p-value = 0.00848 < 0.05, so we can reject H0.
# Hence, we keep the full model and change nothing.

# # Get the df for and estimate the new partial models only removing EDU_rec # - - - - - - - - - - - - - - - - - - - - - - - - - - -
# # only removing EDU_rec
# vrbls_2_drop <- c('EDU_rec')
# 
# regdf_lst_part <- 
#   regdf_lst$logit %>% 
#   lapply(., function(x){
#     x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop)))
#   }) 
# 
# partmod_lst <- 
#   lapply(regdf_lst_part, function(x){
#     y    <- names(x)[startsWith(names(x), 'stack')]
#     xs   <- names(x)[3:length(x)]
#     frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
#     
#     fit <- glm(data = x, formula = frml, family = binomial)
#     
#     return(fit)
#   })
# 
# # LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# 
# mdls <- c(3)
# 
# anova_lst <- 
#   anova.auxfun(mdl_lst1 = partmod_lst[c(mdls)],
#                mdl_lst2 = fullmod_lst$logit[c(mdls)],
#                table = T)
# 
# #LR test for model 3: p-value = 0.156 < 0.05, so we cannot reject H0.

# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

finalmod_lst <- list()
finalmod_lst[['OLS']] <- fullmod_lst[['OLS']]
finalmod_lst[['logit']] <- fullmod_lst[['logit']]

#  if variables are removed/for synteval markdown script/table
finalmod_lst[['logit']][[6]] <- finalmod_lst[['logit']][[5]]
finalmod_lst[['logit']][[5]] <- finalmod_lst[['logit']][[4]]
finalmod_lst[['logit']][[4]] <- partmod_lst[[mdls]]


# Syntvars evaluation: Updating AIC data frames (logit only) # =========================================

# logit AIC df 

logit_aic %<>%
  rbind(.,
        tibble('depvar'    = 'stack_1203',
               'partycode' = 1203,
               'full'      = partmod_lst[[mdls]] %>% AIC,
               'null'      = nullmod_lst$logit[[mdls]] %>% AIC,
        ) %>%
          mutate(diff = full-null)) %>%
  .[order(.$depvar, .$partycode),]



# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))
