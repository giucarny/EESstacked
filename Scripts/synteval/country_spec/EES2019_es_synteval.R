# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Spanish Sample) 
# Author: w.Haeussling
# last update: 2021-04-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Country-spec workflow # ==============================================================================

cntry = 'ES'

EES2019_es <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_es <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_es <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_es_stack <- 
  cbind(EES2019_stckd_es,  
        lapply(data = EES2019_stckd_es, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_es_stack %<>%
  cbind(.,
        lapply(data = EES2019_es,
               cdbk = EES2019_cdbk_es,
               stack = EES2019_es_stack, 
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

source(here('Scripts', 'synteval', 'Synteval_auxfuns.R'))

# Country-specific data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

csdf_lst <- list('std'  = EES2019_es,
                 'cdbk' = EES2019_cdbk_es,
                 'SDM'  = EES2019_es_stack)


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

# logit model 7 shows inflated SE on one predictor, more specifically: 
# Model 7: D10_rec

# Model 7's constant term is not affected by D10_rec inflated SE. 
# Therefore, we do not adapt the model.

# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

finalmod_lst <- list()
finalmod_lst[['OLS']] <- fullmod_lst[['OLS']]
finalmod_lst[['logit']] <- fullmod_lst[['logit']]


# stargazer::stargazer(finalmod_lst$logit, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')


# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))

