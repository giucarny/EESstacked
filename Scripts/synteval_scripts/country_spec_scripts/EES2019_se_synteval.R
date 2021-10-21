# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Sweden Sample) 
# Author: J.Leiser
# last update: 2021-10-19
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr", "rlang", "car",
         "caret", "DescTools", "stargazer", "kableExtra")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())

# General workflow # ===================================================================================

# Load the full version of the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019 <- read_dta(here('Data', 'EES2019', 'ZA7581_v1-0-0.dta'))

# Mutate the EES 2019 voter study # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

source(here('Scripts', 'EES2019_datamut.R'))


# Source the auxiliary data set # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

source(here('Scripts', 'aux_data_scripts', 'EES2019_cdbk_enh.R'))


# Harmonize Q25 and Q9 values # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q25_rec.R'))

source(here('Scripts', 'aux_data_scripts', 'EES2019_Q9_rec.R'))


# Load auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'EES2019_stack_funs.R'))


# Stack observations # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

invisible(
  lapply(paste0(here('Scripts', 'country_spec_scripts'),'/', 
                here('Scripts', 'country_spec_scripts') %>% list.files(pattern = '_stack')),
         source)  
)

EES2019_stckd <- mget(ls(pattern = '_stack')) %>% do.call('rbind',.)
rm(list=ls(pattern='_stack'))

# Stack the original EES2019 variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_stckd %<>% left_join(., EES2019, by=c('countrycode', 'respid'))

# Country-spec workflow # ==============================================================================

cntry = 'SE'

EES2019_se <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_se <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_se <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_se_stack <- 
  cbind(EES2019_stckd_se,  
        lapply(data = EES2019_stckd_se, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_se_stack %<>%
  cbind(.,
        lapply(data = EES2019_se,
               cdbk = EES2019_cdbk_se,
               stack = EES2019_se_stack, 
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

csdf_lst <- list('std'  = EES2019_se,
                 'cdbk' = EES2019_cdbk_se,
                 'SDM'  = EES2019_se_stack)


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
# no problems


# Syntvars evaluation: logit models summary # ==========================================================

# stargazer::stargazer(fullmod_lst$logit, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# some point estimates are rather high, but overall there are no extremely "unusual/problematic" estimates

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

# Summary of Evaluation Results ---------------------------------------------------------
# ols_df
# fulllogit_df
# nulllogit_df

# OLS: no issues detected

# Logit: no issues with the parameter estimates
# comparing the full and null models:
# the full model fits better except for stack_2704 and stack_2709 where the full model fits
# worse than the null model. However, model fits do not differ by much, so this should not be a 
# concerning issue

# Conclusion --------------------------------------------------------------

# No changes need to be made

# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))