# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Lithuania Sample) 
# Author: J.Leiser
# last update: 2021-11-05
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Country-spec workflow # ==============================================================================

cntry = 'LT'

EES2019_lt <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lt <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lt <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_lt_stack <- 
  cbind(EES2019_stckd_lt,  
        lapply(data = EES2019_stckd_lt, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_lt_stack %<>%
  cbind(.,
        lapply(data = EES2019_lt,
               cdbk = EES2019_cdbk_lt,
               stack = EES2019_lt_stack, 
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

csdf_lst <- list('std'  = EES2019_lt,
                 'cdbk' = EES2019_cdbk_lt,
                 'SDM'  = EES2019_lt_stack)


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
# Model 1: D6_une
# Model 3: EDU_rec (both categories)
# Model 6: EDU_rec (both categories), D7_rec (2nd category), D6_une
# Model 7: EDU_rec (both categories)

# Constant term in model 1  is not affected by inflated SEs of predictors 
# The constant term in models 3,6,7 are affected showing unusual values. We deal with those models  
# as they are affected by separation issue.

# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 3 #-----------------------------------------------------------------

mdl  <- 3
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec')

tabs3 <- lapply(data=df, y='stack_1706', na=T, X = cols, FUN = tab.auxfun)

# no respondent with low education voted for party 1706.

# Model 6 #-----------------------------------------------------------------

mdl  <- 6
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec','D7_rec', 'D6_une')

tabs6 <- lapply(data=df, y='stack_1707', na=T, X = cols, FUN = tab.auxfun)

# no respondent with low education voted for party 1707
# no unemployed respondent voted for the party
# only 1 respondent wiht high social status voted for the party
# overall only 11 respondents voted for the party

# Model 7 #-----------------------------------------------------------------

mdl  <- 7
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec')

tabs7 <- lapply(data=df, y='stack_1702', na=T, X = cols, FUN = tab.auxfun)

# no respondent with low education voted for party 1702

# Conclusion # ------------------------------------------------------------

# Model 3: remove EDu_rec
# Model 6: remove EDU_rec, D7_rec, D6_une
# Model 7: remove EDU_rec


# Syntvars evaluation: partial logit models for models 3 & 7, remove EDU_rec # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -
vrbls_2_drop <- c('EDU_rec')

regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop)))
  }) 

partmod_lst_37 <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(3)

anova_lst3 <- 
  anova.auxfun(mdl_lst1 = partmod_lst_37[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)
# model 3: p-value = 0.0589 > 0.05, so we cannot reject H0

mdls <- c(7)

anova_lst7 <- 
  anova.auxfun(mdl_lst1 = partmod_lst_37[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

# model 7: p-value = 0.204 > 0.05, so we cannot reject H0
# Conclusion: remove EDU_rec from models 3 and 7

# Syntvars evaluation: partial logit models for model 6, remove EDU_rec, D7_rec, D6_une # ==========================================================
# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -
vrbls_2_drop <- c('EDU_rec', 'D7_rec', 'D6_une')

regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop)))
  }) 

partmod_lst_6 <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(6)

anova_lst6 <- 
  anova.auxfun(mdl_lst1 = partmod_lst_6[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

# LR test p-value = 0.128 > 0.05, so we cannot reject H0.
# Conclusion: remove EDU_rec, D6_une and D7_rec from model 6


# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

mdls3 <- c(3)
mdls7 <- c(7)
mdls6 <- c(6)

finalmod_lst <- list()
finalmod_lst[['OLS']] <- fullmod_lst[['OLS']]
finalmod_lst[['logit']] <- fullmod_lst[['logit']]

finalmod_lst[['logit']][[10]] <-  partmod_lst_37[[mdls7]] 
finalmod_lst[['logit']][[9]] <- finalmod_lst[['logit']][[7]]
finalmod_lst[['logit']][[8]] <- partmod_lst_6[[mdls6]]
finalmod_lst[['logit']][[7]] <- finalmod_lst[['logit']][[6]]
finalmod_lst[['logit']][[6]] <- finalmod_lst[['logit']][[5]]
finalmod_lst[['logit']][[5]] <- finalmod_lst[['logit']][[4]]
finalmod_lst[['logit']][[4]] <-  partmod_lst_37[[mdls3]]


# Syntvars evaluation: Updating AIC data frames (logit only) # =========================================

# logit AIC df 

# party 1706
logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_1706',
               'partycode' = 1706, 
               'full'      = partmod_lst_37[[mdls3]] %>% AIC,
               'null'      = nullmod_lst$logit[[mdls3]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 

# party 1702
logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_1702',
               'partycode' = 1702, 
               'full'      = partmod_lst_37[[mdls7]] %>% AIC,
               'null'      = nullmod_lst$logit[[mdls7]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 

# party 1707
logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_1707',
               'partycode' = 1707, 
               'full'      = partmod_lst_6[[mdls6]] %>% AIC,
               'null'      = nullmod_lst$logit[[mdls6]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 


# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))

