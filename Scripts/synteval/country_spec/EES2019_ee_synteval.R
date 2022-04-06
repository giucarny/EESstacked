# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Estonian Sample) 
# Author: w.Haeussling
# last update: 2021-11-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Country-spec workflow # ==============================================================================

cntry = 'EE'

EES2019_ee <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_ee <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_ee <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_ee_stack <- 
  cbind(EES2019_stckd_ee,  
        lapply(data = EES2019_stckd_ee, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_ee_stack %<>%
  cbind(.,
        lapply(data = EES2019_ee,
               cdbk = EES2019_cdbk_ee,
               stack = EES2019_ee_stack, 
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

csdf_lst <- list('std'  = EES2019_ee,
                 'cdbk' = EES2019_cdbk_ee,
                 'SDM'  = EES2019_ee_stack)


# Synthetic variables estimation variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

syntvars_vrbls <- list('dep'   = list('OLS'     = 'Q10_gen', 
                                      'logit'   = 'Q7_gen'),
                       'indep' = list('ctgrcl' = c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 
                                                   'D1_rec', 'D7_rec','D6_une'),
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

# logit models 2, 6 and 7 show inflated SE on some predictors, more specifically: 
# Model 2: D6_une
# Model 6: Edu_rec
# Model 7: D5_rec, EDU_rec, D1_rec, D6_une

# Model 2 constant term is not affected by D6_une inflated SE, whereas 
# Model 6 and 7  constant SE is affected showing unusual values. 
# We deal only with model 6 and 7 affected by separation issue.


# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 6 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdl  <- 6
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec')

tabs6 <- lapply(data=df, y='stack_906', na=T, X = cols, FUN = tab.auxfun)

# No respondents with low education did vote 
# for party 906 (voted by only 27 respondents of the Estonian sample and 17 NA).

# Model 7 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdl  <- 7
df   <- regdf_lst$logit[[mdl]]
cols <- c('D5_rec', 'EDU_rec', 'D1_rec', 'D6_une')

tabs7 <- lapply(data=df, y='stack_907', na=T, X = cols, FUN = tab.auxfun)

# No respondents with low education, with high subjective social status, 
# no members of trade unions, and unemployed and only very few respondents married 
# or in partnership as well as married or in a partnership (2 and 9) did vote 
# for party 907 (voted by only 11 respondents of the Cypriot sample).

# Since D5_rec does not have a 0 in its cross tabulation I will develop partial 
# regression models with and without D5_rec and check for SE mistakes and fit against
# eachother and the full regression model.

# Syntvars evaluation: partial logit models # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 6
vrbls_2_drop6 <- c('EDU_rec')

regdf_lst_part6 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop6)))
  }) 

partmod_lst6 <- 
  lapply(regdf_lst_part6, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(6)

anova_lst6 <- 
  anova.auxfun(mdl_lst1 = partmod_lst6[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

# According to the LR test for Model 6 we cannot reject H0 (only p<0.1).


#Model 7 
# partial model without D5_rec
vrbls_2_drop7_1 <- c('D5_rec', 'EDU_rec', 'D1_rec', 'D6_une')

regdf_lst_part7_1 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop7_1)))
  }) 

partmod_lst7_1 <- 
  lapply(regdf_lst_part7_1, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(7)

anova_lst7_1 <- 
  anova.auxfun(mdl_lst1 = partmod_lst7_1[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

# According to the LR test for Model 7 we cannot reject H0 (only p<0.1).

# partial model with D5_rec
vrbls_2_drop7_2 <- c('EDU_rec', 'D1_rec', 'D6_une')

regdf_lst_part7_2 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop7_2)))
  }) 

partmod_lst7_2 <- 
  lapply(regdf_lst_part7_2, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(7)

anova_lst7_2 <- 
  anova.auxfun(mdl_lst1 = partmod_lst7_2[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)
# According to the LR test for Model 7 we cannot reject H0 (only p=0.352).

# Partial Model 7 with D5_rec against without D5_rec
# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(7)

anova_lst7_1vs2 <- 
  anova.auxfun(mdl_lst1 = partmod_lst7_1[c(mdls)],
               mdl_lst2 = partmod_lst7_2[c(mdls)],
               table = T)

# According to the LR test for partial model 7 without D5_rec against 
# partial model 7 with D5_rec H0 is rejected (p<0.05). Hence the partial model 
# with D5_rec has a better fit than the one without D5_rec.  


# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 6 
#partlogit_df6 <-  
#  tibble(
#    'depvar'     = lapply(1:length(partmod_lst6), 
#                          function(x){
#                            names(regdf_lst_part6[[x]]) %>% .[2]
#                          }) %>% unlist,
#    'model'      = rep('partial',length(regdf_lst_part6)),
#    'Ps_Rsq'     = lapply(1:length(partmod_lst6),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst6[[x]], which = 'McFadden')
#                          }) %>% unlist,
#    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst6),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst6[[x]], which = 'McFaddenAdj')
#                          }) %>% unlist,
#    'AIC'        = lapply(1:length(partmod_lst6),
#                          function(x) {
#                            partmod_lst6[[x]] %>% AIC
#                          }) %>% unlist
#  ) %>% 
#  left_join(., relprty_df, by='depvar') %>% 
#  dplyr::select(depvar, partycode, partyname_eng, model,
#                Ps_Rsq, Adj_Ps_Rsq, AIC)


#Model 7 
#partlogit_df7 <-  
#  tibble(
#    'depvar'     = lapply(1:length(partmod_lst7), 
#                          function(x){
#                            names(regdf_lst_part7[[x]]) %>% .[2]
#                          }) %>% unlist,
#    'model'      = rep('partial',length(regdf_lst_part7)),
#    'Ps_Rsq'     = lapply(1:length(partmod_lst7),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst7[[x]], which = 'McFadden')
#                          }) %>% unlist,
#    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst7),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst7[[x]], which = 'McFaddenAdj')
#                          }) %>% unlist,
#    'AIC'        = lapply(1:length(partmod_lst7),
#                          function(x) {
#                            partmod_lst7[[x]] %>% AIC
#                          }) %>% unlist
#  ) %>% 
#  left_join(., relprty_df, by='depvar') %>% 
#  dplyr::select(depvar, partycode, partyname_eng, model,
#                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

finalmod_lst <- list()
finalmod_lst[['OLS']] <- fullmod_lst[['OLS']]
finalmod_lst[['logit']] <- fullmod_lst[['logit']]

finalmod_lst[['logit']][[8]] <- finalmod_lst[['logit']][[7]]
finalmod_lst[['logit']][[7]] <- partmod_lst6[[6]] 

finalmod_lst[['logit']][[9]] <- partmod_lst7_2[[7]]



# Syntvars evaluation: Updating AIC data frames (logit only) # =========================================

# logit AIC df 

# Party 906
logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_906',
               'partycode' = 906, 
               'full'      = partmod_lst6[[6]] %>% AIC,
               'null'      = nullmod_lst$logit[[6]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 

#Party 907
logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_907',
               'partycode' = 907, 
               'full'      = partmod_lst7_1[[7]] %>% AIC,
               'null'      = nullmod_lst$logit[[7]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 

# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))

