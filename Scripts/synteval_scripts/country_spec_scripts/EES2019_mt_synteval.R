# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Maltese Sample) 
# Author: w.Haeussling
# last update: 2021-11-21
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Country-spec workflow # ==============================================================================

cntry = 'MT'

EES2019_mt <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_mt <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_mt <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_mt_stack <- 
  cbind(EES2019_stckd_mt,  
        lapply(data = EES2019_stckd_mt, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_mt_stack %<>%
  cbind(.,
        lapply(data = EES2019_mt,
               cdbk = EES2019_cdbk_mt,
               stack = EES2019_mt_stack, 
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

csdf_lst <- list('std'  = EES2019_mt,
                 'cdbk' = EES2019_cdbk_mt,
                 'SDM'  = EES2019_mt_stack)


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

# logit models 3, 4 and 5 show inflated SE on some predictors, more specifically: 
# Model 3: D8_rec, EDU_rec, D1_rec, D7_rec (only for category 2), D6_une
# Model 4: D8_rec, D7_rec (for category 1 and 2), D6_une
# Model 5: D8_rec, EDU_rec (only for category 3), D7_rec (only for category 2), D6_une

# Models 3, 4 and 5 constant terms are affected by the above mentioned variables' 
# inflated SE showing unusual values. We deal with model 3, 4 and 5 affected 
# by separation issue.



# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 3 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdl  <- 3
df   <- regdf_lst$logit[[mdl]]
cols <- c('D8_rec', 'EDU_rec', 'D1_rec', 'D7_rec', 'D6_une')

tabs3 <- lapply(data=df, y='stack_1903', na=T, X = cols, FUN = tab.auxfun)

# No respondents from rural areas, with low education, 
# with high subjective social status, members of trade unions, and unemployed did vote 
# for party 1903 (voted by only 6 respondents of the Maltese sample).

# Model 4 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdl  <- 4
df   <- regdf_lst$logit[[mdl]]
cols <- c('D8_rec', 'D7_rec', 'D6_une')

tabs4 <- lapply(data=df, y='stack_1904', na=T, X = cols, FUN = tab.auxfun)

# No respondents from rural areas, with NA in subjective social status and 
# NA in employment information did vote for party 1904 (voted by only 7 
# respondents of the Maltese sample).

# Model 5 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdl  <- 5
df   <- regdf_lst$logit[[mdl]]
cols <- c('D8_rec', 'EDU_rec', 'D7_rec', 'D6_une')

tabs5 <- lapply(data=df, y='stack_1905', na=T, X = cols, FUN = tab.auxfun)

# No respondents from rural areas, with high education or NA in education information, 
# with high subjective social status, members of trade unions, and unemployed 
# or NA in employment information did vote for party 1905 
# (voted by only 9 respondents of the Maltese sample).


# Syntvars evaluation: partial logit models # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 3
vrbls_2_drop3 <- c('D8_rec', 'EDU_rec', 'D1_rec', 'D7_rec', 'D6_une')

regdf_lst_part3 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop3)))
  }) 

partmod_lst3 <- 
  lapply(regdf_lst_part3, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(3)

anova_lst3 <- 
  anova.auxfun(mdl_lst1 = partmod_lst3[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)


# According to the LR test for Model 3 we reject H0 (p<0.05). 

#Model 4
vrbls_2_drop4 <-  c('D8_rec', 'D7_rec', 'D6_une')

regdf_lst_part4 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop4)))
  }) 

partmod_lst4 <- 
  lapply(regdf_lst_part4, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(4)

anova_lst4 <- 
  anova.auxfun(mdl_lst1 = partmod_lst4[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)

# According to the LR test for Model 4 we cannot reject H0 (p=0.0541).


#Model 5
vrbls_2_drop5 <- c('D8_rec', 'EDU_rec', 'D7_rec', 'D6_une')

regdf_lst_part5 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit() %>% dplyr::select(-c(all_of(vrbls_2_drop5)))
  }) 

partmod_lst5 <- 
  lapply(regdf_lst_part5, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(5)

anova_lst5 <- 
  anova.auxfun(mdl_lst1 = partmod_lst5[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = T)


# According to the LR test for Model 5 we cannot reject H0 (p=0.165).



# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 3
#partlogit_df3 <-  
#  tibble(
#    'depvar'     = lapply(1:length(partmod_lst3), 
#                          function(x){
#                            names(regdf_lst_part3[[x]]) %>% .[2]
#                          }) %>% unlist,
#    'model'      = rep('partial',length(regdf_lst_part3)),
#    'Ps_Rsq'     = lapply(1:length(partmod_lst3),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst3[[x]], which = 'McFadden')
#                          }) %>% unlist,
#    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst3),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst3[[x]], which = 'McFaddenAdj')
#                          }) %>% unlist,
#    'AIC'        = lapply(1:length(partmod_lst3),
#                          function(x) {
#                            partmod_lst3[[x]] %>% AIC
#                          }) %>% unlist
#  ) %>% 
#  left_join(., relprty_df, by='depvar') %>% 
#  dplyr::select(depvar, partycode, partyname_eng, model,
#                Ps_Rsq, Adj_Ps_Rsq, AIC)

#Model 4
#partlogit_df4 <-  
#  tibble(
#    'depvar'     = lapply(1:length(partmod_lst4), 
#                          function(x){
#                            names(regdf_lst_part4[[x]]) %>% .[2]
#                          }) %>% unlist,
#    'model'      = rep('partial',length(regdf_lst_part4)),
#    'Ps_Rsq'     = lapply(1:length(partmod_lst4),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst4[[x]], which = 'McFadden')
#                          }) %>% unlist,
#    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst4),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst4[[x]], which = 'McFaddenAdj')
#                          }) %>% unlist,
#    'AIC'        = lapply(1:length(partmod_lst4),
#                          function(x) {
#                            partmod_lst4[[x]] %>% AIC
#                          }) %>% unlist
#  ) %>% 
#  left_join(., relprty_df, by='depvar') %>% 
#  dplyr::select(depvar, partycode, partyname_eng, model,
#                Ps_Rsq, Adj_Ps_Rsq, AIC)

#Model 5
#partlogit_df5 <-  
#  tibble(
#    'depvar'     = lapply(1:length(partmod_lst5), 
#                          function(x){
#                            names(regdf_lst_part5[[x]]) %>% .[2]
#                          }) %>% unlist,
#    'model'      = rep('partial',length(regdf_lst_part5)),
#    'Ps_Rsq'     = lapply(1:length(partmod_lst5),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst5[[x]], which = 'McFadden')
#                          }) %>% unlist,
#    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst5),
#                          function(x){
#                            DescTools::PseudoR2(partmod_lst5[[x]], which = 'McFaddenAdj')
#                          }) %>% unlist,
#    'AIC'        = lapply(1:length(partmod_lst5),
#                          function(x) {
#                            partmod_lst5[[x]] %>% AIC
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

finalmod_lst[['logit']][[6]] <- finalmod_lst[['logit']][[5]]
finalmod_lst[['logit']][[5]] <- partmod_lst4[[4]] 

finalmod_lst[['logit']][[7]] <- partmod_lst5[[5]] 

# stargazer::stargazer(finalmod_lst$logit, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# Syntvars evaluation: Updating AIC data frames (logit only) # =========================================

# logit AIC df 

logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_1904',
               'partycode' = 1904, 
               'full'      = partmod_lst4[[4]] %>% AIC,
               'null'      = nullmod_lst$logit[[4]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 

logit_aic %<>% 
  rbind(.,
        tibble('depvar'    = 'stack_1905',
               'partycode' = 1905, 
               'full'      = partmod_lst5[[5]] %>% AIC,
               'null'      = nullmod_lst$logit[[5]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  .[order(.$depvar, .$partycode),] 




# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))

