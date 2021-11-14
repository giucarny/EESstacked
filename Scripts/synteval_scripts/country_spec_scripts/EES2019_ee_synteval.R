# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Estonian Sample) 
# Author: w.Haeussling
# last update: 2021-10-25
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


# Syntvars evaluation: evaluating the source of misfit # ===============================================

#For the logit model 6 Edu_rec and the constant are affected.
#For the logit model 7 D5_rec, Edu_rec, D1_rec and the constant are affected 
#extremely (SE in the thousands) and the Variable D7_rec has a SE around 1.050.


# All the contingency tables # 
#Model 6: 
df6 <- regdf_lst$logit[[6]] 
tab6.1 <- table(df6$stack_906, df6$EDU_rec) %>% as.data.frame()
names(tab6.1)[1:2] <- c('stack_907', 'EDU_rec')

#Model 7: 
df7 <- regdf_lst$logit[[7]] 
tab7.1 <- table(df7$stack_907, df7$D5_rec) %>% as.data.frame()
tab7.2 <- table(df7$stack_907, df7$EDU_rec) %>%
  as.data.frame()
tab7.3 <- table(df7$stack_907, df7$D1_rec) %>% as.data.frame()
names(tab7.1)[1:2] <- c('stack_907', 'D5_rec')
names(tab7.2)[1:2] <- c('stack_907', 'EDU_rec')
names(tab7.3)[1:2] <- c('stack_907', 'D1_rec')


#Huge SE in above mentioned variables of models 6 and 7 most 
#likely due to empty or nearly empty cells observable in the contingency tables.

# Syntvars evaluation: partial logit models # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 6 
regdf_lst_part6 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c(EDU_rec))})

partmod_lst6 <- 
  lapply(regdf_lst_part6, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })


#Model 7
regdf_lst_part7 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c(D5_rec, EDU_rec, D1_rec))})

partmod_lst7 <- 
  lapply(regdf_lst_part7, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })


# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

#Model 6
#LR test:
partmod_loglik <- partmod_lst6[[6]] %>% logLik() %>% as.numeric # k = 9
fullmod_loglik <- fullmod_lst$logit[[6]] %>% logLik() %>% as.numeric # k = 11
degfre         <- 11-9

LR = 2*(fullmod_loglik-partmod_loglik)
p <- pchisq(LR, df=degfre, lower.tail = F)
# p<0.1 
# We can only reject the null hypothesis (partial model fits better than full model) 
# for 0.05<p<0.1. 
#However H0 is only rejected at p<0.05.
#Hence the constrained model fits significantly better than the full one. 
# In this case we implement the partial model. 

#Anova test:
anova_lst6 <- 
  anova.auxfun(mdl_lst1 = partmod_lst6,
               mdl_lst2 = fullmod_lst$logit,
               table = F)
#p<.1
#Model 6: The null hypothesis (the full/unconstrained model fits better than 
#the  partial/constrained model) cannot be rejected for models 1, 2, 3, 4, 5 and 7 at p<0.05
#(p at least 0.3001 or higher). 
#For model 6 the H0 can only be rejected at 0.05<p<0.1.
#However H0 is only rejected at p<0.05.
#Thus the unconstrained model fits significantly better than the constrained model for all
#models except model 6. 
# In this case we implement the partial model for all models 1 to 7. 


#Model 7 
#LR test:
partmod_loglik <- partmod_lst7[[7]] %>% logLik() %>% as.numeric # k = 8
fullmod_loglik <- fullmod_lst$logit[[7]] %>% logLik() %>% as.numeric # k = 11
degfre         <- 11-8

LR = 2*(fullmod_loglik-partmod_loglik)
p <- pchisq(LR, df=degfre, lower.tail = F)
# p<.05 
# We reject the null hypothesis (partial model fits better than full model) 
# since p<0.05. Hence the unconstrained model fits significantly better than the full one. 
# In this we case we do not implement the partial model. 

#Anova test:
anova_lst7 <- 
  anova.auxfun(mdl_lst1 = partmod_lst7,
               mdl_lst2 = fullmod_lst$logit,
               table = F)
#p<.05
#Model 7: The null hypothesis (the partial/constrained model fits better than 
#the full/unconstrained model) cannot be rejected for models 1, 2, 3, 4, 5 and 6 at p<0.05
#(p at least 0.236 or higher). 
#For model 7 the H0 can be rejected at p<0.05.
#Dropping the problematic variables in all Models thus represents a rather 
#disruptive solution. Therefore the constrained model will not be implemented.


#check model 7 against model 6 constrained model

#LR test:
partmod_loglik <- partmod_lst6[[7]] %>% logLik() %>% as.numeric # k = 9
fullmod_loglik <- fullmod_lst$logit[[7]] %>% logLik() %>% as.numeric # k = 11
degfre         <- 11-9

LR = 2*(fullmod_loglik-partmod_loglik)
p <- pchisq(LR, df=degfre, lower.tail = F)
# p>0.3
# We can not reject the null hypothesis (partial model fits better than full model) 
# since p>0.3. Hence the constrained model 6 fits significantly better than the full one. 
# In this we case we implement the partial model. 

#Anova test:
#see anova test in "Model 6:"


# Partial models fit summary # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Model 6 
partlogit_df6 <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst6), 
                          function(x){
                            names(regdf_lst_part6[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial',length(regdf_lst_part6)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst6),
                          function(x){
                            DescTools::PseudoR2(partmod_lst6[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst6),
                          function(x){
                            DescTools::PseudoR2(partmod_lst6[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst6),
                          function(x) {
                            partmod_lst6[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


#Model 7 
partlogit_df7 <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst7), 
                          function(x){
                            names(regdf_lst_part7[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial',length(regdf_lst_part7)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst7),
                          function(x){
                            DescTools::PseudoR2(partmod_lst7[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst7),
                          function(x){
                            DescTools::PseudoR2(partmod_lst7[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst7),
                          function(x) {
                            partmod_lst7[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)



# Syntvars evaluation: New logit models fit stats # ====================================================

logit_df <-  
  fulllogit_df %>% 
  rbind(., partlogit_df7) %>% 
  rbind(., nulllogit_df)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))

