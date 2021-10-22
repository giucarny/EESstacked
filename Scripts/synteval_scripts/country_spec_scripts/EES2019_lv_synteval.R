# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Irish Sample) 
# Author: M.Körnig
# last update: 2021-10-21
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

cntry = 'LV'

EES2019_lv <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_lv <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_lv <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_lv_stack <- 
  cbind(EES2019_stckd_lv,  
        lapply(data = EES2019_stckd_lv, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_lv_stack %<>%
  cbind(.,
        lapply(data = EES2019_lv,
               cdbk = EES2019_cdbk_lv,
               stack = EES2019_lv_stack, 
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

csdf_lst <- list('std'  = EES2019_lv,
                 'cdbk' = EES2019_cdbk_lv,
                 'SDM'  = EES2019_lv_stack)


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

#large std. errors in Logit Model1,5,6,7: EDU_rec, constant
#large std. errors in Logit Model4: EDU_rec, D5_rec, constant

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


# One contingency table #Edu_rec
df <- regdf_lst$logit[[1]]
tab1EDU_rec <- table(df$stack_1611, df$EDU_rec) %>% as.data.frame()
names(tab1EDU_rec)[1:2] <- c('stack_1611', 'EDU_rec')
# Problem: only one voted for party 1611 (For Fatherland and Freedom) and has 15 years or less education
#same is true for model 4,5,6,7

# One contingency table #D5_rec
df <- regdf_lst$logit[[4]]
tab4D5_rec <- table(df$stack_1605, df$D5_rec) %>% as.data.frame()
names(tab4D5_rec)[1:2] <- c('stack_1605', 'D5_rec')


# All the contingency tables #only constant for model5 
tabs <- yxcontab.auxfun(regdf_lst$logit[[4]], contab = F)
# look at elements of tabs list
# lapply(tabs, head)


# Syntvars evaluation: partial logit models # ==========================================================

# Case 1: Only exclusing EDU_rec as variable # - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-EDU_rec)})

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

# lapply(anova_lst, head)
#For Model 1,3,4,5,6 H0 can not be rejected
#For Model 2 can reject at p<0.1 SN
#For Model 7 can reject at p<0 SN. This is troublesome since also model7 shows large std. errors for EDU_rec. 
#                                   Still excluding the variabel is not the "better" fit

stargazer::stargazer(partmod_lst, type = 'text',
                                            column.labels = as.character(relprty_df$Q7),
                                            dep.var.labels = 'Vote choice',
                                            star.cutoffs = c(0.05, 0.01, 0.001),
                                            omit.stat=c("f", "ser"),
                                            header = F,
                                            style = 'ajps')

#problem of high std. errors solved for all including Model7, but not for model4, however it decreased the constant std error


# Partial models fit summary for  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


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


# Case 2 excluding also other variables than EDU_rec # - - - - - - - - - - - - - - - - - - - - - - - - -
# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

regdf_lst_part2 <- 
  regdf_lst$logit %>% 
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c(EDU_rec, D5_rec))})

partmod_lst2 <- 
  lapply(regdf_lst_part2, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

anova_lst2 <- 
  anova.auxfun(mdl_lst1 = partmod_lst2,
               mdl_lst2 = fullmod_lst$logit,
               table = F)

# lapply(anova_lst2, head)
#For Model 1,2,3,4,5,6 H0 can not be rejected
#For Model 7 can be rejected at p<0.001

# stargazer::stargazer(partmod_lst2, type = 'text',
#                                             column.labels = as.character(relprty_df$Q7),
#                                             dep.var.labels = 'Vote choice',
#                                             star.cutoffs = c(0.05, 0.01, 0.001),
#                                             omit.stat=c("f", "ser"),
#                                             header = F,
#                                             style = 'ajps')

#problem of high std. errors solved for all models


# Partial models fit summary for  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


partlogit_df2 <-  
  tibble(
    'depvar'     = lapply(1:length(partmod_lst2), 
                          function(x){
                            names(regdf_lst_part2[[x]]) %>% .[2]
                          }) %>% unlist,
    'model'      = rep('partial2',length(regdf_lst_part2)),
    'Ps_Rsq'     = lapply(1:length(partmod_lst2),
                          function(x){
                            DescTools::PseudoR2(partmod_lst2[[x]], which = 'McFadden')
                          }) %>% unlist,
    'Adj_Ps_Rsq' = lapply(1:length(partmod_lst2),
                          function(x){
                            DescTools::PseudoR2(partmod_lst2[[x]], which = 'McFaddenAdj')
                          }) %>% unlist,
    'AIC'        = lapply(1:length(partmod_lst2),
                          function(x) {
                            partmod_lst2[[x]] %>% AIC
                          }) %>% unlist
  ) %>% 
  left_join(., relprty_df, by='depvar') %>% 
  dplyr::select(depvar, partycode, partyname_eng, model,
                Ps_Rsq, Adj_Ps_Rsq, AIC)


# Syntvars evaluation: New logit models fit stats # ====================================================

logit_df <-  
  fulllogit_df %>% 
  rbind(., partlogit_df) %>% 
  rbind(., partlogit_df2) %>%
  rbind(., nulllogit_df)



# Clean the environment # ==============================================================================

rm(list=ls(pattern='auxfun|regdf|partlogit|fulllogit|nulllogit'))


