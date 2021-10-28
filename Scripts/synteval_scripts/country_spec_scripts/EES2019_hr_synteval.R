# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Croatian Sample) 
# Author: M.Körnig
# last update: 2021-10-28
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Country-spec workflow # ==============================================================================

cntry = 'HR'

EES2019_hr <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_hr <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_hr <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_hr_stack <- 
  cbind(EES2019_stckd_hr,  
        lapply(data = EES2019_stckd_hr, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_hr_stack %<>%
  cbind(.,
        lapply(data = EES2019_hr,
               cdbk = EES2019_cdbk_hr,
               stack = EES2019_hr_stack, 
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

csdf_lst <- list('std'  = EES2019_hr,
                 'cdbk' = EES2019_cdbk_hr,
                 'SDM'  = EES2019_hr_stack)


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

stargazer::stargazer(fullmod_lst$logit, type = 'text',
                     column.labels = as.character(relprty_df$Q7),
                     dep.var.labels = 'Vote choice',
                     star.cutoffs = c(0.05, 0.01, 0.001),
                     omit.stat=c("f", "ser"),
                     header = F,
                     style = 'ajps')

#high std. errors in logit model4 Edu_rec, constant
#high std. errors in logit model5 constant only
#high std. errors in logit model6 Edu_rec, constant
#high std. errors in logit model7 Edu_rec, D8_rec, D5_rec, D7_rec


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
df <- regdf_lst$logit[[4]]
tab4EDU_rec <- table(df$stack_405, df$EDU_rec) %>% as.data.frame()
names(tab4EDU_rec)[1:2] <- c('stack_405', 'EDU_rec')
# Problem: no one voted for party 105 (Alliance for the Future of Austria) lives in a rural area

# One contingency table #EDU_rec
df <- regdf_lst$logit[[6]]
tab6EDU_rec <- table(df$stack_413, df$EDU_rec) %>% as.data.frame()
names(tab6EDU_rec)[1:2] <- c('stack_413', 'EDU_rec')



# All the contingency tables #only constant for model5 
tabs5 <- yxcontab.auxfun(regdf_lst$logit[[5]], contab = F)
# look at elements of tabs list
# lapply(tabs5, head)

# All the contingency tables #model7 - D8_rec, D5_rec, EDU_rec, D7_rec
tabs7 <- yxcontab.auxfun(regdf_lst$logit[[7]], contab = F)
# look at elements of tabs list
# lapply(tabs7, head)


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
#For Model 1,2,3,4,5,6,7 H0 can not be rejected

# stargazer::stargazer(partmod_lst, type = 'text',
#                                             column.labels = as.character(relprty_df$Q7),
#                                             dep.var.labels = 'Vote choice',
#                                             star.cutoffs = c(0.05, 0.01, 0.001),
#                                             omit.stat=c("f", "ser"),
#                                             header = F,
#                                             style = 'ajps')

#problem of high std. errors solved for model6, but not for model7, however it decreased around halve


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
  lapply(., function(x){ x %<>% na.omit() %>% dplyr::select(-c(EDU_rec, D8_rec, D5_rec, D7_rec))})

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
#For Model 7 can only be rejected at p<0.1

stargazer::stargazer(partmod_lst2, type = 'text',
                                            column.labels = as.character(relprty_df$Q7),
                                            dep.var.labels = 'Vote choice',
                                            star.cutoffs = c(0.05, 0.01, 0.001),
                                            omit.stat=c("f", "ser"),
                                            header = F,
                                            style = 'ajps')

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


