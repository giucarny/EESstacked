# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Latvian Sample) 
# Author: M.Körnig
# last update: 2021-11-06
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

source(here('Scripts', 'synteval', 'Synteval_auxfuns.R'))

# Country-specific data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

csdf_lst <- list('std'  = EES2019_lv,
                 'cdbk' = EES2019_cdbk_lv,
                 'SDM'  = EES2019_lv_stack)


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

#large SE in M1,7: EDU_rec, D6_une, constant
#large SE in M5,6: EDU_rec, constant
#large SE in M2: D6_une without constant
#large SE in M4: D5_rec, constant

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

# logit models 1,2,4,5,6,7 show inflated SE on some predictors, more specifically: 
# Model 1,7: EDU_rec, D6_une
# Model 5,6: EDU_rec
# Model 2: D6_une
# Model 4: D5_rec

# Models 1,4,5,6,7 constant is affected showing unusual values, but not Model 2. 
# We deal with models 6,7 affected by separation issue.


# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 1 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 1
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec', 'D6_une')

tabs <- lapply(data=df, y='stack_1611', na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs, head)
# table(df$stack_1611)

# One respondent with 15 years or less education and one unemployed did vote 
# for party 1611 (voted by 86 respondents of the Latvian sample).

# Model 2 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 2
df   <- regdf_lst$logit[[mdl]]
cols <- 'D6_une'

tabs2 <- lapply(data=df, y='stack_1608', na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs2, head)
# table(df$stack_1608)

# No respondent in unemployement did vote
# for party 1608 (voted by only 44 respondents of the Latvian sample).

# Model 4 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 4
df   <- regdf_lst$logit[[mdl]]
cols <- 'D5_rec'

tabs3 <- lapply(data=df, y='stack_1605' , na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs3, head)
# table(df$stack_1605)

# Only one respondent in employement did vote
# for party 1605 (voted by only 8 respondents of the Latvian sample).

# Model 5 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 5
df   <- regdf_lst$logit[[mdl]]
cols <- 'EDU_rec'

tabs4 <- lapply(data=df, y='stack_1610' , na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs4, head)
# table(df$stack_1610)

# Only one respondent with 15 years or less education did vote
# for party 1610 (voted by 87 respondents of the Latvian sample).

# Model 6 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 6
df   <- regdf_lst$logit[[mdl]]
cols <- 'EDU_rec'

tabs5 <- lapply(data=df, y='stack_1604' , na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs5, head)
# table(df$stack_1604)

# No respondent with 15 years or less education did vote
# for party 1604 (voted by 40 respondents of the Latvian sample).

# Model 7 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
mdl  <- 7
df   <- regdf_lst$logit[[mdl]]
cols <- c('EDU_rec', 'D6_une')

tabs6 <- lapply(data=df, y='stack_1616', na=T, X = cols, FUN = tab.auxfun)
# lapply(tabs6, head)
# table(df$stack_1616)

# One respondent with 15 years or less education and three unemployed did vote 
# for party 1616 (voted 135 respondents of the Latvian sample).


# Syntvars evaluation: partial logit models # ==========================================================

# Get the df for and estimate the partial models # - - - - - - - - - - - - - - - - - - - - - - - - - - -

vrbls_2_drop <- 'EDU_rec' #for model 5,6
vrbls_2_drop2 <- c('EDU_rec', 'D6_une') #for model 1,7
vrbls_2_drop3 <- 'D5_rec' #for model 4
vrbls_2_drop4 <- 'D6_une' #for model 2

regdf_lst_part <- 
  regdf_lst$logit %>% 
  lapply(., function(x){
    x %<>% na.omit()
  }) 

regdf_lst_part[[1]] <- regdf_lst_part[[1]][,!(names(regdf_lst_part[[1]]) %in% vrbls_2_drop)]
regdf_lst_part[[8]] <- regdf_lst_part[[1]][,!(names(regdf_lst_part[[1]]) %in% vrbls_2_drop2)]
regdf_lst_part[[2]] <- regdf_lst_part[[2]][,!(names(regdf_lst_part[[2]]) %in% vrbls_2_drop4)]
regdf_lst_part[[4]] <- regdf_lst_part[[4]][,!(names(regdf_lst_part[[4]]) %in% vrbls_2_drop3)]
regdf_lst_part[[5]] <- regdf_lst_part[[5]][,!(names(regdf_lst_part[[5]]) %in% vrbls_2_drop)]
regdf_lst_part[[6]] <- regdf_lst_part[[6]][,!(names(regdf_lst_part[[6]]) %in% vrbls_2_drop)]
regdf_lst_part[[7]] <- regdf_lst_part[[7]][,!(names(regdf_lst_part[[7]]) %in% vrbls_2_drop)]


partmod_lst <- 
  lapply(regdf_lst_part, function(x){
    y    <- names(x)[startsWith(names(x), 'stack')]
    xs   <- names(x)[3:length(x)]
    frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
    
    fit <- glm(data = x, formula = frml, family = binomial)
    
    return(fit)
  })

# LR test (Chisq) # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

mdls <- c(1,2,4,5,6,7)

anova_lst <- 
  anova.auxfun(mdl_lst1 = partmod_lst[c(mdls)],
               mdl_lst2 = fullmod_lst$logit[c(mdls)],
               table = F)

anova_lst2 <- 
  anova.auxfun(mdl_lst1 = partmod_lst[c(8)],
               mdl_lst2 = fullmod_lst$logit[c(1)],
               table = F)


# lapply(anova_lst, head)
# lapply(anova_lst2, head)

# For Model 1 H0 can be rejected at p<0.05  (H0 can not be rejected if only EDU_rec is dropped instead of both EDU_rec & D6_une, no high SE constant)
# We see that not dropping D6_une does not distort the SE of the constant, thus we like to drop as less variables as possible and include 
# the partial model with only EDU_rec dropped for model 1.
# For Model 2 H0 can be rejected at p<0.1
# For Model 4 H0 can be rejected at p<0.05
# For Model 5 H0 can be rejected at p<0.1
# For Model 6 H0 can be rejected at p<0.05
# For Model 7 H0 can be rejected at p<0.001 (H0 can be rejected at p<0.01 if only D6_une is dropped instead of both EDU_rec & D6_une, however than still high SE in constant)

# stargazer::stargazer(partmod_lst, type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')


# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

finalmod_lst <- list()
finalmod_lst[['OLS']] <- fullmod_lst[['OLS']]
finalmod_lst[['logit']] <- fullmod_lst[['logit']]

finalmod_lst[['logit']][[8]] <- finalmod_lst[['logit']][[6]]
finalmod_lst[['logit']][[9]] <- partmod_lst[[6]]
finalmod_lst[['logit']][[10]] <- finalmod_lst[['logit']][[7]]
finalmod_lst[['logit']][[5]] <- finalmod_lst[['logit']][[4]]
finalmod_lst[['logit']][[6]] <- finalmod_lst[['logit']][[5]]
finalmod_lst[['logit']][[7]] <- partmod_lst[[5]]
finalmod_lst[['logit']][[4]] <- finalmod_lst[['logit']][[3]]
finalmod_lst[['logit']][[3]] <- finalmod_lst[['logit']][[2]]
finalmod_lst[['logit']][[2]] <- partmod_lst[[1]]


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
        tibble('depvar'    = 'stack_1611',
               'partycode' = 1611, 
               'full'      = partmod_lst[[1]] %>% AIC,
               'null'      = nullmod_lst$logit[[1]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>% 
  rbind(.,
        tibble('depvar'    = 'stack_1610',
               'partycode' = 1610, 
               'full'      = partmod_lst[[5]] %>% AIC,
               'null'      = nullmod_lst$logit[[5]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>%
  rbind(.,
        tibble('depvar'    = 'stack_1604',
               'partycode' = 1604, 
               'full'      = partmod_lst[[6]] %>% AIC,
               'null'      = nullmod_lst$logit[[6]] %>% AIC,
        ) %>% 
          mutate(diff = full-null)) %>%
  .[order(.$depvar, .$partycode),] 

