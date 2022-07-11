# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Evaluating Synthetic Variables Estimation (EES 2019 Voter Study, Belgian Sample) 
# Author: G.Carteny
# last update: 2022-07-10
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# # Source the general workflow # ========================================================================
# 
# source(here('Scripts', 'synteval', 'Synteval_gen.R'))


# Country-spec workflow # ==============================================================================

# N.B: The Belgian sample is splitted according to the two electoral colleges of Belgium, namely the 
# Dutch and the French electoral college. 

# Belgian electoral colleges # - - -

el_coll_be <- list('DU-el', 'FR-el')


cntry = 'BE'

EES2019_be <- 
  EES2019 %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(meta_lang_be  == 1 ~ 'DU-el',
                                meta_lang_be  == 2 ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

EES2019_stckd_be <- 
  EES2019_stckd %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(meta_lang_be  == 1 ~ 'DU-el',
                                meta_lang_be  == 2 ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

EES2019_cdbk_be <-
  EES2019_cdbk %>% 
  filter(countryshort==cntry) %>% 
  mutate(el_coll_be = case_when(Region  == 'Flanders' ~ 'DU-el',
                                Region  == 'Wallonia' ~ 'FR-el')) %>%  
  split(.$el_coll_be) 

rm(cntry)

# Generic dichotomous variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

EES2019_be <-  
  lapply(EES2019_be, 
         function(df) {df %<>% mutate(Q25_rec = case_when(is.na(Q25_rec) ~ as.integer(90), T ~ Q25_rec))})

EES2019_stckd_be <- 
  lapply(EES2019_stckd_be, 
         function(df) {df %<>% mutate(Q25_rec = case_when(is.na(Q25_rec) ~ as.integer(90), T ~ Q25_rec))})


EES2019_be_stack <- 
  lapply(EES2019_stckd_be, 
         function(df) {
           df_stack <- 
             cbind(df,  
                   lapply(data = df, 
                          X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
                          stack_var = 'party',
                          FUN = gendic.fun) %>% 
                     do.call('cbind',.)) %>% 
             as_tibble()
         })

# Generic distance/proximity variables estimation # - - - - - - - - - - - - - - - - - - - - - - - - - - 

EES2019_be_stack <- 
  lapply(el_coll_be, 
         function(x) {
           df_stack <- 
             cbind(EES2019_be_stack[[x]],  
                   lapply(data = EES2019_be[[x]],
                          cdbk = EES2019_cdbk_be[[x]],
                          stack = EES2019_be_stack[[x]],
                          crit = 'average',
                          rescale = T,
                          check = F,
                          keep_id = F,
                          X = list('Q10','Q11','Q23'),
                          FUN = gendis.fun) %>% 
                     do.call('cbind',.)) %>% 
             as_tibble()
         })

names(EES2019_be_stack) <- unlist(el_coll_be)

# Syntvars evaluation: Functions, variables and data frames # ==========================================

# Source auxiliary functions # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

source(here('Scripts', 'synteval', 'Synteval_auxfuns.R'))

# Country-specific data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

csdf_lst <- 
  lapply(el_coll_be,
         function(x) {
           list('std'  = EES2019_be[[x]],
                'cdbk' = EES2019_cdbk_be[[x]],
                'SDM'  = EES2019_be_stack[[x]])
         })

# csdf_lst$`DU-el`$cdbk %<>% arrange(Q7)

names(csdf_lst)  <- unlist(el_coll_be)


# Synthetic variables estimation variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

syntvars_vrbls <- list('dep'   = list('OLS'     = 'Q10_gen', 
                                      'logit'   = 'Q7_gen'),
                       'indep' = list('ctgrcl' = c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec', 
                                                   'D1_rec', 'D7_rec'),
                                      'cntns'  =  c('D4_age', 'D10_rec')))

# Synthetic variables estimation data frames # - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


regdf_lst  <- 
  lapply(el_coll_be,
         function(x){
           list('OLS'   = regdf.auxfun(data        = csdf_lst[[x]]$SDM,
                                       depvar      = syntvars_vrbls$dep$OLS,
                                       cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                       cont.indvar = syntvars_vrbls$indep$cntns),
                'logit' = regdf.auxfun(data        = csdf_lst[[x]]$SDM,
                                       depvar      = syntvars_vrbls$dep$logit,
                                       cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                       cont.indvar = syntvars_vrbls$indep$cntns))
         })
  
names(regdf_lst)  <- unlist(el_coll_be)


# Relevant parties data frame # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

relprty_df <- 
  lapply(el_coll_be,
         function(y){
           tibble('depvar'    = 
                    lapply(1:length(regdf_lst[[y]]$OLS), function(x){names(regdf_lst[[y]]$OLS[[x]]) %>% 
                        .[2]}) %>% unlist,
                  'partycode' =
                    lapply(1:length(regdf_lst[[y]]$OLS), function(x){names(regdf_lst[[y]]$OLS[[x]]) %>%
                        .[2]}) %>% unlist %>% 
                    gsub('stack_','',.) %>% 
                    as.numeric)  
         })

names(relprty_df) <- unlist(el_coll_be)

relprty_df <- 
  lapply(el_coll_be,
       function(y){
         relprty_df[[y]] %<>% 
           mutate('partyname_eng' = 
                    csdf_lst[[y]]$cdbk %>% 
                    dplyr::select(partyname_eng, Q7) %>% 
                    filter(Q7 %in% relprty_df[[y]][['partycode']]) %>% 
                    .[['partyname_eng']],
                  'elcoll' = y)
       })

relprty_df %<>% do.call('rbind',.)



# Syntvars evaluation: Null and full regression models # ===============================================

set.seed(123)

fullmod_lst <- 
  lapply(el_coll_be, 
         function(x){
           list('OLS'   = gensyn.fun(data        = csdf_lst[[x]]$SDM,
                                     depvar      = syntvars_vrbls$dep$OLS,
                                     cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                     cont.indvar = syntvars_vrbls$indep$cntns,
                                     yhat.name   = 'socdem_synt',
                                     regsum      = T),
                'logit' = gensyn.fun(data        = csdf_lst[[x]]$SDM,
                                     depvar      = syntvars_vrbls$dep$logit,
                                     cat.indvar  = syntvars_vrbls$indep$ctgrcl, 
                                     cont.indvar = syntvars_vrbls$indep$cntns,
                                     yhat.name   = 'socdem_synt',
                                     regsum      = T))
         })

names(fullmod_lst) <- unlist(el_coll_be)

nullmod_lst <- list( 'DU-el' =
                       list('OLS'   = lapply(X = regdf_lst[['DU-el']]$OLS,   regmod = 'OLS',   null_mod.auxfun),
                            'logit' = lapply(X = regdf_lst[['DU-el']]$logit, regmod = 'logit', null_mod.auxfun)),
                     'FR-el' = 
                       list('OLS'   = lapply(X = regdf_lst[['FR-el']]$OLS,   regmod = 'OLS',   null_mod.auxfun),
                            'logit' = lapply(X = regdf_lst[['FR-el']]$logit, regmod = 'logit', null_mod.auxfun))
                   )

# fullmod_lst[[1]]$logit %>% lapply(.,summary)
# fullmod_lst[[2]]$logit %>% lapply(.,summary)

# Syntvars evaluation: OLS models summary # ============================================================

# stargazer::stargazer(fullmod_lst[[1]]$OLS,
#                      title = 'Dutch Electoral College',
#                      type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'PTV',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# stargazer::stargazer(fullmod_lst[[2]]$OLS,
#                      title = 'French Electoral College',
#                      type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'PTV',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# Syntvars evaluation: logit models summary # ==========================================================

# stargazer::stargazer(fullmod_lst[[1]]$logit,
#                      title = 'Dutch Electoral College',
#                      type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')

# stargazer::stargazer(fullmod_lst[[2]]$logit,
#                      title = 'French Electoral College',
#                      type = 'text',
#                      column.labels = as.character(relprty_df$Q7),
#                      dep.var.labels = 'Vote choice',
#                      star.cutoffs = c(0.05, 0.01, 0.001),
#                      omit.stat=c("f", "ser"),
#                      header = F,
#                      style = 'ajps')


# Syntvars evaluation: OLS models fit stats # ==========================================================

ols_df <- lapply(el_coll_be,
                 function(y) {
                   tibble(
                     'depvar'  = lapply(1:length(regdf_lst[[y]]$OLS), 
                                        function(x){
                                          names(regdf_lst[[y]]$OLS[[x]]) %>% .[2]
                                        }) %>% unlist,
                     'model'   = rep('full',length(regdf_lst[[y]]$OLS)),
                     'Rsq'     = lapply(1:length(fullmod_lst[[y]]$OLS),
                                        function(x) {
                                          fullmod_lst[[y]]$OLS[[x]] %>% summary %>% .$r.squared %>% round(., 3)
                                        }) %>% unlist,
                     'Adj_Rsq' = lapply(1:length(fullmod_lst[[y]]$OLS),
                                        function(x) {
                                          fullmod_lst[[y]]$OLS[[x]] %>% summary %>% .$adj.r.squared %>% round(., 3)
                                        }) %>% unlist,
                     'AIC'     = lapply(1:length(fullmod_lst[[y]]$OLS),
                                        function(x) {
                                          fullmod_lst[[y]]$OLS[[x]] %>% AIC
                                        }) %>% unlist) %>% 
                     rbind(.,
                           tibble(
                             'depvar'  = lapply(1:length(regdf_lst[[y]]$OLS), 
                                                function(x){
                                                  names(regdf_lst[[y]]$OLS[[x]]) %>% .[2]
                                                }) %>% unlist,
                             'model'   = rep('null',length(regdf_lst[[y]]$OLS)),
                             'Rsq'     = lapply(1:length(nullmod_lst[[y]]$OLS),
                                                function(x) {
                                                  nullmod_lst[[y]]$OLS[[x]] %>% summary %>% .$r.squared %>% round(., 3)
                                                }) %>% unlist,
                             'Adj_Rsq' = lapply(1:length(nullmod_lst[[y]]$OLS),
                                                function(x) {
                                                  nullmod_lst[[y]]$OLS[[x]] %>% summary %>% .$adj.r.squared %>% round(., 3)
                                                }) %>% unlist,
                             'AIC'     = lapply(1:length(fullmod_lst[[y]]$OLS),
                                                function(x) {
                                                  nullmod_lst[[y]]$OLS[[x]] %>% AIC
                                                }) %>% unlist))
                   
                 })

names(ols_df) <- c('DU-el', 'FR-el')
  
ols_df %<>%
  lapply(. ,
         function(x) {
           left_join(x, relprty_df, by='depvar', suffix = c("", ".y")) %>% 
             dplyr::select(depvar, partycode, partyname_eng, model,
                           Rsq, Adj_Rsq, AIC, -elcoll)
         })

# Syntvars evaluation: logit models fit stats # ========================================================


logit_df <- lapply(el_coll_be,
                   function(y) {
                     tibble(
                       'depvar'     = lapply(1:length(regdf_lst[[y]]$logit), 
                                             function(x){
                                               names(regdf_lst[[y]]$OLS[[x]]) %>% .[2]
                                             }) %>% unlist,
                       'model'      = rep('full',length(regdf_lst[[y]]$logit)),
                       'Ps_Rsq'     = lapply(1:length(fullmod_lst[[y]]$logit),
                                             function(x){
                                               DescTools::PseudoR2(fullmod_lst[[y]]$logit[[x]], which = 'McFadden')
                                             }) %>% unlist,
                       'Adj_Ps_Rsq' = lapply(1:length(fullmod_lst[[y]]$logit),
                                             function(x){
                                               DescTools::PseudoR2(fullmod_lst[[y]]$logit[[x]], which = 'McFaddenAdj')
                                             }) %>% unlist,
                       'AIC'        = lapply(1:length(fullmod_lst[[y]]$logit),
                                             function(x) {
                                               fullmod_lst[[y]]$logit[[x]] %>% AIC
                                             }) %>% unlist
                     ) %>% 
                       rbind(.,
                             tibble(
                               'depvar'     = lapply(1:length(regdf_lst[[y]]$logit), 
                                                     function(x){
                                                       names(regdf_lst[[y]]$OLS[[x]]) %>% .[2]
                                                     }) %>% unlist,
                               'model'      = rep('null',length(regdf_lst[[y]]$logit)),
                               'Ps_Rsq'     = lapply(1:length(nullmod_lst[[y]]$logit),
                                                     function(x){
                                                       DescTools::PseudoR2(nullmod_lst[[y]]$logit[[x]], which = 'McFadden')
                                                     }) %>% unlist,
                               'Adj_Ps_Rsq' = lapply(1:length(nullmod_lst[[y]]$logit),
                                                     function(x){
                                                       DescTools::PseudoR2(nullmod_lst[[y]]$logit[[x]], which = 'McFaddenAdj')
                                                     }) %>% unlist,
                               'AIC'        = lapply(1:length(fullmod_lst[[y]]$logit),
                                                     function(x) {
                                                       nullmod_lst[[y]]$logit[[x]] %>% AIC
                                                     }) %>% unlist
                             ))
                   })
                      
names(logit_df) <- c('DU-el', 'FR-el')

logit_df %<>%
  lapply(. ,
         function(x) {
           left_join(x , relprty_df, by='depvar') %>% 
           dplyr::select(depvar, partycode, partyname_eng, model,
                         Ps_Rsq, Adj_Ps_Rsq, AIC)
         })


# AIC data frames # ====================================================================================

# OLS AIC df 

ols_aic <- 
  lapply(el_coll_be,
         function(x) {
            ols_df[[x]] %>%
            pivot_wider(id_cols = c('depvar', 'partycode', 'partyname_eng'), values_from = 'AIC',
                        names_from = 'model') %>%
            mutate(diff = full - null) %>%
            mutate(across(c('full', 'null', 'diff'), ~round(.,3))) %>%
            dplyr::select(-c(partyname_eng))
         })

names(ols_aic) <- c('DU-el', 'FR-el')

# Logit AIC df 

logit_aic <- 
  lapply(el_coll_be,
         function(x) {
            logit_df[[x]] %>%
            pivot_wider(id_cols = c('depvar', 'partycode', 'partyname_eng'), values_from = 'AIC',
                        names_from = 'model') %>%
            mutate(diff = full - null) %>%
            mutate(across(c('full', 'null', 'diff'), ~round(.,3))) %>%
            dplyr::select(-c(partyname_eng))
         })

names(logit_aic) <- c('DU-el', 'FR-el')


# Full models evaluation # =============================================================================

# logit model 5 for the French electoral college shows inflated SE for the following variables
# D8_rec
# EDU_rec
# D7_rec

# Syntvars evaluation: evaluating the source of misfit # ===============================================

# Model 5
mdl  <- 5
df   <- regdf_lst$`FR-el`$logit[[mdl]]
cols <- c('D8_rec', 'EDU_rec', 'D7_rec')

tabs <- lapply(data=df, y='stack_212', na=T, X = cols, FUN = tab.auxfun)
lapply(tabs, head)

#tabs <- (, contab = F)[[2]]
# No respondents living in rural areas, of high social class, and highly educated voted for party 212



# Syntvars evaluation: partial logit models # ==========================================================

# In order to avoid repetitions I creat an ad hoc workflow - - - - - - - - - - - - - - - - - - - - - - -

# Model 5 # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
el_coll <- 'FR-el'
mdl <- 5
x <- regdf_lst[[el_coll]]$logit[[mdl]] %>% na.omit %>% dplyr::select(-c(D7_rec, D8_rec, EDU_rec))
y    <- names(x)[startsWith(names(x), 'stack')]
xs   <- names(x)[3:length(x)]
frml <- paste(y, paste0(xs, collapse = ' + '), sep = " ~ ") %>% as.formula
part_fit <- glm(data = x, formula = frml, family = binomial)

anova_lst <- anova(part_fit, fullmod_lst[[el_coll]]$logit[[mdl]], test='Chisq')

# H0 cannot be rejected at p<.05


# LR test evaluation # =================================================================================

# For model 5 (French electoral college) H0 cannot be rejected with p<.05. 

# The following variables are going to be excluded 
# D7_rec, D8_rec, and EDU_rec

# Syntvars evaluation: Updating logit models (and related data frames) lists # ===============================

# fullmod_lst$logit[c(mdls)] <- partmod_lst[c(mdls)]

finalmod_lst <- list()
finalmod_lst[['DU-el']] <- fullmod_lst[['DU-el']]
finalmod_lst[['FR-el']] <- fullmod_lst[['FR-el']]

finalmod_lst$`FR-el`[['logit']][[8]] <- finalmod_lst$`FR-el`[['logit']][[7]]
finalmod_lst$`FR-el`[['logit']][[7]] <- finalmod_lst$`FR-el`[['logit']][[6]]
finalmod_lst$`FR-el`[['logit']][[6]] <- part_fit





# Syntvars evaluation: Updating AIC data frames (logit only) # =========================================

# logit AIC df 

logit_aic$`FR-el` %<>% 
          rbind(.,
                tibble('depvar'    = 'stack_212*',
                       'partycode' = 212, 
                       'full'      = part_fit %>% AIC,
                       'null'      = nullmod_lst$`FR-el`$logit[[5]] %>% AIC,
                ) %>% 
                  mutate(diff = full-null)) %>% 
          .[order(.$depvar, .$partycode),] 


 