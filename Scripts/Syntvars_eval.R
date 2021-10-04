# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Synthetic variables evaluation Script 
# Authors: G.Carteny
# last update: 2021-10-03
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# (1) run the EES2019_stack.R script until the 'Stack the original EES2019 variables' section and 
# (2) run the whole country-specific genvars script 

# Select the country-specific SDM # 

stckd_data <- EES2019_hr_stack



# Get rmse for OLS models # ============================================================================

df_lst <- 
  regdf.auxfun(data = stckd_data,
               depvar = 'Q10_gen',
               cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), 
               cont.indvar =  c('D4_age', 'D10_rec'),
               yhat.name = 'socdem',
               regsum = T)

fit_lst <-
  gensyn.fun(data = stckd_data,
             depvar = 'Q10_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), 
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name = 'socdem',
             regsum = T)



null_fit_lst <- 
  lapply(X = df_lst, regmod = 'OLS', null_mod.auxfun)

rmse_df <- 
  lapply(1:length(fit_lst), function(i) {
  tibble(full = get_rmse.auxfun(fit_lst[[i]], df_lst[[i]], regmod = 'OLS'),
         null = get_rmse.auxfun(null_fit_lst[[i]], df_lst[[i]], regmod = 'OLS'))
  }) %>% 
  do.call('rbind',.)



# Get rmse for logit models # ============================================================================

df_lst <- 
  regdf.auxfun(data = stckd_data,
               depvar = 'Q7_gen',
               cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), # 'D6_une', 'D6_rec', 'D9_rec'
               cont.indvar =  c('D4_age', 'D10_rec'),
               yhat.name = 'socdem',
               regsum = T)

fit_lst <-
  gensyn.fun(data = stckd_data,
             depvar = 'Q7_gen',
             cat.indvar =  c('D3_rec', 'D8_rec',  'D5_rec', 'EDU_rec'), #'D6_une', 'D6_rec', 'D9_rec'
             cont.indvar =  c('D4_age', 'D10_rec'),
             yhat.name = 'socdem',
             regsum = T)



null_fit_lst <- 
  lapply(X = df_lst, regmod = 'logit', null_mod.auxfun)


rmse(actual = df_lst[[1]]$stack_412 %>% as.numeric-1,
     predicted = predict(fit_lst[[1]], newdata = df_lst[[1]], type='response') %>% rbinom(length(.),1,.))

get_rmse(fit_lst[[1]], data=df_lst[[1]])

rmse_df <- 
  lapply(1:length(fit_lst), function(i) {
  tibble(full = get_rmse.auxfun(fit_lst[[i]], df_lst[[i]], regmod = 'logit'),
         null = get_rmse.auxfun(null_fit_lst[[i]], df_lst[[i]], regmod = 'logit'))
  }) %>% 
  do.call('rbind',.)





# Evaluate problematic models # ========================================================================


# Get y ~ x_i contingency tables for possible empty cells # - - - - - - - - - - - - - - - - - - - - - - 

tabs_lst <- lapply(X = df_lst, contab = T, FUN = yxcontab.auxfun)

tabs_lst[[6]][[4]] %>% .[order(.[,1]),]
tabs_lst[[7]][[3]] %>% .[order(.[,1]),]
tabs_lst[[7]][[4]] %>% .[order(.[,1]),]

# Select the data frames for testing the regression results # - - - - - - - - - - - - - - - - - - - - - 

df_lst %<>% lapply(., na.omit)

xs <- names(df_lst[[1]])[-c(1,2)]
y6 <- names(df_lst[[6]])[c(2)]
y7 <- names(df_lst[[7]])[c(2)]

mod6_full_frml <- formula(paste(y6, paste(xs, collapse = " + "), sep = " ~ "))
mod6_nest_frml <- formula(paste(y6, paste(xs[xs!='EDU_rec'], collapse = " + "), sep = " ~ "))

mod7_full_frml <- formula(paste(y7, paste(xs, collapse = " + "), sep = " ~ "))
mod7_nest_frml <- formula(paste(y7, paste(xs[xs %!in% c('EDU_rec', 'D5_rec', 'D8_rec')], collapse = " + "), sep = " ~ "))


mod6_full_fit  <- glm(data = df_lst[[6]], formula = mod6_full_frml, family = binomial)
mod6_nest_fit  <- glm(data = df_lst[[6]], formula = mod6_nest_frml, family = binomial)

mod7_full_fit  <- glm(data = df_lst[[7]], formula = mod7_full_frml, family = binomial)
mod7_nest_fit  <- glm(data = df_lst[[7]], formula = mod7_nest_frml, family = binomial)



anova(mod6_nest_fit, mod6_full_fit, test='Chisq')
anova(mod7_nest_fit, mod7_full_fit, test='Chisq')


df_lst[[6]] %<>% mutate(yhat = predict(mod6_nest_fit, type='response'), cat = rbinom(1,1,yhat))
df_lst[[7]] %<>% mutate(yhat = predict(mod7_nest_fit, type='response'), cat = rbinom(1,1,yhat))

# https://daviddalpiaz.github.io/r4sl/logistic-regression.html
# For ConfusionMatrix (sensitivity, specificty, etc.)

tab_mod6 <- table(predicted = df_lst[[6]]$cat, actual = df_lst[[6]]$stack_413)
tab_mod7 <- table(predicted = df_lst[[7]]$cat, actual = df_lst[[7]]$stack_401)

# caret::confusionMatrix(tab_mod6) # without any 1 predicted it cannot be computed


# Eval a '''''good''''' model # ========================================================================

df_lst[[1]] %<>% na.omit()

xs <- names(df_lst[[1]])[-c(1,2)]
y1 <- names(df_lst[[1]])[c(2)]

mod1_full_frml <- formula(paste(y1, paste(xs, collapse = " + "), sep = " ~ "))
mod1_full_fit  <- glm(data = df_lst[[1]], formula = mod1_full_frml, family = binomial)
df_lst[[1]] %<>% mutate(yhat_prob = predict(mod1_full_fit, type='response'),
                        yhat_logi = predict(mod1_full_fit, type='link'),
                        yhat_cent = yhat_logi - mean(yhat_logi),
                        yhat_centprob = logit2prob.auxfun(yhat_cent),
                        cat = rbinom(n(),1,yhat_prob),
                        cat_cent = rbinom(n(),1,yhat_centprob)) # , cat = rbinom(1,1,yhat))



tab_mod1a <- table(predicted = df_lst[[1]]$cat, actual = df_lst[[1]]$stack_412)
tab_mod1b <- table(predicted = df_lst[[1]]$cat_cent, actual = df_lst[[1]]$stack_412)


caret::confusionMatrix(tab_mod1a, positive='1') # without any 1 predicted it cannot be computed
caret::confusionMatrix(tab_mod1b, positive='1') # without any 1 predicted it cannot be computed