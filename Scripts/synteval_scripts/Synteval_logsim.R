# Admin # ==============================================================================================

want = c("tidyverse", "magrittr", "haven", "data.table", "labelled", "here", "stringr", "rlang", "car",
         "caret")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
options(scipen = 99)

rm(list = ls())
# rm(list= ls()[!(ls() %in% c('keepThis','andThis'))]) # useful for further implementations


# Useful references # ==================================================================================


# https://www.statlect.com/fundamentals-of-statistics/maximum-likelihood-hypothesis-testing
# https://academic.oup.com/aje/article/187/4/864/4084405
# https://daviddalpiaz.github.io/appliedstats/logistic-regression.html
# https://stephens999.github.io/fiveMinuteStats/asymptotic_normality_mle.html
# https://academic.oup.com/aje/article/179/2/252/123902#948326

# Study of models with zero cells 


sim_logistic_data = function(sample_size = 25, beta_0 = -2, beta_1 = 3) {
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}