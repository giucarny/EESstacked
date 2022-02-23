# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 new variables labels 
# Author: G.Carteny & J.Leiser
# last update: 2022-02-23
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# D1_rec # =============================================================================================

EES2019_stckd$D1_rec <- 
  labelled(
    x = EES2019_stckd$D1_rec, 
    labels = c(
      "Not a member of a trade union" = 0,
      "Member of a trade union"       = 1,
      "Don't know"                    = 98,
      "No answer"                     = 99
    ),
    label = 'Trade union membership'
  )

