# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: EES2019 new variables labels 
# Author: G.Carteny & J.Leiser
# last update: 2022-03-07
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# countryshort # =============================================================================================

# create helper vector(s) to facilitate labeling

countrynames <- select(EES2019_stckd, countryname) %>% unique() %>% pull(countryname)

countryshort <- select(EES2019_stckd, countryshort) %>% unique() %>% pull(countryshort)

# tibble(countrynames, countryshort) %>% print(n=100)

# create a label vector

cntry_helper <-  c()

for (i in 1:27) {
  
  cntry_helper[i] <- paste0("'", countrynames[i], "'", "=", "'", countryshort[i], "'", "," )
  
}
cntry_helper[28] <- paste0("'", countrynames[28], "'", "=", "'", countryshort[28], "'" )

# collapse into single string
cntry_expr <- str_c(cntry_helper, collapse="")

# create labelled vector
cntry_helper2 <- eval(parse(text=paste0("c(", cntry_expr, ")")))


EES2019_stckd$countryshort <- 
  labelled(
    x = EES2019_stckd$countryshort, 
    labels = cntry_helper2,
    label = ''
  )

rm(cntry_helper, cntry_helper2, cntry_expr, countryshort, countrynames)

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


# D3_rec # =============================================================================================

EES2019_stckd$D3_rec <- 
  labelled(
    x = EES2019_stckd$D3_rec, 
    labels = c(
      "Male"                     = 1,
      "Female"                   = 2,
      "Other"                    = 3
    ),
    label = ''
  )

# D5_rec # =============================================================================================

EES2019_stckd$D5_rec <- 
  labelled(
    x = EES2019_stckd$D5_rec, 
    labels = c(
      "Single"                                           = 0,
      "Married/Remarried/Single living with a partner"   = 1,
      "Don't know"                                      = 98,
      "No answer"                                        = 99
    ),
    label = ''
  )

# D6_rec # =============================================================================================

EES2019_stckd$D6_rec <- 
  labelled(
    x = EES2019_stckd$D6_rec, 
    labels = c(
      "Self-employed"              = 1,
      "Employed"                   = 2,
      "In school"                  = 3,
      "Working in the household"   = 4,
      "Retired"                    = 5,
      "Unemployed"                 = 6,
      "Other"                      = 7,
      "No answer"                  = 99
    ),
    label = ''
  )

# D6_std_rec # =============================================================================================

EES2019_stckd$D6_std_rec <- 
  labelled(
    x = EES2019_stckd$D6_std_rec, 
    labels = c(
      "Student"                    = 0,
      "Not a student"              = 1,
      "No answer"                  = 99
    ),
  )


# D6_une_rec # =============================================================================================

EES2019_stckd$D6_une_rec <- 
  labelled(
    x = EES2019_stckd$D6_une_rec, 
    labels = c(
      "Not Unemployed"             = 0,
      "Unemployed"                 = 1,
      "No answer"                  = 99
    ),
    label = ''
  )


# D7_rec # =============================================================================================

EES2019_stckd$D7_rec <- 
  labelled(
    x = EES2019_stckd$D7_rec, 
    labels = c(
      "Working or lower middle class"            = 0,
      "Middle class"                             = 1,
      "Upper middle class or upper class"        = 2,
      "Other"                                    = 97,
      "Don't know"                               = 98,
      "No answer"                                = 99
    ),
    label = ''
  )


# D8_rec # =============================================================================================

EES2019_stckd$D8_rec <- 
  labelled(
    x = EES2019_stckd$D8_rec, 
    labels = c(
      "Rural area or village"            = 0,
      "Small, middle, or large town"     = 1
    ),
    label = ''
  )


# D9_rec # =============================================================================================

EES2019_stckd$D9_rec <- 
  labelled(
    x = EES2019_stckd$D9_rec, 
    labels = c(
      "Non believer/ Atheist/ Agnostic"            = 0,
      "Catholic"                                   = 1,
      "Orthodox"                                   = 2,
      "Protestant"                                 = 3,
      "Other Christian"                            = 4,
      "Other"                                      = 5,
      "No answer"                                  = 99
    ),
    label = ''
  )


# D10_rec # =============================================================================================


EES2019_stckd$D10_rec <- 
  labelled(
    x = EES2019_stckd$D10_rec, 
    labels = c(
      "Never/ About once a year"             = 0,
      "Less often"                           = 1,
      "About once a year"                    = 2,
      "Only on special holy days"            = 3,
      "About each 2 or 3 months"             = 4,
      "Once a month"                         = 5,
      "Once a week"                          = 6,
      "More than once a week"                = 7,
      "Don't know"                           = 98,
      "No answer"                            = 99
    ),
    label = ''
  )


# EDU_rec # =============================================================================================


EES2019_stckd$EDU_rec <- 
  labelled(
    x = EES2019_stckd$EDU_rec, 
    labels = c(
      "Low (15 or less years of schooling)"             = 1,
      "Medium (16-19 years of schooling)"               = 2,
      "High (20+ years of schooling)"                   = 3,
      "No answer"                                       = 99
    ),
    label = ''
  )


# Q25_rec # =============================================================================================

# create helper named vector to use for labelling

# create "raw" expression vector
Q25_rec_helper <- c()
Q25_rec_helper[1] <- paste0("'Respondent does not feel close to a political party'", " ", "=", " ", "0", ",") 
Q25_rec_helper[2] <- paste0("'Respondent feels close to a party not among the answer categories or a non-relevant party'"," ", "=", " ", "90", ",")
# t <-  101:2807
# length(t)

for (i in 3:2708) {
  Q25_rec_helper[i] <- paste0("'Respondent feels close to the  party ", i+98 , "' " , "=", " " , i+98, ",") 
  
}

Q25_rec_helper[2709] <- paste0("'Respondent feels close to the  party 2807'"," " , "=", " ", 2807) 


# collapse into single string
Q25middle_expr <- str_c(Q25_rec_helper, collapse="")

# create labelled vector
Q25_rec_helper2 <- eval(parse(text=paste0("c(", Q25middle_expr, ")")))


EES2019_stckd$Q25_rec <- 
  labelled(
    x = EES2019_stckd$Q25_rec, 
    labels = Q25_rec_helper2,
    label = ''
  )

rm("Q25_rec_helper", "Q25_rec_helper2", "Q25middle_expr")

# Q26_rec # =============================================================================================


EES2019_stckd$Q26_rec <- 
  labelled(
    x = EES2019_stckd$Q26_rec, 
    labels = c(
      "Responent is merely a sympathiser of the party specified in Q25_rec"             = 0,
      "Responent is fairly close to the party specified in Q25_rec"             = 1,
      "Responent is very close to the party specified in Q25_rec"               = 2,
      "Not asked (Respondent does not feel close to any party or does not know)"                   = 3,
      "Respondent does not remember/No answer"                                       = 99
    ),
    label = ''
  )

# Q9_rec # =============================================================================================

# create a helper function for labeling
Q9_rec_helper <- c()
Q9_rec_helper[1] <- paste0("'Respondent did not vote'", " ", "=", " ", "0", ",") 
Q9_rec_helper[2] <- paste0("'Respondent voted for another party'", " ", "=", " ", "90", ",") 
Q9_rec_helper[3] <- paste0("'Respondent did vote blanc or nil'", " ", "=", " ", "96", ",") 
Q9_rec_helper[4] <- paste0("'Respondent does not remember'", " ", "=", " ", "98", ",") 
Q9_rec_helper[5] <- paste0("'No answer'", " ", "=", " ", "99", ",") 

# get the number of unique parties
length(101:2814) #2714

for (i in 6:2718) {
  Q9_rec_helper[i] <- paste0("'Respondent voted for the  party ", i+95 , "' " , "=", " " , i+95, ",") 
  
}

Q9_rec_helper[2719] <- paste0("'Respondent voted for the  party ", 2814 , "' " , "=", " " , 2814) 

# collapse into single string
Q9middle_expr <- str_c(Q9_rec_helper, collapse="")

# create labelled vector
Q9_rec_helper2 <- eval(parse(text=paste0("c(", Q9middle_expr, ")")))

EES2019_stckd$Q9_rec <- 
  labelled(
    x = EES2019_stckd$Q9_rec, 
    labels = Q9_rec_helper2,
    label = ''
  )


rm("Q9_rec_helper", "Q9_rec_helper2", "Q9middle_expr", "i")



# Q2_gen # =============================================================================================

EES2019_stckd$Q2_gen <- 
  labelled(
    x = EES2019_stckd$Q2_gen, 
    labels = c(
      "Respondent does not consider the stack party the best at dealing with the most important issue"       = 0,
      "Respondent considers the stack party the best at dealing with the most important issue"               = 1,
      "Not applicable (Answer to Q1 = Don't know)"                                                           = 96,
      "Respondent does not know"                                                                             = 98,
      "No answer"                                                                                            = 99
    ),
    label = ''
  )



# Q7_gen # =============================================================================================

EES2019_stckd$Q7_gen <- 
  labelled(
    x = EES2019_stckd$Q7_gen, 
    labels = c(
      "Respondent did not vote for the stack party"             = 0,
      "Respondent voted for the stack party"                    = 1,
      "Respondent does not remember"                            = 98
    ),
    label = ''
  )


# Q9_gen # =============================================================================================

EES2019_stckd$Q9_gen <- 
  labelled(
    x = EES2019_stckd$Q9_gen, 
    labels = c(
      "Respondent did not vote for the stack party"             = 0,
      "Respondent voted for the stack party"                    = 1,
      "Respondent does not remember"                            = 98
    ),
    label = ''
  )


# Q25_gen # =============================================================================================

EES2019_stckd$Q25_gen <- 
  labelled(
    x = EES2019_stckd$Q25_gen, 
    labels = c(
      "Respondent does not feel close to the stack party"   = 0,
      "Respondent feels close to the stack party"           = 1,
      "Respondent does not know"                            = 98
    ),
    label = ''
  )

# Q26_gen # =============================================================================================

EES2019_stckd$Q26_gen <- 
  labelled(
    x = EES2019_stckd$Q26_gen, 
    labels = c(
      "Respondent does not feel close to the stack party"       = 0,
      "Respondent is merely a sympathiser of the stack party"    = 1,
      "Respondent feels fairly close to the stack party"        = 2,
      "Respondent feels very close to the stack party"          = 3,
      "Respondent does not know/ No answer"                     = 98
    ),
    label = ''
  )

# Q10_gen # =============================================================================================

EES2019_stckd$Q10_gen <- 
  labelled(
    x = EES2019_stckd$Q10_gen, 
    labels = c(
      "Respondent has a very low propensity to vote for the stack party"   = 0,
      "Respondent has a very high propensity to vote for the stack party"  = 1,
      "Respondent does not know"                                           = 98
    ),
    label = ''
  )


# Q11_Q13_gen # =============================================================================================

EES2019_stckd$Q11_Q13_gen <- 
  labelled(
    x = EES2019_stckd$Q11_Q13_gen, 
    labels = c(
      "Respondent is very distant from the stack party"   = 0,
      "Respondent is very close to the stack party"       = 1,
      "Respondent does not know"                          = 98
    ),
    label = ''
  )


# Q23_Q24_gen # =============================================================================================


EES2019_stckd$Q23_Q24_gen <- 
  labelled(
    x = EES2019_stckd$Q23_Q24_gen, 
    labels = c(
      "Respondent is very distant from the stack party"   = 0,
      "Respondent is very close to the stack party"       = 1,
      "Respondent does not know"                          = 98
    ),
    label = ''
  )


# socdem_synt_ptv # =============================================================================================

# EES2019_stckd$socdem_synt_ptv <- 
#   labelled(
#     x = EES2019_stckd$socdem_synt_ptv, 
#     labels = c(
#       "Respondent has a very low affinity with the stack party"   = 0,
#       "Respondent has a very high affinity with the stack party"  = 1,
#       "Not available"                                             = 99
#     ),
#     label = ''
#   )

# socdem_synt_vc # =============================================================================================

# socdem_vc_helper_neg <- EES2019_stckd$socdem_synt_vc %>% 
#   
# 
# 
# EES2019_stckd$socdem_synt_vc <-
#   labelled(
#     x = EES2019_stckd$socdem_synt_vc,
#     labels = c(
#       "Respondent has a very low affinity with the stack party"   = c(-2, -1),
#       "Respondent has a very high affinity with the stack party"  = 1,
#       "Not available"                                             = 99
#     ),
#     label = ''
#   )




# Checking the labels --------------------------------------------------------------------

# EES2019_stckd$countryshort # works
# EES2019_stckd$D1_rec # works
# EES2019_stckd$D3_rec # works
# EES2019_stckd$D5_rec # works
# EES2019_stckd$D6_rec # works
# EES2019_stckd$D6_std_rec # works
# EES2019_stckd$D6_une_rec # works
# EES2019_stckd$D7_rec # works
# EES2019_stckd$D8_rec # works
# EES2019_stckd$D9_rec # works
# EES2019_stckd$D10_rec # works
# EES2019_stckd$EDU_rec # works
# EES2019_stckd$Q25_rec # works
# EES2019_stckd %>% select(Q25_rec) %>% print(n = 100)
# EES2019_stckd$Q26_rec # works
# EES2019_stckd$Q9_rec # works
# EES2019_stckd %>% select(Q9_rec) %>% print(n = 100)
# EES2019_stckd$Q2_gen # works
# EES2019_stckd$Q7_gen # works
# EES2019_stckd$Q9_gen # works
# EES2019_stckd$Q25_gen # works
# EES2019_stckd$Q26_gen # works
# EES2019_stckd$Q10_gen # works
# EES2019_stckd$Q11_Q13_gen # works
# EES2019_stckd$Q23_Q24_gen # works




