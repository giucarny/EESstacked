# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Variable value labels harmonization check 
# Authors: G.Carteny
# last update: 2021-09-04
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Get value labels # ===================================================================================

source(here('Scripts', 'EES2019_stack.R'))

getvallabs.fun <- 
  function(vrbl) {
    df <- 
      data.frame(val = EES2019[[vrbl]] %>% val_labels() %>% as.integer(),
                 lab = EES2019[[vrbl]] %>% val_labels() %>% attr(., 'names')) %>% 
      mutate(variable = vrbl) %>% 
      as_tibble()
    
    return(df)
  }

lst <- lapply(c('Q2', 'Q7', 'Q9', 'Q25'), getvallabs.fun)

names(lst) <- c('Q2', 'Q7', 'Q9', 'Q25')


# Check the values of interest # =======================================================================

# lapply(lst, head)
# 
# lapply(lst, tail)

# New labels for original EES2019 variables # ==========================================================

# Q2: 
# 0  = None of the parties
# 90 = Other party
# 96 = Not applicable (Q1 = DK)
# 98 = DK
# 99 = NA

# Q7: 
# 0  = Did not vote
# 90 = Voted for other party
# 96 = Did vote blanc or nil
# 98 = Do not remember
# 99 = NA

# Q9: 
# 0  = Did not vote
# 90 = Voted for other party
# 96 = Did vote blanc or nil
# 98 = Do not remember
# 99 = NA

# Q25: 
# 90 = Feels close to other party
# 98 = DK
# Minor values for each country (excluding missing values) = Do not feel close to any party

# Possible labels for EES2019 stacked variables # ======================================================

source(here('Scripts', 'country_spec_scripts', 'EES2019_it_genvars.R'))

EES2019_it_stack %>%
  dplyr::select(Q25, Q25_rec, Q25_rec_gen) %>%
  distinct() %>%
  print(., n=nrow(.))


# Q2_gen:
# 0 = R does not consider the stack party the best at dealing with the most important issue
# 1 = R considers the stack party the best at dealing with the most important issue
# 96 = Not applicable (EES2019 Q1 = Don't know)
# 98 = R does not know 

# Q7_gen:
# 0 = R did not vote for the stack party (Voted for another party, or did not vote, or voted blank or nil)
# 1 = R voted for the stack party 
# 98 = R does not remember

# Q9_rec_gen:
# 0 = R did not vote for the stack party (Voted for another party, or did not vote, or voted blank or nil)
# 1 = R voted for the stack party 
# 98 = R does not remember

# Q25_reg_gen:
# 0 = R does not feel close to the stack party (Feels close to another party or does not feel close to any)
# 1 = R feels close to the stack party 
# 98 = R does not know




