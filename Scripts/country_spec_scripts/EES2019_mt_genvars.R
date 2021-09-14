# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for Estimating Generic Variables (EES 2019 Voter Study, Malta Sample) 
# Author: W. Haeussling
# last update: 2021-09-13
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Subset the EES original data frame, the SDM, and the EES codebook # ==================================

cntry = 'MT'

EES2019_mt <- EES2019 %>% filter(countryshort==cntry)
EES2019_stckd_mt <- EES2019_stckd %>% filter(countryshort==cntry)
EES2019_cdbk_mt <- EES2019_cdbk %>% filter(countryshort==cntry)

rm(cntry)

# Generic dichotomous variables estimation # ===========================================================

# Check first the variable of interest values
# lapply(c('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
#       function(vrbl) {
#         EES2019_stckd_mt %>%
#           dplyr::select(all_of(vrbl)) %>%
#           mutate(across(all_of(vrbl), ~as.numeric(.))) %>%
#           distinct})
#
# EES2019_stckd_mt %>%
#  dplyr::select(Q2) %>%
#  val_labels()

#Q2 , Q7, Q9_rec and Q25_rec also have value 90 (other parties). This 
#corresponds with a 0 in Q2_gen, Q7_gen, Q9_rec_gen and Q25_rec_gen.
#Given that 90 does not refer to parties, I made no additional changes 
#to the code. 
#Q9_rec also has value NA. This corresponds with a 0 in Q9_rec_gen. Since NA 
#does not refer to parties, I made no additional changes to the code. 

EES2019_mt_stack <- 
  cbind(EES2019_stckd_mt,  
        lapply(data = EES2019_stckd_mt, 
               X = list('Q2', 'Q7', 'Q9_rec', 'Q25_rec'),
               stack_var = 'party',
               FUN = gendic.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# Check the dataset 

# checkdataset.fun <- 
#   function(vrbl) {
#     
#     orivar <- vrbl
#     genvar <- paste0(vrbl, '_gen')
#     
#    EES2019_mt_stack %>%
#       dplyr::select(respid, party, all_of(orivar), all_of(genvar)) %>%
#       print(n=100)
#   
#   }
# checkdataset.fun('Q9_rec')%>% filter(is.na(Q9_rec)  & Q9_rec_gen!=0)

# Generic distance/proximity variables estimation # ====================================================

EES2019_mt_stack %<>%
  cbind(.,
        lapply(data = EES2019_mt,
               cdbk = EES2019_cdbk_mt,
               crit = 'average',
               rescale = T,
               check = F,
               keep_id = F,
               X = list('Q10','Q11','Q23'),
               FUN = gendis.fun) %>% 
          do.call('cbind',.)) %>% 
  as_tibble()

# EES2019_mt_stack %>% 
#  dplyr::select(respid, party, ends_with('gen')) %>% 
#  filter((abs(Q10_gen)>1 & Q10_gen!=98) | (abs(Q11_Q13_gen)>1 & Q11_Q13_gen!=98) |
#           (abs(Q23_Q24_gen)>1 & Q23_Q24_gen!=98))

# Clean the environment # ==============================================================================

rm(list=ls(pattern='_mt$'))  
