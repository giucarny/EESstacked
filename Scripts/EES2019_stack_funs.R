# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Auxiliary Functions for the Stacking Procedure
# Author: G.Carteny
# last update: 2021-09-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Generic dichotomous variables estimation # ===========================================================

gendic.fun <- function(data, var, stack_var) {
  
  if (var == 'Q2') {
    data$Q2[data$Q2==9999] <- 96
  } else if (var == 'Q7') {
    data$Q7[data$Q7==9999] <- 0
  }
  
  newvar <- paste0(var, '_gen')
  exprss <- paste0('case_when(', var, '>96 & ', var,'<100', ' ~ ', var,', ',
                   var, ' == ', stack_var, ' ~ 1,',
                   ' T ~ 0)')
  
  q <- quote(mutate(data, !! newvar := exprss))
  
  df <-
    eval(parse(text=sub("exprss", exprss, deparse(q)))) %>%
    dplyr::select(all_of(newvar)) %>%
    as_tibble() %>%
    zap_labels()
  
  return(df)
}

# Examples 

# Q2_gen <- gendic.fun(EES2019_stckd, var = 'Q2', stack_var = 'party')
# 
# Q7_gen <- gendic.fun(EES2019_stckd, var = 'Q7', stack_var = 'party')


# Older functions # ====================================================================================

# Additional function for creating stacked variables - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# genstackedvar.int.fun <- function(data, depvar, index, refvar) {
#   
#   newvar = paste0(depvar, '_stack_', index)
#   
#   data[[newvar]] <- data[[depvar]]
#   exprss <- paste0('case_when(', refvar, '==', index, ' ~ ', depvar,',', 
#                    'is.na(', refvar, ') ~ NA_real_,',
#                    'T ~ 0)')
#   q <- quote(mutate(data, !! newvar := exprss))
#   df2 <- 
#     eval(parse(text=sub("exprss", exprss, deparse(q)))) %>% 
#     dplyr::select(all_of(newvar)) %>%
#     as_tibble()
#   return(df2)
# }
# 
# genstackedvar <- function(data, indices, stub1, stub2) {
#   df <- lapply(data=data, X = indices, depvar = stub1, refvar = stub2, FUN = genstackedvar.int.fun) %>% do.call('cbind',.)
#   return(df)
# }


# Y-hat fun - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# genyhats <- function(data, depvar, regtype, indvar, newname) {
#   
#   frmla <- as.formula(paste(depvar, paste(indvar, collapse = " + "), sep = " ~ "))
#   
#   ics <- data %>% dplyr::select(all_of(depvar), all_of(indvar))
#   
#   # for(i in 2:length(ics)) {
#   #   vec <- ics[[i]]
#   # 
#   #   cl <- class(ics[[i]])
#   # 
#   #   if(cl=='numeric') {
#   #     ics[[i]] <- ifelse(is.na(vec), mean(vec, na.rm=T) , vec)
#   #   } else if (cl=='factor') {
#   #     ics[[i]] <- ifelse(is.na(vec), median(vec, na.rm=T) , vec)
#   #   }
#   # }
#   
#   if (regtype=='logit' | regtype=='log') {
#     x <- glm(frmla, data = ics, family = "binomial")
#     
#     outcome <- data.frame(respos = predict(x) %>% attr(., 'names') %>% as.numeric(),
#                           yhat = predict(x, type='response'))
#     
#   } else if (rlang::is_missing(regtype) | regtype=='linear' | regtype=='OLS') {
#     x <- lm(frmla, data = ics)
#     
#     outcome <- data.frame(respos = predict(x) %>% attr(., 'names') %>% as.numeric(),
#                           yhat = predict(x))
#   }
#   
#   respid <- data.frame(respos = 1:nrow(data),
#                        respid = data$respid)
#   
#   outcomedf <- left_join(respid, outcome, by='respos')
#   
#   outcomedf %<>% dplyr::select(respid, yhat)
#   
#   names(outcomedf)[names(outcomedf)=='yhat'] <- paste0(newname, '_yhat_', gsub(pattern = '.*\\_', '', depvar))
#   
#   df <- left_join(data, outcomedf, by='respid')
#   
#   return(df)
# }


# Functions for generating party-voter distances - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# gendist.int.fun <- function(data, index, var1, var2){
#   exprss <- paste0('abs(', var1, '-', var2, index, ')')
#   newvar <- paste0(var2, index, "dist")
#   q <- quote(mutate(data, !! newvar := exprss))
#   df2 <- eval(parse(text=sub("exprss", exprss, deparse(q)))) %>% dplyr::select(all_of(newvar))
#   return(df2)
# }
# 
# gendist <- function(data, indices, stub1, stub2) {
#   df <- lapply(data=data, X = indices, var1 = stub1, var2 = stub2, FUN = gendist.int.fun) %>% do.call('cbind',.)  
#   return(df)
# }


# Function for generating the stacked data matrix - - - - - - - - - - - - - - - - - - - - - - - - - - -

# genstacks <- function(idvar, data, stubs, keepvar) {
#   
#   df <- data %>% dplyr::select(starts_with(paste0(stubs)))
#   
#   id1 <- data %>% dplyr::select(all_of(idvar)) %>% unlist()
#   id2 <- names(df) %>% gsub('.*\\_', '', ., perl = T) %>% unique %>% as.numeric()
#   
#   stack_df <- expand.grid(id1, id2) %>% as_tibble() %>% .[order(.$Var1),]
#   
#   for (i in stubs) {
#     df2 <- data %>% 
#       dplyr::select(all_of(idvar), starts_with(i)) %>%
#       pivot_longer(cols = starts_with(paste0(i)),
#                    names_to = paste0(i, '_n'), 
#                    values_to = i)
#     df2[[paste0(i, '_n')]] %<>% gsub('.*\\_', '', ., perl = T) %>% as.numeric()
#     names(df2)[names(df2)==idvar] <- 'Var1'
#     names(df2)[names(df2)==paste0(i, '_n')] <- 'Var2'
#     stack_df <- left_join(stack_df, df2, by = c('Var1', 'Var2'))
#   }
#   
#   if (rlang::is_missing(keepvar)) {
#     return(stack_df)  
#   } else {
#     if (any(is.null(keepvar)) | any(is.na(keepvar))) {
#       warning("Argument 'keepvar' is invalid")
#     } else {
#       
#       keepvar %<>% as.character()
#       
#       if (any(keepvar %in% colnames(data))) {
#         if(all(keepvar %in% colnames(data))) {
#           df <- data %>% dplyr::select(all_of(idvar), all_of(keepvar))
#           names(df)[[1]] <- 'Var1'
#           stack_df <- left_join(stack_df, df, by = 'Var1')
#           names(stack_df)[names(stack_df)=='Var1'] <- idvar
#           names(stack_df)[names(stack_df)=='Var2'] <- 'stack'
#           return(stack_df)
#         } else {
#           x <- which(keepvar %in% colnames(data))
#           df <- data %>% dplyr::select(all_of(idvar), keepvar[c(x)])
#           names(df)[[1]] <- 'Var1'
#           stack_df <- left_join(stack_df, df, by = 'Var1')
#           names(stack_df)[names(stack_df)=='Var1'] <- idvar
#           names(stack_df)[names(stack_df)=='Var2'] <- 'stack'
#           warning("Some elements of 'keepvar' are missing")
#           return(stack_df)
#         }
#       } else {
#         warning("Argument 'keepvar' is missing in the data")
#       } 
#     }
#   }
# }
# 
# 
# 
