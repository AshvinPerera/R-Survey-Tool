tabulation <- function(survey_data, 
                       variable, 
                       should_decode=FALSE,
                       codec=NULL,
                       code_filename=NULL) 
{
  gp <- survey_data %>%
    dplyr::group_by_at(vars(variable))
  tab <- dplyr::summarise(gp, n = n()) %>% 
    mutate(totalN = (cumsum(n)),
    percent = round((n / sum(n)), 3),
    cumpercent = round(cumsum(freq = n / sum(n)), 3))
  
  if(should_decode) {
    tab <- decodey(code_filename, codec, tab, variable)
  }
  
  return (tab)
}

cross_tabulation <- function(survey_data, 
                             variable1, 
                             variable2,
                             should_decode=FALSE,
                             codec=NULL,
                             code_filename=NULL)
{
  x <- survey_data[[variable1]]
  y <- survey_data[[variable2]]
  
  tab1 <- crosstab(survey_data, row.vars = variable1, col.vars = variable2, type = "f")
  tab2 <- crosstab(survey_data, row.vars = variable1, col.vars = variable2, type = "r")
  tab3 <- crosstab(survey_data, row.vars = variable1, col.vars = variable2, type = "c")
  
  tab <- list(tab1, tab2, tab3)
  names(tab) <- c("frequency", "within row", "within col")
  
  if(should_decode) {
    tab <- decodexy(code_filename, codec, tab, variable1, variable2)
  }
  return (tab)
}

multi_row_tabulation <- function(survey_data, 
                                 row_variables,
                                 col_variable,
                                 should_decode=FALSE,
                                 codec=NULL,
                                 code_filename=NULL)
{
  tab <- crosstab(survey_data, row.vars = row_variables, 
           col.vars = col_variable, type = "j")
  
  if(should_decode) {
    tab <- decodexyz(code_filename, codec, tab, row_variables, col_variable)
  }

  return(tab)
}

# returns a table capturing change of variable over time
change_over <- function(survey_data, 
                        variable1, 
                        variable2, 
                        output_variable, 
                        common_name,                                 
                        should_decode=FALSE,
                        codec=NULL,
                        code_filename=NULL)
{
  tab1 <- as.data.frame(tabulation(survey_data, variable1))
  tab2 <- as.data.frame(tabulation(survey_data, variable2))
  
  colnames(tab1)[1] <- common_name
  colnames(tab2)[1] <- common_name
  
  tab_final <- merge(tab1, tab2, by=common_name, all=T)
  tab_final <- tab_final[,-c(3, 4, 5, 7, 8, 9)]
  colnames(tab_final)[c(2,3)] <- c("before", "after") 
  
  tab_final[is.na(tab_final$before), "before"] <- 0
  tab_final[is.na(tab_final$after), "after"] <- 0
  
  tab_final$change <- tab_final$after - tab_final$before
  
  if(should_decode) {
    tab_final <- decodex(code_filename, codec, tab_final, variable1, common_name)
  }
    
  return(tab_final)
}
