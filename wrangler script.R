process_multiple_response <- function(survey_data, col_indexes, 
                                      output_variable)
{
  column_names <- colnames(survey_data)[col_indexes]
  
  unique_values <- vector()
  for (column_name in column_names) {
    unique_values <- append(unique_values, survey_data[[column_name]])
  }
  unique_values <- as.integer(unique(unique_values))
  
  created_variables <- vector()
  for (value in unique_values)
  {
    if (value != 0)
    {
      name <- paste(output_variable,value,sep="_m")
      created_variables <- append(created_variables, name)
      
      survey_data[[name]] <- with(survey_data, 0)
      
      for (column_name in column_names) {
        qname <- quo(name)
        selection <- survey_data[[column_name]] == as.integer(value)
        
        survey_data <- mutate_at(survey_data,
                                 vars(name),
                                 ~replace(.,
                                          selection,
                                          value))
      }
    }
  }
  
  print("columns created: ")
  print(created_variables)
  
  return (survey_data)
}

var1_or_var2 <- function(survey_data, variable1, variable2, 
                         selection_criteria, output_variable)
{
  survey_data[[output_variable]] <- with(survey_data, 0)
  
  for(value in selection_criteria) {
    selection1 <- survey_data[[variable1]] == as.integer(value)
    selection2 <- survey_data[[variable2]] == as.integer(value)
    selection <- selection1 | selection2
    
    survey_data <- mutate_at(survey_data,
                             vars(all_of(output_variable)),
                             ~replace(.,
                                      selection,
                                      1))
  }
  
  return(survey_data)
}

less_than_age <- function(survey_data, indicator, age, output_variable)
{
  survey_data[[output_variable]] <- 0
  
  columns <- colnames(survey_data)[grepl(indicator, colnames(survey_data))]
  for (column in columns)
  {
    survey_data[[output_variable]] <- 
      survey_data[[output_variable]] + 
      ((survey_data[[column]] >= 0) & (survey_data[[column]] < age))
  }
  
  return (survey_data)
}

household_members <- function(survey_data, indicator, output_variable)
{
  survey_data[[output_variable]] <- 0
  
  columns <- colnames(survey_data)[grepl(indicator, colnames(survey_data))]
  for (column in columns)
  {
    survey_data[[output_variable]] <- 
      survey_data[[output_variable]] + (survey_data[[column]] > 0)
  }
  
  return (survey_data)
}

includes <- function(survey_data, input_variable, output_variable, fill_na)
{
  if(is.integer(survey_data[[input_variable]]))
  {
    survey_data[[output_variable]] <- fill_na
    selection <- survey_data[[input_variable]] > 0
    
    survey_data <- mutate_at(survey_data,
              vars(output_variable),
              ~ifelse(selection,
                      1,
                      fill_na))
  }
  return (survey_data)
}

drop_variable <- function(survey_data, variable)
{
  survey_data <- select(survey_data, -as.name(variable))
  return(survey_data)
}
