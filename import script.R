read_schema <- function(filename) 
{
  schema <- read_csv(filename,
                     col_types = cols(.default = col_character()))
  
  col_types <- pull(schema, ColTypes)
  col_names <- pull(schema, ColNames)
  na_fill <- pull(schema, NAFill)
  schema <- list(col_types, col_names, na_fill)
  names(schema) <- c("type", "name", "fill")
  return (schema)
}

read_survey <- function(file_name, 
                        col_types,
                        col_names,
                        age_format="none",
                        na_fill,
                        drop_last=FALSE) 
{
  
  survey_data <- read_csv(file_name, 
                       col_types = cols(.default = col_character()),
                       na="NA")
  
  survey_data <- survey_data %>% mutate_at(vars(col_names[col_types == "integer"]),
                            funs(as.integer)) %>% 
    mutate_at(vars(col_names[col_types == "double"]),
                            funs(as.double)) %>% 
    mutate_at(vars(col_names[col_types == "character"]),
                            funs(as.character)) %>% 
    mutate_at(vars(col_names[col_types == "factor"]),
                            funs(as.factor)) 
  
  for (i in 1:length(col_types)) 
  {
    if((col_types[i] == "integer") | (col_types[i] ==  "double")) {
      survey_data <- survey_data %>% 
        mutate_at(vars(col_names[i]), 
                  ~ if_else(is.na(.), as.integer(na_fill[i]), .))
    }
    if(col_types[i] == "factor") {
      levels(survey_data[[i]])[levels(survey_data[[i]]) == ""] <- 
        as.character(na_fill[i])
    }
  }
      
  if (age_format == "YM") {
    survey_data <- survey_data %>% mutate_at(vars(col_names[col_types == "age"]),
                                             ~ if_else(grepl("Y|y",.), 
                                                       str_sub(.,1,1), .)) %>% 
      mutate_at(vars(col_names[col_types == "age"]),
                ~ if_else(grepl("M|m",.), "0", .)) %>% 
      mutate_at(vars(col_names[col_types == "age"]), funs(as.integer))
  } else {
    survey_data <- survey_data %>% mutate_at(vars(col_names[col_types == "age"]), 
                                             funs(as.integer))
  }
  for (i in 1:length(col_types)) {
    if (col_types[i] == "age") {
      survey_data <- survey_data %>% mutate_at(vars(col_names[i]), 
                                ~ if_else(is.na(.), 
                                          as.integer(na_fill[[i]]), .))
    }
  }
  
  if (drop_last) {survey_data <- select(survey_data, -last_col())}
  
  return(survey_data)
} 

drop_value <- function(survey_data,
                       value="888",
                       col_index,
                       col_types) 
{
  col_names <- colnames(survey_data)[col_index]
  types <- col_types[col_index]
  
  for (name in col_names[types == "integer"]) {
    to_remove <- which(survey_data[[name]] == as.integer(value))
    if (length(to_remove) != 0) {survey_data <- slice(survey_data, -to_remove)}
  } 
  for (name in col_names[types == "double"]) {
    to_remove <- which(survey_data[[name]] == as.double(value))
    if (length(to_remove) != 0) {survey_data <- slice(survey_data, -to_remove)}
  }
  for (name in col_names[types == "character"]) {
    to_remove <- which(survey_data[[name]] == value)
    if (length(to_remove) != 0) {survey_data <- slice(survey_data, -to_remove)}
  }  
  for (name in col_names[types == "factor"]) {
    to_remove <- which(survey_data[[name]] == value)
    if (length(to_remove) != 0) {survey_data <- slice(survey_data, -to_remove)}
  }
  for (name in col_names[types == "age"]) {
    to_remove <- which(survey_data[[name]] == as.integer(value))
    if (length(to_remove) != 0) {survey_data <- slice(survey_data, -to_remove)}
  }
  
  return (survey_data)
}

stack_table <- function(survey_data, 
                         variable_list)
{
  var_set <- vector()
  
  for (name in variable_list) 
  {
    colnames <- names(survey_data)[grepl(paste(name,"_",sep=""), names(survey_data))]
    var_set <- append(var_set, colnames)
  }
  
  first_run <- TRUE
  
  for (i in 1:15) 
  {
    column_set <- seq(i,
                      length(var_set) - 
                      length(var_set)/length(variable_list) +i,
                      length(var_set)/length(variable_list))
    col_names <- var_set[column_set]
    selection <- survey_data %>% select(all_of(col_names))
    colnames(selection) <- variable_list
    
    if (first_run) {
      
      stacked_data <- selection
      first_run <- FALSE
    } else {
      stacked_data <- bind_rows(stacked_data, selection)
    }
    
  } 
  
  return (stacked_data)
}
