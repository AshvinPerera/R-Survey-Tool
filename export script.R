read_export_schema <- function(filename)
{
  schema <- read_csv(filename,
                     col_types = cols(.default = col_character()))
  
  workbooks <- pull(schema, Workbook)
  sheets <- pull(schema, Sheet)
  Variable_1 <- pull(schema, Var1)
  Variable_2 <- pull(schema, Var2)
  gen_types <- pull(schema, Type)
  schema <- list(workbooks, sheets, Variable_1, Variable_2, gen_types)
  names(schema) <- c("workbooks", "sheets", 
                     "variable_1", "variable_2", "gen_types")
  return (schema)
}

run_export_schema <- function(survey_data, filename)
{
  export_schema <- read_export_schema(filename)
  
  for(book_name in unique(export_schema$workbooks))
  {
    indexes <- export_schema$workbooks == book_name
    out_data <- create_df_output(survey_data,
                                 export_schema$variable_1[indexes],
                                 export_schema$variable_2[indexes],
                                 export_schema$sheets[indexes],
                                 export_schema$gen_types[indexes]) 
    
    create_workbook(out_data, book_name)
  }
}

create_workbook <- function(to_write, book_name)
{
  temp_name <- paste(book_name,"xlsx", sep = ".")
  of=paste("output/", temp_name, sep = "") 
  wb <- xlsx::createWorkbook()
  
  for (sname in names(to_write))
  {
    sheet <- xlsx::createSheet(wb,sname)
    dfs <- to_write[[sname]]
    
    write_data(wb, sheet, dfs)
  }
  
  xlsx::saveWorkbook(wb = wb,file = of)
}

create_df_output <- function(survey_data, 
                             variable1, 
                             variable2, 
                             sheet_names, 
                             out_types)
{
  out_data = list()
  
  for(sheet in unique(sheet_names))
  {
    out_data[[sheet]] = list()
    temp_var1 <- variable1[sheet_names == sheet]
    temp_var2 <- variable2[sheet_names == sheet]
    temp_type <- out_types[sheet_names == sheet]
    
    for (i in 1:length(temp_type))
    {
      out_data[[sheet]][[as.character(i)]] <- list()
      if(substr(temp_type[i], start = 1, stop = 3) == "TAB")
      {
        if(substr(temp_type[i], start = 13, stop = 13) == "T")
        {
          df <- as.data.frame(tabulation(survey_data, temp_var1[i], 
                                         TRUE, decode, code_file))
        }
        else
        {
          df <- as.data.frame(tabulation(survey_data, temp_var1[i]))
        }
        out_data[[sheet]][[as.character(i)]][["type_str"]] <- temp_type[i]
        out_data[[sheet]][[as.character(i)]][["variable1"]] <- temp_var1[i]
        out_data[[sheet]][[as.character(i)]][["variable2"]] <- NA
        out_data[[sheet]][[as.character(i)]][["table1"]] <- df
        out_data[[sheet]][[as.character(i)]][["table2"]] <- NA
      }
      
      if(substr(temp_type[i], start = 1, stop = 3) == "CRS")
      {
        if(substr(temp_type[i], start = 13, stop = 13) == "T")
        {
          tab <- cross_tabulation(survey_data, temp_var1[i], temp_var2[i], 
                                  TRUE, decode, code_file)
        }
        else
        {
          tab <- cross_tabulation(survey_data, temp_var1[i], temp_var2[i])
        }
        tab_f <- tab$frequency$table
        df1 <- as.data.frame.matrix(tab_f)
        
        tab_wr <- tab$`within row`$crosstab
        df2 <- as.data.frame.matrix(tab_wr)
        
        out_data[[sheet]][[as.character(i)]][["type_str"]] <- temp_type[i]
        out_data[[sheet]][[as.character(i)]][["variable1"]] <- temp_var1[i]
        out_data[[sheet]][[as.character(i)]][["variable2"]] <- temp_var2[i]
        out_data[[sheet]][[as.character(i)]][["table1"]] <- df1
        out_data[[sheet]][[as.character(i)]][["table2"]] <- df2
      }
      
      if(substr(temp_type[i], start = 1, stop = 3) == "SUM")
      {
        df <- as.data.frame(unclass(summary(
          survey_data[[temp_var1[i]]][!is.na(survey_data[[temp_var1[i]]])]
          )))
        df <- cbind(data.frame(statistic=rownames(df)), df)
        colnames(df) <- c("statistic", "value")
        
        sdev <- sqrt(var(survey_data[[temp_var1[i]]], na.rm = TRUE))
        
        temp <- data.frame(statistic = "S.Dev", value = sdev, row.names = "X.6")
        df <- rbind(df, temp)
        
        sample_size <- length(survey_data[[temp_var1[i]]]) - 
          sum(is.na(survey_data[[temp_var1[i]]]))
        
        temp <- data.frame(statistic = "Sample Size", value = sample_size, row.names = "X.7")
        df <- rbind(df, temp)
        
        out_data[[sheet]][[as.character(i)]][["type_str"]] <- temp_type[i]
        out_data[[sheet]][[as.character(i)]][["variable1"]] <- temp_var1[i]
        out_data[[sheet]][[as.character(i)]][["variable2"]] <- NA
        out_data[[sheet]][[as.character(i)]][["table1"]] <- df
        out_data[[sheet]][[as.character(i)]][["table2"]] <- NA
      }
      
      if(substr(temp_type[i], start = 1, stop = 3) == "CAT")
      {
        
        categories <- sort(unique(survey_data[[temp_var1[i]]]))
        
        df <- data.frame(Min.=double(),
                         `1st Qu.`=double(),
                         Median=double(),
                         Mean=double(),
                         `3rd Qu.`=double(),
                         Max.=double(),
                         S.Dev=double(),
                         Sample=integer())
        
        for(category in categories)
        {
          temp_df<- t(as.data.frame(unclass(summary(
            survey_data[[temp_var2[i]]][!is.na(survey_data[[temp_var2[i]]]) & survey_data[[temp_var1[i]]] == category]
            ))))
          
          temp_df <- cbind(temp_df, data.frame(
            S.Dev=sqrt(var(survey_data[survey_data[[temp_var1[i]]] == category, temp_var2[i]], na.rm = TRUE)),  row.names = rownames(temp_df)
              )) 
          
          temp_df <- cbind(temp_df, data.frame(
            Sample=(length(survey_data[[temp_var2[i]]][survey_data[[temp_var1[i]]] == category]) - 
                     sum(is.na(survey_data[[temp_var2[i]]][survey_data[[temp_var1[i]]] == category]))),  row.names = rownames(temp_df)
          )) 
          
          colnames(temp_df) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "S.Dev", "Sample")
          rownames(temp_df) <- c(as.character(category)) 
          
          df <- rbind(df, temp_df)
        }
        
        out_data[[sheet]][[as.character(i)]][["type_str"]] <- temp_type[i]
        out_data[[sheet]][[as.character(i)]][["variable1"]] <- temp_var1[i]
        out_data[[sheet]][[as.character(i)]][["variable2"]] <- temp_var2[i]
        out_data[[sheet]][[as.character(i)]][["table1"]] <- df
        out_data[[sheet]][[as.character(i)]][["table2"]] <- NA
      }
    }
  }
  
  return(out_data)
}

write_data <- function(wb, sname, dfs)
{
  startRow <- 2
  
  for(data_table in dfs)
  {
    if(substr(data_table$type_str, start = 1, stop = 3) == "TAB")
    {
      metadata <- data.frame(values=c(data_table$variable1, "categorical tabulation"), row.names = c("variable", "type"))
      addDataFrame(x=metadata, sheet=sname,row.names=TRUE, col.names=FALSE,showNA=F,startRow=startRow)
      startRow = startRow + nrow(metadata) + 1
      
      addDataFrame(x=data_table$table1, sheet=sname,row.names=FALSE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(data_table$table1) + 3
    }
    
    if(substr(data_table$type_str, start = 1, stop = 3) == "CRS")
    {
      metadata <- data.frame(values=c(data_table$variable1, data_table$variable2, "categorical cross tabulation"), row.names = c("variable 1", "variable 2", "type"))
      addDataFrame(x=metadata, sheet=sname,row.names=TRUE, col.names=FALSE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(metadata) + 1
      
      addDataFrame(x=data_table$table1, sheet=sname,row.names=TRUE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(data_table$table1) + 2
      
      addDataFrame(x=data_table$table2, sheet=sname,row.names=TRUE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(data_table$table2) + 3
    }
    
    if(substr(data_table$type_str, start = 1, stop = 3) == "SUM")
    {
      metadata <- data.frame(values=c(data_table$variable1, "numerical summary"), row.names = c("variable", "type"))
      addDataFrame(x=metadata, sheet=sname,row.names=TRUE, col.names=FALSE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(metadata) + 1
      
      addDataFrame(x=data_table$table1, sheet=sname,row.names=FALSE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(data_table$table1) + 3
    }
    
    if(substr(data_table$type_str, start = 1, stop = 3) == "CAT")
    {
      metadata <- data.frame(values=c(data_table$variable1, data_table$variable2, "numerical summary by category"), row.names = c("variable1", "variable2", "type"))
      addDataFrame(x=metadata, sheet=sname,row.names=TRUE, col.names=FALSE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(metadata) + 1
      
      addDataFrame(x=data_table$table1, sheet=sname,row.names=TRUE,showNA=T,startRow=startRow)
      startRow = startRow + nrow(data_table$table1) + 3
    }
  }
}

write_chart <- function(wb, sname, dfs)
{
  
}

# out_type format : XXX-XXX-XXX-X
# type of table output - need chart? - chart type - T/F for decoding

# sheet list will have out_type, table1, table2, 

# write_data will write out all required tables, and then generate required 
# charts in excel using a python script