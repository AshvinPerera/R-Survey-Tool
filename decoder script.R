import_decoder <- function(filename) 
{
  codec <- read_csv(filename,
                     col_types = cols(.default = col_character()))

  return (codec)
}

# important - unique schema/ column names
decodex <- function(code_filename, codec, tab, variable, col_name)
{
  sheet <- codec$SheetName[codec$Variable == variable]
  if(!is.na(sheet))
  {
    if(length(sheet) == 0) {
      print("coding table has no match for variable")
      return(tab)
    }
    
    if (sheet != "none") 
    {
      coding <- read_excel(code_filename, sheet = sheet)
      
      colnames(coding)[1] <- col_name
      
      tab <- dplyr::left_join(tab, coding, col_name)
      tab <- relocate(tab, Value)
      tab <- tab %>% select(-one_of(col_name))
      
      colnames(tab)[1] <- col_name
    }
  }
  
  
  return(tab)
}

# important - unique schema/ column names
decodey <- function(code_filename, codec, tab, variable)
{
  sheet <- codec$SheetName[codec$Variable == variable]
  
  if(!is.na(sheet))
  {
    if (sheet != "none") 
    {
      coding <- read_excel(code_filename, sheet = sheet)
      
      colnames(coding)[1] <- variable
      
      tab <- dplyr::left_join(tab, coding, variable)
      tab <- relocate(tab, Value)
      tab <- tab %>% select(-one_of(variable))
      
      colnames(tab)[1] <- variable
    }
  }
  
  return(tab)
}

# important - unique schema/ column names
decodexy <- function(code_filename, codec, tab, variablex, variabley)
{
  sheet1 <- codec$SheetName[codec$Variable == variablex]
  if(!is.na(sheet1))
  {
    if (sheet1 != "none") 
    {
      codingx <- read_excel(code_filename, sheet = sheet1)
      
      codes_to_get <- unlist(dimnames(tab$frequency$table)[1])
      codes_to_get <- codes_to_get[1:(length(codes_to_get)-1)]
      codes_to_get <- as.integer(codes_to_get)
      new_codes <- vector()
      for (i in 1:length(codes_to_get))
      {
        val <- codingx$Value[codingx$Code == codes_to_get[i]]
        new_codes <- append(new_codes, val)
      } 
      
      dimnames(tab$frequency$table)[[variablex]] <- (append(new_codes, "sum"))
      dimnames(tab$`within row`$table)[[variablex]] <- new_codes
      dimnames(tab$`within col`$table)[[variablex]]<- append(new_codes, "sum")
      dimnames(tab$frequency$crosstab)[[variablex]] <- (append(new_codes, "sum"))
      dimnames(tab$`within row`$crosstab)[[variablex]] <- new_codes
      dimnames(tab$`within col`$crosstab)[[variablex]]<- append(new_codes, "sum")
    }
  }
  
  sheet2 <- codec$SheetName[codec$Variable == variabley]
  if(!is.na(sheet2))
  {
    if (sheet2 != "none") 
    {
      codingy <- read_excel(code_filename, sheet = sheet2)
      
      codes_to_get <- unlist(dimnames(tab$frequency$table)[2])
      codes_to_get <- codes_to_get[1:(length(codes_to_get)-1)]
      codes_to_get <- as.integer(codes_to_get)
      new_codes <- vector()
      for (i in 1:length(codes_to_get))
      {
        val <- codingy$Value[codingy$Code == codes_to_get[i]]
        new_codes <- append(new_codes, val)
      } 
      
      dimnames(tab$frequency$table)[[variabley]] <- append(new_codes, "sum")
      dimnames(tab$`within row`$table)[[variabley]] <- append(new_codes, "sum")
      dimnames(tab$`within col`$table)[[variabley]] <- new_codes
      dimnames(tab$frequency$crosstab)[[variabley]] <- append(new_codes, "sum")
      dimnames(tab$`within row`$crosstab)[[variabley]] <- append(new_codes, "sum")
      dimnames(tab$`within col`$crosstab)[[variabley]] <- new_codes
    }
  }
  
  return (tab)
}

decodexyz <- function(code_filename, codec, tab, row_variables, col_variable)
{
  sheet1 <- codec$SheetName[codec$Variable == col_variable]
  if(!is.na(sheet1))
  {
    if (sheet1 != "none") 
    {
      codingcol <- read_excel(code_filename, sheet = sheet1)
      
      codes_to_get <- dimnames(tab$table)[[col_variable]]
      codes_to_get <- codes_to_get[1:(length(codes_to_get)-1)]
      codes_to_get <- as.integer(codes_to_get)
      new_codes <- vector()
      
      for (i in 1:length(codes_to_get))
      {
        val <- codingcol$Value[codingcol$Code == codes_to_get[i]]
        new_codes <- append(new_codes, val)
      } 
      
      dimnames(tab$table)[[col_variable]] <- (append(new_codes, "sum"))
      dimnames(tab$crosstab)[[col_variable]] <- (append(new_codes, "sum"))
    }
  }
  
  
  for (row_variable in row_variables)
  {
    sheet2 <- codec$SheetName[codec$Variable == row_variable]
    if(!is.na(sheet2))
    {
      if (sheet2 != "none") 
      {
        codingy <- read_excel(code_filename, sheet = sheet2)
        
        codes_to_get <- dimnames(tab$table)[[row_variable]]
        
        if (row_variable == row_variables[length(row_variables)])
        {
          codes_to_get <- codes_to_get[1:(length(codes_to_get)-1)]
        } 
        
        codes_to_get <- as.integer(codes_to_get)
        new_codes <- vector()
        
        for (i in 1:length(codes_to_get))
        {
          val <- codingy$Value[codingy$Code == codes_to_get[i]]
          new_codes <- append(new_codes, val)
        } 
        
        if (row_variable == row_variables[length(row_variables)]) {
          dimnames(tab$table)[[row_variable]] <- append(new_codes, "sum")
          dimnames(tab$crosstab)[[row_variable]] <- append(new_codes, "sum")
        } else {
          dimnames(tab$table)[[row_variable]] <- new_codes
          dimnames(tab$crosstab)[[row_variable]] <- new_codes
        }
      }
    }
  }
  
  return (tab)
}
