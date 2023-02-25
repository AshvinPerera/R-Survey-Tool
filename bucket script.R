create_age_bucket <- function(survey_data, column_index, variable) 
{
  survey_data[, variable] <- 0
  survey_data[(survey_data[,column_index] >= 0 & survey_data[,column_index] <= 19), variable] <- 1
  survey_data[(survey_data[,column_index] >= 20 & survey_data[,column_index] <= 29), variable] <- 2
  survey_data[(survey_data[,column_index] >= 30 & survey_data[,column_index] <= 39), variable] <- 3
  survey_data[(survey_data[,column_index] >= 40 & survey_data[,column_index] <= 49), variable] <- 4
  survey_data[(survey_data[,column_index] >= 50 & survey_data[,column_index] <= 59), variable] <- 5
  survey_data[(survey_data[,column_index] >= 60), variable] <- 6
  
  return(survey_data)
}

create_industry_bucket1 <- function(survey_data, column_index, variable)
{
  
}

create_industry_bucket2 <- function(survey_data, column_index, variable)
{
  Agri <- A
  Manufacture <- vector()
  for (letter in c(B, C)) {
    Manufacture <- append(Manufacture, letter)
  }
  Service <- vector()
  for (letter in c(D, E, Ff, G, H, I, J, K, L, M, N, O, P, Q, R, S)) {
    Service <- append(Service, letter)
  }
}