create_dashboard <- function(survey_data) {
  
  ui <- dashboardPage(
    dashboardHeader(title = "VRSurveyR Output"),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
        box(
          selectInput("variable1", "variable 1: ",
                      c("None", colnames(survey_data))
          ) # selectInput
          ,selectInput("type1", "type 1: ",
                      c("Numerical", "Categorical")
          ) # selectInput
          ,selectInput("variable2", "variable 2: ",
                       c("None", colnames(survey_data))
          ) # selectInput
          ,selectInput("type2", "type 2: ",
                       c("Numerical", "Categorical")
          ) # selectInput
        ) # box
      ), # fluidRow
      fluidRow(
        box(
          div(style = "overflow-x:scroll;",
            tableOutput("tabulation")
          ) # div
          , id = "table_box"
        ) # box
      )
    ) # dashboardBody
  ) # dashboardPage
  
  server <- function(input, output)
  {
    output$tabulation <- renderTable(
      {
        create_table(survey_data,
                     input$variable1, 
                     input$variable2)
      }
    )
    
    box_width <- reactive({
      get_width(survey_data, input$variable1, input$variable2)
    })
    
    updateBox(
      "table_box", 
      action = "update", 
      options = list(
        title = h2("Tabulation"),
        width = box_width
      )
    )

  }
  
  shinyApp(ui, server)
  
}

create_table <- function(survey_data, variable1, variable2)
{
  if(variable1 == "None" & variable2 == "None")
  {
    return(data.frame(Error="No variable selected"))
  }
  if(variable1 == "None" & variable2 != "None")
  {
    tab <- as.data.frame(tabulation(survey_data, variable2))
    return(tab)
  }
  if(variable1 != "None" & variable2 == "None")
  {
    tab <- as.data.frame(tabulation(survey_data, variable1))
    return(tab)
  }
  if(variable1 != "None" & variable2 != "None")
  {
    tab <- cross_tabulation(survey_data, variable1, variable2)$frequency$table
    tab <- as.data.frame.matrix(tab)
    return(tab)
  }
}

get_width <- function(survey_data, variable1, variable2)
{
  if((variable2 != "None") & (variable1 != "None"))
  {
    len <- length(table(survey_data[[variable1]]))*3
    return(len)
  }
  return(6)
}
