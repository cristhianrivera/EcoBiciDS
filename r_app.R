library(shiny)
library(keras)
library(plotly)
library(ggplot2)

ui <- fluidPage(
  numericInput(inputId = 'station', 
               label = 'Station', 
               value = 3, 
               min = 1, 
               max = 480),
  plotlyOutput("forecast")
)

server <- function(input, output) {
  path = 'C:/Users/a688291/Downloads/Personal/ecobici/'
  #path = '/Users/Cristhian/Documents/EcoBici/ecobici/'
  data_201701 <- read.csv(file = paste(path,"2017-01.csv", sep = ""),
                          header = TRUE, 
                          sep=",")
  
  data_201702 <- read.csv(file= paste(path,"2017-02.csv", sep = ""),
                          header = TRUE, 
                          sep=",")
  
  data_201703 <- read.csv(file= paste(path,"2017-03.csv", sep = ""),
                          header = TRUE, 
                          sep=",")
  
  data_201701 <- data_201701 %>%
    mutate(Fecha_Retiro = case_when(
      Fecha_Retiro == '01/02/2017' ~ '02/01/2017',
      Fecha_Retiro == '01/03/2017' ~ '03/01/2017',
      Fecha_Retiro == '01/04/2017' ~ '04/01/2017',
      Fecha_Retiro == '01/05/2017' ~ '05/01/2017',
      Fecha_Retiro == '01/06/2017' ~ '06/01/2017',
      Fecha_Retiro == '01/07/2017' ~ '07/01/2017',
      Fecha_Retiro == '01/08/2017' ~ '08/01/2017',
      Fecha_Retiro == '01/09/2017' ~ '09/01/2017',
      Fecha_Retiro == '01/10/2017' ~ '10/01/2017',
      Fecha_Retiro == '01/11/2017' ~ '11/01/2017',
      Fecha_Retiro == '01/12/2017' ~ '12/01/2017',
      TRUE ~ as.character(data_201701$Fecha_Retiro )
    )) %>%
    mutate(Fecha_Arribo = case_when(
      Fecha_Arribo == '01/02/2017' ~ '02/01/2017',
      Fecha_Arribo == '01/03/2017' ~ '03/01/2017',
      Fecha_Arribo == '01/04/2017' ~ '04/01/2017',
      Fecha_Arribo == '01/05/2017' ~ '05/01/2017',
      Fecha_Arribo == '01/06/2017' ~ '06/01/2017',
      Fecha_Arribo == '01/07/2017' ~ '07/01/2017',
      Fecha_Arribo == '01/08/2017' ~ '08/01/2017',
      Fecha_Arribo == '01/09/2017' ~ '09/01/2017',
      Fecha_Arribo == '01/10/2017' ~ '10/01/2017',
      Fecha_Arribo == '01/11/2017' ~ '11/01/2017',
      Fecha_Arribo == '01/12/2017' ~ '12/01/2017',
      TRUE ~ as.character(data_201701$Fecha_Arribo )
    ))
  data_201701 <- data_201701 %>%
    select(-X)
  data <- rbind(data_201701,data_201702)
  data <- rbind(data,data_201703)
  data$Fecha_Retiro <- as.Date(data$Fecha_Retiro,"%d/%m/%Y")
  data$Fecha_Arribo <- as.Date(data$Fecha_Arribo,"%d/%m/%Y")
  
  
  
  output$hist <- renderPlotly({
    hi <- hist(rnorm(input$num),breaks=input$num_breaks)
    hi <- cbind(data.frame(hi$counts), data.frame(hi$breaks[-1]))
    colnames(hi)<-c('counts','breaks')
    print(ggplotly(
      ggplot(data=hi,aes(x=breaks,y=counts))+
        geom_bar(stat="identity"))
    )
  })
}

shinyApp(ui = ui, server = server)
