library(rpivotTable)
library(tibble)
library(chron)
library(lubridate)
library(zoo)
library(dplyr)
library(reshape)
library(reshape2)
library(plotly)
library(stringr)



data <- read.csv(file= "C:/Users/a688291/Downloads/Personal/ecobici/2017-01.csv",
                 header = TRUE, 
                 sep=",")

#Let's take a look into the data
head(data)
summary(data)

#we would like to have the dates with a date format. After trying to do so, I got some errors and found
#that the format was not consistent through all the dataset
#Let's see the different data that the column Fecha_Retiro has.

data_dates = data %>% 
  distinct(Fecha_Retiro)

View(data_dates)

#let's apply the required transformations so we can give a date format
data_dates = data_dates %>%
  mutate(Fecha_Retiro_correct = case_when(
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
    TRUE ~ as.character(data_dates$Fecha_Retiro )
    ))

View(data_dates)
#everything looks good now. Let's appply the same transformation for Fecha_Arribo

data_dates = data %>% 
  distinct(Fecha_Arribo)

data_dates = data_dates %>%
  mutate(Fecha_Arribo_correct = case_when(
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
    TRUE ~ as.character(data_dates$Fecha_Arribo )
  ))


View(data_dates)

#We are ready to apply the same transformation to the complete dataset

data <- data %>%
  mutate(Fecha_Retiro_correct = case_when(
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
    TRUE ~ as.character(data$Fecha_Retiro )
  )) %>%
  mutate(Fecha_Arribo_correct = case_when(
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
    TRUE ~ as.character(data$Fecha_Arribo )
  ))
  

data$Fecha_Retiro_correct <- as.Date(data$Fecha_Retiro_correct,"%d/%m/%Y")
data$Fecha_Arribo_correct <- as.Date(data$Fecha_Arribo_correct,"%d/%m/%Y")



tripsPerDay <- data %>%
  group_by(Fecha_Retiro_correct,Genero_Usuario) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro_correct)))


p <- ggplot(tripsPerDay)+
  geom_line(aes(x = Fecha_Retiro_correct, y = trips))+
  facet_wrap(~Genero_Usuario)
  theme_minimal()

p <- ggplot(data = tripsPerDay, aes(x = Fecha_Retiro_correct, y = trips)) + geom_point()
p + facet_wrap(~Genero_Usuario)
  
str(tripsPerDay)
ggplotly(p)

summary(data)




