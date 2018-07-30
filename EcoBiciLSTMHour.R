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
library(forecast)
library(xts)
library(numbers)
library(keras)



#path = 'C:/Users/a688291/Downloads/Personal/ecobici/'
path = '/Users/Cristhian/Documents/EcoBici/ecobici/'


data_201701 <- read.csv(file = paste(path,"2017-01.csv", sep = ""),
                        header = TRUE, 
                        sep=",")

# data_201702 <- read.csv(file = paste(path,"2017-02.csv", sep = ""),
#                         header = TRUE, 
#                         sep=",")
# 
# data_201703 <- read.csv(file = paste(path,"2017-03.csv", sep = ""),
#                         header = TRUE, 
#                         sep=",")


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



data <- data_201701 %>%
  select(-X)

# 
# data_201701 <- data_201701 %>%
#   select(-X)
# 
# data <- rbind(data_201701,data_201702)
# data <- rbind(data,data_201703)
# 


data$Fecha_Retiro <- as.Date(data$Fecha_Retiro,"%d/%m/%Y")
data$Fecha_Arribo <- as.Date(data$Fecha_Arribo,"%d/%m/%Y")


#transformation to get hours and days together
data <- data %>%
  mutate(dt_time = ymd_hms(paste(Fecha_Retiro, Hora_Retiro))) %>%
  mutate(h = floor_date(dt_time, unit = "hours")) %>%
  mutate(Hora_Retiro_Gut = hour(as_datetime( Hora_Retiro)))

View(head(data))

data_day <- data %>%
  group_by(h, Genero_Usuario) %>%
  summarise(trips= sum(!is.na(h))) %>%
  filter(h >= as.Date('2017-01-01'))

data_hour <- data %>%
  group_by(h, Genero_Usuario) %>%
  summarise(trips= sum(!is.na(h))) %>%
  filter(h >= as.Date('2017-01-01'))

data_day %>%
  ungroup() %>%
  group_by(Genero_Usuario) %>%
  summarise(trips= sum(trips))

p <- ggplot(data = data_day, aes(x = h, y = trips)) + 
  geom_line(group=1) +
  geom_point()+
  xlab("Date")+
  ylab("Number of trips")+
  facet_wrap(~Genero_Usuario, ncol=1)
ggplotly(p)



#how many datetime point do we have per day?
datetime <- data%>%
    ungroup()%>%
    group_by(Fecha_Retiro) %>%
    summarise(hours_per_day = n_distinct(h))

#We have 20 different hours for every day, we will need to create one-hot encoding for this variable

#Steps for data transformation:
#1- Filter to have just data from 2017
#2- Summarize data by station and datetime
#3- Create dictionary of datetime
#4- Create dictionary of stations

#Steps 1 and 2
ToLSTM <- data %>%
  group_by(h, Ciclo_Estacion_Retiro) %>%
  summarise(trips= sum(!is.na(h))) %>%
  filter(h >= as.Date('2017-01-01'))


#Dictionary of stations
(stations <- ToLSTM%>%
    ungroup()%>%
    distinct(Ciclo_Estacion_Retiro))
View(stations)
#Dictionary of dates# poner one hot
(dates_retiro <- ToLSTM%>%
    ungroup()%>%
    distinct(h)%>%
    mutate(pivot_h = as.numeric(hour(as.Date(h))))%>%
    select(-Fecha_Retiro))

mutate(dates_retiro,hh = time(as.Date(h)) )
#to calculate the total number of rows for our array 
#data (points_per_day * days_in_total - length_input_and_output) * number_stations

(hours <- 120)
(n <- (20*31 - 120) * 447)
##                                            n, hours, trips + one_hot_encoding_hour_day
data_array <- array(data = numeric(n), dim = c(n, hours, 1 + 20))
hour_array <- array(data = numeric(20), dim = c(20,20))
for(i in 1:20){
  hour_array[i,i]=1
}


#Fill the data_array

##########################################################
##########################################################
startDate <- dim(dates_retiro)[1] - 28
rowN <- 0
for(i_s in 1:447){
  # i_s <- 444 ########
  print(i_s)
  loop_station <- as.numeric(stations[i_s,1])
  
  data_station <- ToLSTM %>%
    filter(Ciclo_Estacion_Retiro == loop_station) %>%
    select(h, trips, weekday) %>%
    arrange(h)
  
  dtToArray <- dates_retiro %>% 
    left_join(data_station, by = 'Fecha_Retiro' )
  
  dtToArray <- dtToArray %>%
    mutate(trips = ifelse(!is.na(trips), trips, 0),
           weekday = ifelse(!is.na(weekday), weekday, pivot_wk))%>%
    select(Fecha_Retiro, trips, weekday)
  
  for(k_w in 1:startDate){
    # k_w <- 1 ########
    rowN <- rowN + 1
    dq <- dates_retiro$Fecha_Retiro[k_w]
    dt_from <- as.Date(dq)
    dt_to   <- as.Date(dq) + 27
    data_point <- dtToArray %>%
      ungroup()%>%
      filter(Fecha_Retiro >= as.Date(dt_from) & Fecha_Retiro <= as.Date(dt_to)) %>%
      select(trips, weekday)
    
    data_array[rowN ,  , 1 ] <- dplyr::pull(data_point,trips)
    data_array[rowN ,  , -1 ] <- week_array[ as.numeric(dplyr::pull(data_point,weekday)), ]
  }
}


##########################################################
##########################################################




