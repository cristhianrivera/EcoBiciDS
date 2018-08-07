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



path = 'C:/Users/a688291/Downloads/Personal/ecobici/'
#path = '/Users/Cristhian/Documents/EcoBici/ecobici/'


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

View(data)

#transformation to get hours and days together
data <- data %>%
  mutate(dt_time = ymd_hms(paste(Fecha_Retiro, Hora_Retiro))) %>%
  mutate(h = floor_date(dt_time, unit = "hours")) 
  #%>% mutate(Hora_Retiro_Gut = hour(as_datetime( Hora_Retiro)))

#View(data)

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

p<- ggplot(data = data_day,
       aes(x = h, 
           y = trips, 
           colour = Genero_Usuario )) +
  geom_line(group=1) +
  geom_point()+
  xlab("Date")+
  ylab("Number of trips")

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

#View(ToLSTM)
#Dictionary of stations
(stations <- ToLSTM%>%
    ungroup()%>%
    distinct(Ciclo_Estacion_Retiro))
n_stations <- length(stations$Ciclo_Estacion_Retiro) ##might be the answer to control the amount of information

#View(stations)
#Dictionary of dates# poner one hot
(dates_retiro <- ToLSTM%>%
    ungroup()%>%
    distinct(h))

dates_retiro <- cbind(dates_retiro,data.frame(seq(from = 1, to = length(dates_retiro$h), by = 1) ))

colnames(dates_retiro) <- c('h','numberDataHour')

dates_retiro <- dates_retiro %>%
  mutate(number_hour = as.numeric(format(h, "%H")),
         weekday = as.numeric(format(as.Date(h),"%u"))) %>%
  mutate(number_hour = ifelse(number_hour==0, number_hour+1, number_hour-3) )#to keep the range between 1-20

#View(dates_retiro)

#to calculate the total number of rows for our array 
#data (points_per_day * days_in_total - length_input_and_output) * number_stations

(hours <- 60)
(n <- (20 * 31 - hours) * n_stations)
## n, hours, trips + one_hot_encoding_hour_day + one_oht_encoding_weekday
data_array <- array(data = numeric(n)  , dim = c(n, hours, 1 + 20 + 7))
hour_array <- array(data = numeric(20) , dim = c(20,20))
week_array <- array(data = numeric(7)  , dim = c(7,7))


for(i in 1:20){
  hour_array[i,i]=1
}

for(i in 1:7){
  week_array[i,i]=1
}



#Fill the data_array

##########################################################
##########################################################
#startDate <- dim(dates_retiro)[1] - 28
rowN <- 0
for(i_s in 1:n_stations){
  #i_s <- 1 ########
  print(i_s)
  loop_station <- as.numeric(stations[i_s,1])
  
  data_station <- ToLSTM %>%
    filter(Ciclo_Estacion_Retiro == loop_station) %>%
    select(h, trips) %>%
    arrange(h)
  
  dtToArray <- dates_retiro %>% 
    left_join(data_station, by = 'h' ) #To fill with zeros the hours with no data
  
  dtToArray <- dtToArray %>%
    mutate(trips = ifelse(!is.na(trips), trips, 0))
  
  for(k_w in 1:(620 - 60  )){  # until the last 60 window of hourly trips
    #k_w <- 561 ########
    rowN <- rowN + 1
    
    numberDataHour_from <- k_w
    numberDataHour_to   <- k_w + 59
    data_point <- dtToArray %>%
      ungroup()%>%
      filter(numberDataHour >= numberDataHour_from & numberDataHour <= numberDataHour_to ) %>%
      select(trips, weekday, number_hour)
    
    data_array[rowN ,  , 1 ]    <- dplyr::pull(data_point,trips)
    data_array[rowN ,  , 2:21 ]  <- hour_array[ as.numeric(dplyr::pull(data_point,number_hour)), ]
    data_array[rowN ,  , 22:28 ] <- week_array[ as.numeric(dplyr::pull(data_point,weekday)), ]
  }
}


##########################################################
##########################################################



set.seed(12345)
nt <- floor(dim(data_array)[1]*0.75)
train <-sample(dim(data_array)[1], nt )
# divisors(nt)
# divisors(62580)

x_train <- data_array[ train , 1:40 ,  ]
y_train <- data_array[ train , 41:60, 1]

x_test <- data_array[ -train , 1:40 ,  ]
y_test <- data_array[ -train , 41:60, 1]


dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)

batch_size <- 42 #28, 70
epochs <- 10

cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 75, 
             input_shape = c( 40, 28), 
             batch_size = batch_size,
             return_sequences = FALSE, 
             stateful = TRUE) %>% 
  layer_dropout(rate = 0.5) %>%
  #layer_lstm(units = 75, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 20)
summary(model)

rmsprop <- optimizer_rmsprop(lr=0.005)
adm <- optimizer_adam(lr=0.0005)

model %>% compile(loss = 'mse', 
                  optimizer = adm,
                  metrics = c('mse')
)
history <- model %>% fit(
  x_train, y_train, 
  batch_size = batch_size,
  epochs = epochs, 
  verbose = 1, 
  validation_data = list(x_test, y_test),
  shuffle = TRUE)

plot(history)
history$metrics

#Predict values
predicted_output <- model %>% 
  predict(x_test, batch_size = batch_size)

