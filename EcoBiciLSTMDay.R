###Load all the packages that we will use
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
library(shiny)


####Data preprocessing 
path = "C:/Users/a688291/Downloads/Personal/ecobici/"


data_201701 <- read.csv(file = paste(path,"2017-01.csv", sep = ""),
                        header = TRUE, 
                        sep=",")

data_201702 <- read.csv(file= paste(path,"2017-02.csv", sep = ""),
                        header = TRUE, 
                        sep=",")

data_201703 <- read.csv(file= paste(path,"2017-03.csv", sep = ""),
                        header = TRUE, 
                        sep=",")

#there are some format issues on the dataset
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

#Giving date format to the date column
data$Fecha_Retiro <- as.Date(data$Fecha_Retiro,"%d/%m/%Y")
data$Fecha_Arribo <- as.Date(data$Fecha_Arribo,"%d/%m/%Y")

#Simple plot divided by the gender of the user
data_day <- data %>%
  group_by(Fecha_Retiro, Genero_Usuario) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01'))

data_day %>%
  ungroup() %>%
  group_by(Genero_Usuario) %>%
  summarise(trips= sum(trips))

p <- ggplot(data = data_day, aes(x = Fecha_Retiro, y = trips, colour=Genero_Usuario)) + 
  geom_line(group=1) +
  geom_point()+
  xlab("Date")+
  ylab("Number of trips")#+
  # facet_wrap(~Genero_Usuario, ncol=1)
ggplotly(p)


#Filter the dataset by date, we wanted to consider only 2017
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

inverse_range <- function(min_x, max_x, x){
  (max_x - min_x) * x + min_x
}

# data_day_station <- data %>%
#   filter(Fecha_Retiro >= as.Date('2017-01-01')) %>%
#   group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
#   summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
#   mutate(trips_ranged = range01(trips)) %>%
#   mutate(trips_inversed = inverse_range(min_trips, max_trips, trips_ranged)) 


data_day_station <- data %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01')) %>%
  group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
  summarise(trips = sum(!is.na(Fecha_Retiro)))

# for(i_s in 1:448){
#   i_s = 1##
#   loop_station <- as.numeric(stations[i_s,1])
#   data_station <- ToLSTM %>%
#     filter(Ciclo_Estacion_Retiro == loop_station) %>%
#     select(Fecha_Retiro, trips_normal, weekday) %>%
#     arrange(Fecha_Retiro)
# }

# data_station <- ToLSTM %>%
#   filter(Ciclo_Estacion_Retiro == loop_station) %>%
#   select(Fecha_Retiro, trips, weekday) %>%
#   arrange(Fecha_Retiro)

max_trips <- max(data_day_station$trips)
min_trips <- min(data_day_station$trips)

View(head(data_day_station))

ToLSTM <- data_day_station %>%
  mutate(weekday = format(as.Date(Fecha_Retiro),"%u")) %>%
  mutate(monthday = day(as.Date(Fecha_Retiro))) %>%
  mutate(weeknumber = strftime(as.Date(Fecha_Retiro),"%V"))


#Number of stations?
(n_stations <- ToLSTM%>%
    ungroup()%>%
    summarise(n_ciclo = n_distinct(Ciclo_Estacion_Retiro)))

#Number of dates?
(n_days <- ToLSTM%>%
    ungroup()%>%
    summarise(n_ciclo = n_distinct(Fecha_Retiro)))



#One way we can build 28 data points from  12 total weeks is by selecting groups of 28 consecutive days
#and we can get up to 55 groups of 28 datapoints out of 12 weeks (84 days)
#Our data will have the shape ( 55X448,    28    ,    8    )
#-----------------------------(samples, timesteps, features)
#55 comes from 90-7-28
library(keras)
(n <-55*448)
(days <- 28)
data_array <- array(data = numeric(n), dim = c(n, days, 8))
week_array <- array(data = numeric(7), dim = c(7,7))
for(i in 1:7){
  week_array[i,i]=1
}
#Dictionary of stations
(stations <- ToLSTM%>%
    ungroup()%>%
    distinct(Ciclo_Estacion_Retiro))

#Dictionary of dates# poner one hot
(dates_retiro <- ToLSTM%>%
    ungroup()%>%
    distinct(Fecha_Retiro)%>%
    mutate(pivot_wk = as.numeric(format(as.Date(Fecha_Retiro),"%u"))))

##########################################################
###Fill the array with data
LastStartDate <- dim(dates_retiro)[1] - 28
rowN <- 0
for(i_s in 1:448){
  ##i_s <- 1 ##
  print(i_s)
  loop_station <- as.numeric(stations[i_s,1])
  
  data_station <- ToLSTM %>%
    filter(Ciclo_Estacion_Retiro == loop_station) %>%
    select(Fecha_Retiro, trips, weekday) %>%
    arrange(Fecha_Retiro)
  
  dtToArray <- dates_retiro %>% 
    left_join(data_station, by = 'Fecha_Retiro' )
  
  dtToArray <- dtToArray %>%
    mutate(trips = ifelse(!is.na(trips), trips, 0),
           weekday = ifelse(!is.na(weekday), weekday, pivot_wk),
           tripsD7 = log(trips+1) - lag(log(trips+1),7) ) %>%
    select(Fecha_Retiro, tripsD7, weekday)
  
  # ggplot(dtToArray)+
  #   geom_line(aes(x=Fecha_Retiro,y=tripsD7), group=1)
  
  for(k_w in 8:LastStartDate+1){
    #k_w <- 63
    rowN <- rowN + 1
    dq <- dates_retiro$Fecha_Retiro[k_w]
    dt_from <- as.Date(dq)
    dt_to   <- as.Date(dq) + 27
    data_point <- dtToArray %>%
      ungroup()%>%
      filter(Fecha_Retiro >= as.Date(dt_from) & Fecha_Retiro <= as.Date(dt_to)) %>%
      select(tripsD7, weekday)
    
    data_array[rowN ,  , 1 ] <- dplyr::pull(data_point,tripsD7)
    data_array[rowN ,  , 2:8 ] <- week_array[ as.numeric(dplyr::pull(data_point,weekday)), ]
  }
}

##########################################################
##########################################################

#Split into test and training data

set.seed(12345)
nt <- floor(dim(data_array)[1]*0.75)
train <-sample(dim(data_array)[1], nt )
#divisors(18480) 33 35 40 42 44 48 
#divisors(6160) 28 35 40 44 55 56 

x_train <- data_array[ train , 1:21,  ]
y_train <- data_array[ train , 22:28, 1]

x_test <- data_array[ -train , 1:21,  ]
y_test <- data_array[ -train, 22:28, 1]


dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)
batch_size <- 40
epochs <- 100

cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 100, input_shape = c( 21, 8), batch_size = batch_size,
             return_sequences = FALSE, stateful = FALSE) %>% 
  layer_dropout(rate = 0.5) %>%
  #layer_lstm(units = 75, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 7)
summary(model)

rmsprop <- optimizer_rmsprop(lr=0.005)
adm <- optimizer_adam(lr=0.0005)

model %>% compile(loss = 'mean_absolute_error', 
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



###################################################
#####Model that receives just one instance ########
###################################################

mape <- function(y_true, y_for){
  len <- length(y_for)
  mp <- 0
  for(i in 1:len){
    mp <- mp + (abs(y_true[i]-y_for[i])/y_true[i])
  }
  return(100*mp)
}

#get weigths
mweights <- keras::get_weights(model)

#build the model
modelPredict <- keras_model_sequential()
modelPredict %>%
  layer_lstm(units = 100, input_shape = c( 21, 8), batch_size = 1,
             return_sequences = FALSE, stateful = FALSE) %>% 
  layer_dropout(rate = 0.5) %>%
  #layer_lstm(units = 75, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 7)
summary(modelPredict)

#set weigths from the trained model
keras::set_weights(modelPredict, mweights)

#Get data for a given station i_s
i_s <- 405 # values from 1 to 448
print(i_s)
loop_station <- as.numeric(stations[i_s,1])

data_station <- ToLSTM %>%
  filter(Ciclo_Estacion_Retiro == loop_station) %>%
  select(Fecha_Retiro, trips, weekday) %>%
  arrange(Fecha_Retiro)

dtToArray <- dates_retiro %>% 
  left_join(data_station, by = 'Fecha_Retiro' )

dtToArrayFull <- dtToArray %>%
  mutate(trips = ifelse(!is.na(trips), trips, 0),
         weekday = ifelse(!is.na(weekday), weekday, pivot_wk),
         tripsD7 = log(trips+1) - lag(log(trips+1),7) ) 

dtToArray <- dtToArrayFull %>%
  select(Fecha_Retiro, tripsD7, weekday)

data_point <- dtToArray %>%
  ungroup()%>%
  filter(Fecha_Retiro >= as.Date('2017-03-04') & Fecha_Retiro <= as.Date('2017-03-24')) %>%
  select(tripsD7, weekday)

predict_array <- array(data = numeric(8*21), dim = c(1, 21, 8))
predict_array[1 ,  , 1 ] <- dplyr::pull(data_point,tripsD7)
predict_array[1 ,  , 2:8 ] <- week_array[ as.numeric(dplyr::pull(data_point,weekday)), ]

#predict output just for one instance
predicted_output <- modelPredict %>% 
  predict(predict_array, batch_size = 1)


d_to_complete <- dtToArray %>%
  ungroup()%>%
  filter(Fecha_Retiro >= as.Date('2017-01-01') & Fecha_Retiro <= as.Date('2017-03-24')) %>%
  select(tripsD7)

real <- na.exclude(dtToArray$tripsD7)[1:length(na.exclude(dtToArray$tripsD7))]
pred <- c(d_to_complete$tripsD7, predicted_output)
pred <- na.exclude(pred)[1:length(na.exclude(pred))] 

#Return to unlagged series
real <- diffinv(real, lag=7, differences = 1, xi=log(dtToArrayFull$trips[1:7] +1) )
pred <- diffinv(pred, lag=7, differences = 1, xi=log(dtToArrayFull$trips[1:7] +1) )

(mape(real,pred))

plotfun <- as.data.frame(
  cbind(ts = dtToArray$Fecha_Retiro,
        real = exp(real)-1,
        pred = exp(pred)-1 ))

plotfun <- reshape2::melt(plotfun, id="ts")

p <- ggplot(data = plotfun,
            aes(x = as.Date(ts), 
                y = value, 
                colour = variable )) +
  geom_line()+
  xlab('Time')+
  ylab('Value')+
  theme_minimal()
p


