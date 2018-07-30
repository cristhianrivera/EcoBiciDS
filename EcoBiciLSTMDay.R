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


####Data preprocessing 
path = '/Documents/EcoBici/ecobici/'


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

p <- ggplot(data = data_day, aes(x = Fecha_Retiro, y = trips)) + 
  geom_line(group=1) +
  geom_point()+
  xlab("Date")+
  ylab("Number of trips")+
  facet_wrap(~Genero_Usuario, ncol=1)
ggplotly(p)


#Filter the dataset by date, we wanted to consider only 2017
data_day_station <- data %>%
  group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01')) #%>%



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
#and we can get up to 77 groups of 28 datapoints out of 12 weeks (84 days)
#Our data will have the shape ( 62X448,    28    ,    8    )
#-----------------------------(samples, timesteps, features)
library(keras)
(n <-62*448)
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
startDate <- dim(dates_retiro)[1] - 28
rowN <- 0
for(i_s in 1:448){
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
           weekday = ifelse(!is.na(weekday), weekday, pivot_wk))%>%
    select(Fecha_Retiro, trips, weekday)
  
  for(k_w in 1:startDate){
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

#Split into test and training data

set.seed(12345)
nt <- floor(dim(data_array)[1]*0.75)
train <-sample(dim(data_array)[1], nt )
#divisors(20832)


x_train <- data_array[ train , 1:21,  ]
y_train <- data_array[ train , 22:28, 1]

x_test <- data_array[ -train , 1:21,  ]
y_test <- data_array[ -train, 22:28, 1]


dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)
batch_size <- 32 #56
epochs <- 300

cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 75, input_shape = c( 21, 8), batch_size = batch_size,
             return_sequences = TRUE, stateful = TRUE) %>% 
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 75, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 7)
summary(model)

rmsprop <- optimizer_rmsprop(lr=0.005)
adm <- optimizer_adam(lr=0.0005)

model %>% compile(loss = 'mse', 
                  optimizer = adm#,
                  #metrics = c('mse')
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

#Plot the whole month of a station

i = 1456 #number of row to plot
real <- c(x_test[i,,1],y_test[i,]) 

seqNA <- seq(1,21,1)
seqNA[seqNA != 0] <- NA
predicted <- c(seqNA,predicted_output[i,])

plotfun <- as.data.frame(
  cbind(ts = seq(from=1,to=28,by=1),
        real,
        predicted))

plotfun <- reshape2::melt(plotfun, id="ts")

p <- ggplot(data = plotfun,
            aes(x = ts, 
                y = value, 
                colour = variable )) +
  geom_line()+
  xlab('Time')+
  ylab('Value')+
  theme_minimal()
p