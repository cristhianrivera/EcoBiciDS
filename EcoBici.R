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


data <- read.csv(file= "C:/Users/a688291/Downloads/Personal/ecobici/2017-01.csv",
                 header = TRUE, 
                 sep=",")

#Let's take a look into the data
View(head(data))
summary(data)

hist(data$Ciclo_Estacion_Arribo)
hist(data$Ciclo_Estacion_Retiro)

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
  summarise(trips= sum(!is.na(Fecha_Retiro_correct))) %>%
  filter(Fecha_Retiro_correct >= as.Date('2017-01-01'))

#We can now plot the data, for further analysis let's wrap it by sex
p <- ggplot(data = tripsPerDay, aes(x = Fecha_Retiro_correct, y = trips)) + 
  geom_line(group=1) +
  geom_point()+ 
  facet_wrap(~Genero_Usuario, ncol=1)
  
ggplotly(p)

#autoarima 
View(tripsPerDay)

ts_M <- tripsPerDay %>%
  filter(Genero_Usuario == 'M', Fecha_Retiro_correct >= as.Date('2017/01/01'))

ts_F <- tripsPerDay %>%
  filter(Genero_Usuario == 'F', Fecha_Retiro_correct >= as.Date('2017/01/01'))

ts_M <- xts(ts_M$trips, as.Date(ts_M$Fecha_Retiro_correct))
ts_F <- xts(ts_F$trips, as.Date(ts_F$Fecha_Retiro_correct))

fit_auto_M <- auto.arima(ts_M)
fit_auto_F <- auto.arima(ts_F)

print(fit_auto_M)
print(fit_auto_F)

plot(forecast(fit_auto_M, h = 7))
plot(forecast(fit_auto_F, h = 7))


#We will get to (hopefuly) the same results by analyzing the ACF and PACF
#as we are going to be looking the ACF and PACF, let's create a function that shows both charts at a glance
TSAnalysis <- function(ts, lags=10){
  par(mfrow=c(3, 1))
  plot.ts(ts,  main = "", xlab = "", type = "o")
  ts <- na.omit(ts)
  forecast::Acf(ts,lags, main = "", xlab = "")
  forecast::Pacf(ts,lags, main = "")
}

#one of the points I wanted to check was the decision making from the auto.arima machinery.
#For the "manual" model selection, we must be sure that the model has a constant variance through the 
#time and it has no trend, although we can deal with that using the I part of an ARIMA model

log_ts_M <- log(ts_M+1)

TSAnalysis(log_ts_M, 20)

log_ts_M.7 <- diff(log_ts_M,7)

#From the ACF we can see some big values on the 7th lag, this suggests to include a 7th order ar
TSAnalysis(log_ts_M.7, 20)


(fit_M <- arima(log_ts_M, order = c(2,0,0), seasonal=list(order=c(0,1,0),period = 7)) )

#Let's take a look at the residuals
plot(fit_M$residuals)
summary(fit_M$residuals)

#Now, we would expect that the ACF and PACF values show no more correlation over the lags
TSAnalysis(fit_M$residuals)
#Finally we should ensure that the residuals come from a normal distribution
qqnorm(residuals(fit_M)); qqline(residuals(fit_M))

#So far we have selected an ARIMA(2,0,0)(0,1,0)[7] given the ACF and PACF values,
#then we took a look at the residuals:
#  no trend
#  constant variance
#  normal distributed


#Finally, let's produce a forecast for 7 days
par(mfrow=c(2, 1))
plot(forecast(fit_M, h=7))

fit_M.forecast <- forecast(fit_M, h=7)

#One of the metrics I have use over the years is the MAPE (mean average percentage error)
#this tells us how far is our prediction from the actual value of the series
mape <- function(ori_vl, for_vl){
  ori <- ori_vl
  fore <- for_vl
  nvl<-length(ori)
  mp <- 0
  for (i in 1:nvl){
    mp <- mp + abs((ori[i]-fore[i])/ori[i])
  }
  return(100 * mp/nvl)
}

ori = exp(as.double(fit_M.forecast$x))-1
fore = exp(as.double(fit_M.forecast$fitted))-1
mape(ori, fore)

#Finally, this is how it looks
ts_df <- data.frame( 
  dt = seq(from= base::as.Date('2017/01/01'), to= base::as.Date('2017/01/31'), by = 1),
  lnori = as.double(fit_M.forecast$x),
  lnfore = as.double(fit_M.forecast$fitted),
  ori = ori,
  fore = fore )

p <-ggplot(ts_df)+
  geom_line(aes(x = dt,
                y = ori))+
  geom_line(aes(x = dt,
                y = fore ),
            col = 'light green')+
  labs(title ='EcoBici', x = 'date', y ="")+
  scale_y_continuous(label=comma)+
  theme_light()
  
ggplotly(p)




##################
#So far the approach to compute a forecast has been fully oriented to ARIMA models and we could answer questions
#like, how many trips are going to be performed the following days?,
#how many users by gender are going to use the EcoBici system in the next week?, etc
#Now what if we would like to know the how many bikes are going to be needed at an specific location 
#for the following days?
#Let's change our approach to a Machine Learning technique. 




head(data)
data_day_station <- data %>%
  group_by(Fecha_Retiro_correct, Ciclo_Estacion_Retiro) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro_correct))) %>%
  filter(Fecha_Retiro_correct >= as.Date('2017-01-01'))

#rpivotTable(data_day_station)
head(data_day_station)

#The next histogram will show us the distribution of the number of trips per day
hist(data_day_station$trips,200)
#Not too much to talk about this, but it could maybe open the discussion about the geographic distribution of 
#some stations.... for example:

(data_day_station.5 <- data_day_station%>%
  filter(trips<5)%>%
  ungroup()%>%
  summarise(n_ciclo = n_distinct(Ciclo_Estacion_Retiro)))

#Let's continue with the data wrangling, we want to predict one week of trips given 3 weeks.
#AQUI DEBER?AS PONER C?MO VA A RECIBIR LA INFORMACI?N KERAS

#we need to create an array with tha shape: (#Stations, #days, #trips)

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

#with more information this is how the data looks like:
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


#DECRIPCION DE C?MO VAS A TRANSFORMAR LA INFORMACI?N

data_day_station <- data %>%
  group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01')) #%>%
  #filter(Fecha_Retiro <= as.Date('2017-03-25'))



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
#Our data will have the shape (77X448,    28,         1    )
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


View(stations)
#Fill the array with data
# rowN <- 0
# for(i_s in 1:448){
#   print(i_s)
#   loop_station <- as.numeric(stations[i_s,1])
#   data_station <- ToLSTM %>%
#     filter(Ciclo_Estacion_Retiro == loop_station) %>%
#     select(Fecha_Retiro, trips, weekday) %>%
#     arrange(Fecha_Retiro)
#   
#   for(k_w in 1:56){
#     rowN <- rowN + 1
#     dq <- dates_retiro$Fecha_Retiro[k_w]
#     
#     for(j in 1:days){
#       week_d <- as.Date(dq) + (j-1)
#       data_point <- data_station %>%
#         ungroup()%>%
#         filter(Fecha_Retiro == as.Date(week_d)) %>%
#         select(trips)
#       data_day_point <- data_station %>%
#         ungroup()%>%
#         filter(Fecha_Retiro == as.Date(week_d)) %>%
#         select(weekday)
#       data_point <- as.numeric(data_point)
#       data_point <- ifelse(!is.na(data_point), data_point, 0)
#       data_array[[rowN , j , 1 ]] <- data_point
#       data_array[[rowN , j , 2 ]] <- data_point
#     }
#   }
# }

##########################################################
##########################################################
startDate <- dim(dates_retiro)[1] - 28
rowN <- 0
for(i_s in 1:448){
  # i_s <- 444 ########
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

#Split into test and training data


set.seed(12345)
nt <- floor(dim(data_array)[1]*0.75)
train <-sample(dim(data_array)[1], nt )
#divisors(20832)


x_train <- data_array[ train , 1:21,  ]
#x_train <- array_reshape(x = x_train, dim = list(nt, 21, 2))
y_train <- data_array[ train , 22:28, 1]

x_test <- data_array[ -train , 1:21,  ]
#x_test <- array_reshape(x = x_test, dim = list(dim(data_array)[1]-nt, 21, 2) )
y_test <- data_array[ -train, 22:28, 1]


dim(x_train)
dim(y_train)

dim(x_test)
dim(y_test)
#divisors(25872)
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

#Prediction and compasiron with the actual values
predicted_output <- model %>% 
  predict(x_test, batch_size = batch_size)

dim(x_test)
dim(y_test)
dim(predicted_output)

#loop to check how the predicted value looks like
for(rtc in 1:20){
  #rtc<-5
  plotfun <- as.data.frame(
    cbind(ts = seq(from = 1, to= 7, by= 1),
          pred = predicted_output[rtc*4,], 
          real = as.numeric(y_test[rtc*4,]) ))
  
  plotfun <- reshape2::melt(plotfun, id="ts")
  
  p <- ggplot(data = plotfun,
         aes(x = ts, 
             y = value, 
             colour = variable)) +
    expand_limits(y=c(0,300))+
    geom_line()
  print(p)
  Sys.sleep(2.2)}

# model %>% compile(loss = 'mse', optimizer = 'rmsprop')
# 
# cat('Training\n')
# for (i in 1:epochs) {
#   print(paste("Real epoch: ", as.character(i)))
#   model %>% fit(x_train, y_train, batch_size = batch_size,
#                 epochs = 1, verbose = 1, shuffle = TRUE)
#   plot(model)
#   model %>% reset_states()
# }


# Your train matrix should be 3-dimensional (samples, timesteps, features). Then you have to use 2nd and 3rd dimensions for input_shape:
#   
#   input_shape = c(dim(X_train_scaled)[2], dim(X_train_scaled)[3])
# Also, number of rows in your dataset is samples, not timesteps. You can read more about samples, timesteps and features here.



