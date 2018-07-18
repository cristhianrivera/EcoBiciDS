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
#AQUI DEBERÍAS PONER CÓMO VA A RECIBIR LA INFORMACIÓN KERAS

#we need to create an array with tha shape: (#Stations, #days, #trips)



data_201701 <- read.csv(file= "C:/Users/a688291/Downloads/Personal/ecobici/2017-01.csv",
                 header = TRUE, 
                 sep=",")

data_201702 <- read.csv(file= "C:/Users/a688291/Downloads/Personal/ecobici/2017-02.csv",
                        header = TRUE, 
                        sep=",")

data_201703 <- read.csv(file= "C:/Users/a688291/Downloads/Personal/ecobici/2017-03.csv",
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
  group_by(Fecha_Retiro) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01'))

p <- ggplot(data = data_day, aes(x = Fecha_Retiro, y = trips)) + 
  geom_line(group=1) +
  geom_point()
ggplotly(p)

#DECRIPCIÓN DE CÓMO VAS A TRANSFORMAR LA INFORMACIÓN

data_day_station <- data %>%
  group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01'))

ToLSTM <- data_day_station%>%
  filter(Ciclo_Estacion_Retiro==1)%>%
  mutate(weekNumber = strftime(Fecha_Retiro))

View(head(filter(data_day_station,Ciclo_Estacion_Retiro==1), 100))



tripsPerDay <- data %>%
  group_by(Fecha_Retiro,Genero_Usuario) %>%
  summarise(trips= sum(!is.na(Fecha_Retiro))) %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01'))

p <- ggplot(data = tripsPerDay, aes(x = Fecha_Retiro, y = trips)) + 
  geom_line(group=1) +
  geom_point()+ 
  facet_wrap(~Genero_Usuario, ncol=1)

ggplotly(p)



data_dates = data_day_station %>% 
  distinct(Fecha_Retiro)








arr_data <- array(data = numeric(n), dim = c(n, 1, 1))

(n_stations <- data_day_station%>%
    ungroup()%>%
    summarise(n_ciclo = n_distinct(Ciclo_Estacion_Retiro)))


numeric(3)

data_day_station





filter(data_day_station, trips<5)
data_day_station$Fecha_Retiro_correct

#LSTM with keras
#install.packages("devtools")
#devtools::install_github("rstudio/keras")
#install_keras()
library(keras)


imdb <- dataset_imdb(num_words = 20000)
x_train <- imdb$train$x
y_train <- imdb$train$y
x_test <- imdb$test$x
y_test <- imdb$test$y

x_train <- pad_sequences(x_train, maxlen = 80)
dim(x_train)
# since we are using stateful rnn tsteps can be set to 1
tsteps <- 1
batch_size <- 25
epochs <- 2
# number of elements ahead that are used to make the prediction
lahead <- 1

# Generates an absolute cosine time series with the amplitude exponentially decreasing
# Arguments:
#   amp: amplitude of the cosine function
#   period: period of the cosine function
#   x0: initial x of the time series
#   xn: final x of the time series
#   step: step of the time series discretization
#   k: exponential rate
gen_cosine_amp <- function(amp = 100, period = 1000, x0 = 0, xn = 50000, step = 1, k = 0.0001) {
  n <- (xn-x0) * step
  cos <- array(data = numeric(n), dim = c(n, 1, 1))
  for (i in 1:length(cos)) {
    idx <- x0 + i * step
    cos[[i, 1, 1]] <- amp * cos(2 * pi * idx / period)
    cos[[i, 1, 1]] <- cos[[i, 1, 1]] * exp(-k * idx)
  }
  cos
}

cat('Generating Data...\n')
cos <- gen_cosine_amp()
cat('Input shape:', dim(cos), '\n')

expected_output <- array(data = numeric(length(cos)), dim = c(length(cos), 1))
for (i in 1:(length(cos) - lahead)) {
  expected_output[[i, 1]] <- mean(cos[(i + 1):(i + lahead)])
}

cat('Output shape:', dim(expected_output), '\n')

cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 50, input_shape = c(tsteps, 1), batch_size = batch_size,
             return_sequences = TRUE, stateful = TRUE) %>% 
  layer_lstm(units = 50, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 1)
model %>% compile(loss = 'mse', optimizer = 'rmsprop')

cat('Training\n')
for (i in 1:epochs) {
  model %>% fit(cos, expected_output, batch_size = batch_size,
                epochs = 2, verbose = 1, shuffle = TRUE)
  
  model %>% reset_states()
}



length(cos)
length(expected_output)
cat('Predicting\n')
predicted_output <- model %>% predict(cos, batch_size = batch_size)

cat('Plotting Results\n')
op <- par(mfrow=c(2,1))
plot(expected_output, xlab = '')
title("Expected")
plot(predicted_output, xlab = '')
title("Predicted")
par(op)