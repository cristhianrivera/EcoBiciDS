data_day_station <- data %>%
  filter(Fecha_Retiro >= as.Date('2017-01-01')) %>%
  group_by(Fecha_Retiro, Ciclo_Estacion_Retiro) %>%
  summarise(trips = sum(!is.na(Fecha_Retiro)))


stations <- data_day_station%>%
    ungroup()%>%
    distinct(Ciclo_Estacion_Retiro)

EDAStation <- function(number_station){
  station <- stations$Ciclo_Estacion_Retiro[number_station]
   
  EDAStations <- data_day_station %>%
    filter(Ciclo_Estacion_Retiro == station ) %>%
    right_join(dates_retiro, by = 'Fecha_Retiro' )%>%
    select(Fecha_Retiro, trips) %>%
    mutate(trips= ifelse(!is.na(trips), trips, 0))%>%
    arrange(Fecha_Retiro)
  
  p <- ggplot(EDAStations)+
    geom_line(aes(x=Fecha_Retiro, y=trips), group=1)
  
  return(variance = var(EDAStations$trips))
  
}


varianceStations <- data.frame(station = numeric(),
                               variance = numeric())

for(i in 1:448){
  varianceStations <- rbind(varianceStations , 
                            data.frame(as.numeric(i), EDAStation(i)) 
                            )
}
colnames(varianceStations)<- c("station","variance")
varianceStations <- as.tbl(varianceStations)

#boxplot to analyze the variance of the number of trips per station
p <- ggplot(varianceStations)+
  geom_boxplot(aes(x='a', y=variance), group =1)

ggplotly(p)

varianceStations <- varianceStations %>%
  mutate(outlier = ifelse(variance>2659,'outlier','normal'))

varianceStations %>%
  group_by(outlier) %>%
  summarise(countOutlier= n())

varianceStations <- varianceStations %>%
  filter(outlier != 'outlier')


#boxplot without outliers
p <- ggplot(varianceStations)+
  geom_boxplot(aes(x='a', y=variance), group = 1)

ggplotly(p)
###############Another approach to identofy outliers. Count number of zeroes


EDAStationZeroes <- function(number_station){
  station <- stations$Ciclo_Estacion_Retiro[number_station]
  
  EDAStations <- data_day_station %>%
    filter(Ciclo_Estacion_Retiro == station ) %>%
    right_join(dates_retiro, by = 'Fecha_Retiro' )%>%
    select(Fecha_Retiro, trips) %>%
    mutate(trips= ifelse(!is.na(trips), trips, 0))%>%
    arrange(Fecha_Retiro)
  
  EDAStations <- EDAStations%>%
    filter(trips == 0)
  
  return(n_zeroes = length(EDAStations$trips))
  
}


zeroesStations <- data.frame(station = numeric(),
                               variance = numeric())

for(i in 1:448){
  zeroesStations <- rbind(zeroesStations , 
                            data.frame(as.numeric(i), EDAStationZeroes(i)) 
  )
}
colnames(zeroesStations)<- c("station","n_zeroes")
zeroesStations <- as.tbl(zeroesStations)

hist(zeroesStations$n_zeroes, 100)


##Decided to exclude stations with more than 10 zeroes on their trips
wipi<-filter(zeroesStations,n_zeroes >10)

#############################################################
##MAPE analysis for every station############################
#############################################################

mapeCum <- 0
CheckStations <- NULL
for (cli in 1:420) {
  i_s <- cli # values from 1 to 420
  #i_s <- 22
  #print(i_s)
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
  
  real <- exp(real)-1
  pred <- exp(pred)-1
  
  if(forecast::accuracy(real[84:90],pred[84:90])[5] > 30){
    print(paste(i_s,forecast::accuracy(real[84:90],pred[84:90])[5]))
    CheckStations <- c(CheckStations,i_s)
    next()
  }
  mapeCum <- mapeCum + forecast::accuracy(real[84:90],pred[84:90])[5]
}

mapeCum/420
