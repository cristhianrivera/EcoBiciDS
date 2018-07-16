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
  plot.ts(ts,  main = "", xlab = "" )
  ts <- na.omit(ts)
  forecast::Acf(ts,lags, main = "", xlab = "")
  forecast::Pacf(ts,lags, main = "")
}

TSAnalysis(log(ts_M+1), 20)

hist(ts_M)
hist(log(ts_M+1))


ts_M.1 <- base::diff(ts_M)
TSAnalysis(ts_M.1, 20)

fit_M <- arima(log(ts_M+1), order = c(1,0,0), seasonal=list(order=c(0,1,0),period = 7))
print(fit_M)
TSAnalysis(log(ts_M+1),20)

print(fit_M)
plot(forecast(fit_M, h=7))
plot(fit_M$residuals)
TSAnalysis(fit_M$residuals)


qqnorm(residuals(fit_auto_M)); qqline(residuals(fit_auto_M))
par(mfrow=c(1, 1))
qqnorm(residuals(fit_M)); qqline(residuals(fit_M))





