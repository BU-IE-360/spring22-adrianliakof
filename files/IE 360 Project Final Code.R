library(RcppRoll)
library(tidyr)
library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(forecast)
library(dplyr)






#read the weather data 
Regressor_Data <- fread("2022-06-05_weather.csv")
#tail(Regressor_Data,n=50)
Regressor_Data <- data.table(Regressor_Data)
Regressor_Data$hour <-sprintf("%02d:00",Regressor_Data$hour)
#tail(Regressor_Data)
#read the production data
Production_Data <- fread("2022-06-05_production.csv")
summary(is.na(Production_Data$production))
#tail(Production_Data,n=50)
Production_Data <- data.table(Production_Data)
Production_Data$hour <-sprintf("%02d:00",Production_Data$hour)
#tail(Production_Data)
#too see data characteristics
str(Regressor_Data)
str(Production_Data)


head(Regressor_Data)
head(Production_Data)

tail(Regressor_Data)
tail(Production_Data)
#########

### formating data to wide from long  
Regressor_Data_Wide <- dcast(Regressor_Data, date + hour ~ variable + lat + lon, value.var = "value")
#tail(Regressor_Data_Wide,50)
#tail(All_Data_Wide$production)
All_Data_Wide <- merge(Regressor_Data_Wide,Production_Data, by = c("date","hour"),all=T )
summary(All_Data_Wide)
All_Data_Wide$merged_date = paste(All_Data_Wide$date,"-",All_Data_Wide$hour)
All_Data_Wide$merged_date_2 = as.POSIXct(All_Data_Wide$merged_date ,format="%Y-%m-%d - %H:%M",tz=Sys.timezone())
All_Data_Wide$index = 1:nrow(All_Data_Wide)
All_Data_Wide=All_Data_Wide[,!"merged_date"]
tail(All_Data_Wide,50)
#determine the rows in which production has na values
production_na=All_Data_Wide[is.na(All_Data_Wide$production),"index"]
production_na
indexes=production_na[["index"]]
indexes<- indexes[indexes<11713]#should be updated for each day according to the data to exclude some future days
indexes
length(indexes)
for (i in 1: length(indexes)){
  All_Data_Wide[All_Data_Wide$index==indexes[i],"production"] = All_Data_Wide[All_Data_Wide$index==(indexes[i]-24),"production"]
}
## in the loop given above we got rid of from NA values by equallin their values to value in 1 day before

#!!! All_Data_Wide[(nrow(All_Data_Wide)-19):nrow(All_Data_Wide),"production"] = NA
## we have values for data which needs to have NA value because of the loop given above, we have fixed that problem.  

All_Data_Wide_NoNA <- All_Data_Wide #[1:(nrow(All_Data_Wide)-20),]
## we do not have value for last 20 hours in the last day. So we deleted them. 

All_Data_Wide_NoNA$Max_Capacity <-c("")

production_na2=All_Data_Wide_NoNA[is.na(All_Data_Wide_NoNA$production),"index"]
naindexes<-production_na2[["index"]]
naindexes
class(naindexes)
#Finding maximum capacities by looking last 24 days starting from 2 days before. 
for(i in 0:9){
  vector_cap<-rollmax(All_Data_Wide_NoNA[hour==paste("0",as.character(i),":00",sep="")]$production,24,align='right')
  All_Data_Wide_NoNA[All_Data_Wide_NoNA[hour==paste("0",as.character(i),":00",sep="")],"Max_Capacity"]<-c(rep("NA",23+2),vector_cap[1:(length(vector_cap)-2)])
  
  
}

for(i in 10:23){
  mean_series1=rollmax(All_Data_Wide_NoNA[hour==paste(as.character(i),":00",sep="")]$production,24,align='right')
  All_Data_Wide_NoNA[All_Data_Wide_NoNA[hour==paste(as.character(i),":00",sep="")],"Max_Capacity"]<-c(rep(NA,23+2),mean_series1[1:(length(mean_series1)-2)])
  
}
for (i in naindexes[1]:(naindexes[1]+length(naindexes)-1)){
  All_Data_Wide_NoNA[i,"Max_Capacity"]<-All_Data_Wide_NoNA[i-24,"Max_Capacity"]
}
#naindexes[1]:(naindexes[1]+length(naindexes)-1)
All_Data_Wide_NoNA$Max_Capacity<-as.numeric(All_Data_Wide_NoNA$Max_Capacity)


#All_Data_Wide_NoNA<-All_Data_Wide_NoNA[601:nrow(All_Data_Wide_NoNA)]
##deleting first 600 rows because they have na max prod
#All_Data_Wide_NoNA[is.na(All_Data_Wide_NoNA$Max_Capacity),"merged_date_2"]

#deleting rows having NA in their maximum capacity column
Prepared_Forecast_Data <- All_Data_Wide_NoNA[(25*24+1):nrow(All_Data_Wide_NoNA)]

#Looking production rates to determine which hours have 0 production in general. They will be excluded in the following codes. 
for (i in 0:9){
  hist(Prepared_Forecast_Data[hour==paste("0",as.character(i),":00",sep="")]$production,freq = FALSE,breaks = 100)
}
for(i in 10:23){
  hist(Prepared_Forecast_Data[hour==paste(as.character(i),":00",sep="")]$production,freq = FALSE,breaks = 100)
  
}
#hours are determined 
deletion_list=c("20:00","21:00","22:00","23:00","00:00","01:00","02:00","03:00","04:00","05:00")

#Only determined hours are taken to dataset. 
Data_To_Forecasted <- Prepared_Forecast_Data[ ! hour %in% deletion_list]
Data_To_Forecasted$hour<-as.factor(Data_To_Forecasted$hour)

#str(Data_To_Forecasted)
#Data_To_Forecasted$scaled_prod_ratio <- Data_To_Forecasted$production/(Data_To_Forecasted$Max_Capacity+0.000001)
#### adding capacity*other variables to see their interrelation


#ggplot(data=Data_To_Forecasted)+
#geom_point(mapping=aes(x=merged_date_2,y=Data_To_Forecasted$production) ) #production plot... 

Data_To_Forecasted$index<- 1:nrow(Data_To_Forecasted) #adding indexes

#creating colums for average values of variables having different values for different coordinates. 
Data_To_Forecasted$Avg_Cloud<-0 
Data_To_Forecasted$Avg_DSWRF<-0
Data_To_Forecasted$Avg_Humidity<-0
Data_To_Forecasted$Avg_Temp<-0
#max(Data_To_Forecasted$production/(Data_To_Forecasted$Max_Capacity+0.000001))#epsilon is added to prevent 0 division

#average of variabes added for smoothing 
for (i in 1:nrow(Data_To_Forecasted)){
  Data_To_Forecasted[i]$Avg_Cloud <- sum(Data_To_Forecasted[i,3:11])/9
}
for (i in 1:nrow(Data_To_Forecasted)){
  Data_To_Forecasted[i]$Avg_DSWRF <- sum(Data_To_Forecasted[i,12:20])/9
}
for (i in 1:nrow(Data_To_Forecasted)){
  Data_To_Forecasted[i]$Avg_Humidity <- sum(Data_To_Forecasted[i,21:29])/9
}
for (i in 1:nrow(Data_To_Forecasted)){
  Data_To_Forecasted[i]$Avg_Temp <- sum(Data_To_Forecasted[i,30:38])/9 #
}
str(Data_To_Forecasted)
#To examine the relation btw capacity and other independent variables, their multiplications are added as dicrete regressors. 
Data_To_Forecasted$Temp_Cap <- Data_To_Forecasted$Avg_Temp*Data_To_Forecasted$Max_Capacity
Data_To_Forecasted$Cloud_Cap <- Data_To_Forecasted$Avg_Cloud*Data_To_Forecasted$Max_Capacity
Data_To_Forecasted$DSWRF_Cap <- Data_To_Forecasted$Max_Capacity*Data_To_Forecasted$Avg_DSWRF
Data_To_Forecasted$Humidity_Cap<- Data_To_Forecasted$Max_Capacity*Data_To_Forecasted$Avg_Humidity
Data_To_Forecasted$lag42<-lag(Data_To_Forecasted$production,42)
Data_To_Forecasted$temp_lag2<-lag(Data_To_Forecasted$Avg_Temp,2)
Data_To_Forecasted$Humidity_lag2<-lag(Data_To_Forecasted$Humidity_Cap,2)

#str(Data_To_Forecasted)



#MODEL CREATION PHASE
production_na3=Data_To_Forecasted[is.na(Data_To_Forecasted$production),"index"]
naindexes_flag<-production_na3[["index"]]
my_flag<-naindexes_flag[1]-1
my_flag
Training<-Data_To_Forecasted[1:my_flag]
Test<-Data_To_Forecasted[(my_flag+1):(my_flag+42)]

## ARIMA first model including primary dates (growth section) START 
acf(Training$production)
pacf(Training$production)
# acf is sinusoidal add lag p 
# arima paramters are created 
arima_parameters <- auto.arima(Training$production,max.p = 20,max.q = 20)
arima_parameters
#arime model is created 
model_arima <- arima(Training$production, order= c(4,1,2))
accuracy(model_arima)
print(model_arima)

## prediction values made by arima model
model_fitted <- Training$production-model_arima$residuals
ts.plot(Training$production,xlab = "Time", ylab = "Production",legend())
points(model_fitted, type = "l", col = 2, lty = 2)

## wmape calculation because MAPE has infinity value. 
wmape<-0
numerator<-0
denumerator<-0
summary(is.na(Training$production))
for (i in 1:nrow(Training)){
  numerator<-abs(Training[i]$production-model_fitted[i])+numerator
  denumerator <- Training[i]$production + denumerator
}

wmape <- numerator/denumerator
wmape
# 0.2232968

## ARIMA model including primary dates END


#### ARIMA second model excluding growth section Start
Arima_Data <- Training[1150:nrow(Training),]

arima_parameters2 <- auto.arima(Arima_Data$production,max.p = 20,max.q = 20)
arima_parameters2
#arime model is created 
model_arima2 <- arima(Arima_Data$production, order= c(4,1,2))
accuracy(model_arima2)
print(model_arima2)

## prediction values made by arima model
model_fitted2 <- Arima_Data$production-model_arima2$residuals
ts.plot(Arima_Data$production,xlab = "Time", ylab = "Production")
points(model_fitted2, type = "l", col = 2, lty = 2)

## wmape calculation 
wmape2<-0
numerator2<-0
denumerator2<-0
for (i in 1:nrow(Arima_Data)){
  numerator2<-abs(Arima_Data[i]$production-model_fitted2[i])+numerator2
  denumerator2 <- Arima_Data[i]$production + denumerator2
}

wmape2 <- numerator2/denumerator2
wmape2
#0.2228017
### ARIMA model 2 END


ar_production = Arima_Data$production
ar_production_sliced = ar_production[4200:length(ar_production)]
ts.plot(ar_production_sliced,xlab = "Time", ylab = "Production")
length(ar_production_sliced)
acf(ar_production_sliced)
pacf(ar_production_sliced)
## difference 1 lag 3 + seasonal lag = 14
model_arima3 <- arima(ar_production_sliced, order= c(3,1,0),seasonal = c(14,0,0))
model_arima3$residuals
model_fitted3 <- ar_production_sliced-model_arima3$residuals
ts.plot(ar_production_sliced,xlab = "Time", ylab = "Production")
points(model_fitted3, type = "l", col = 2, lty = 2)

accuracy(model_arima3)
print(model_arima3)

wmape3<-0
numerator3<-0
denumerator3<-0
for (i in 1:length(ar_production_sliced)){
  numerator3<-abs(ar_production_sliced[i]-model_fitted3[i])+numerator3
  denumerator3 <- ar_production_sliced[i] + denumerator3
}

wmape3 <- numerator3/denumerator3
wmape3

# WMAPE = 0.1223735

## ARIMA MODEL 3 ENDS

## if we want to exclude growth section this code can be implemented. 
#Data_To_Forecasted <- Data_To_Forecasted[1150:nrow(Data_To_Forecasted)]

#Model 1 incl. only relation variables such as Temp_Cap, Cloud_Cap
initial_model <- lm(production~Temp_Cap+Humidity_Cap+Cloud_Cap+DSWRF_Cap, Training)
summary(initial_model)
checkresiduals(initial_model) # ;)
# when we look at autocorrelation graph, we determined to add some lag values.
pacf(initial_model$residuals)
#partial autocof is also controlled. 
# Lag values are created.



#Model 2 incl. all regressors. 
str(Training)


second_model <- lm(production~., Training)
summary(second_model)
checkresiduals(second_model) # ;)
second_model$residuals
Test
predictions<-predict(second_model, newdata = Test)
predictions

str(Training)
#Training[5000,]
#Model 3 incl. only needed ####TAMAMI ICIN BUNU KULLAN 
Model_3Data <- Training[(5000:nrow(Training)),-c(3:38,1,40)]
Model_3Predict<-Test[,-c(3:38,1,40)]
str(Model_3Data)
third_model <- lm(production~., Model_3Data)
summary(third_model)
checkresiduals(third_model) # ;)
predict(third_model,newdata =Model_3Predict )
Test[29:42]
#Model4 incld. Only needed
ggplot(data=Training)+
  geom_point(mapping=aes(x=merged_date_2,y=Training$production) ) +
  labs(x="Date",y="Production")#production plot.
Model_4Data <-Training[5000:nrow(Training)]
Model_4Test<-Test
Model_4Data <- Model_4Data

str(Model_4Data)
fourth_model <- lm(production~., Model_4Data)
summary(fourth_model)
checkresiduals(third_model) # ;)
predict(third_model,newdata =Model_3Predict )

str(Training)
str(Test)
Test

##### For each hour discrete prediction
for (i in c(6,7,8,9)){
  hourlydata<-Training[hour==paste("0",as.character(i),":00",sep="")]
  hourlydata <- hourlydata[,-c(3:38,1,40,2)]
  
  hourlypredict<-Test[hour==paste("0",as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[,-c(3:38,1,40,2)]
  modelhourly<-lm(production~.,hourlydata)
  # print(summary(modelhourly))
  c1<-predict(modelhourly,newdata = hourlypredict)
  print(c1)
  modelhourly_pred<-modelhourly$fitted.values
  print(i)
  print(wmape_func(hourlydata$production,modelhourly_pred))
  
}
for(i in 10:23){
  hourlydata<-Training[hour==paste(as.character(i),":00",sep="")]
  hourlydata <- hourlydata[,-c(3:38,1,40,2)]
  hourlypredict<-Test[hour==paste(as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[,-c(3:38,1,40,2)]
  modelhourly<-lm(production~.,hourlydata)
  #print(summary(modelhourly))
  c1<-predict(modelhourly,newdata = hourlypredict)
  print(c1)
  modelhourly_pred<-modelhourly$fitted.values
  print(wmape_func(hourlydata$production,modelhourly_pred))
}


############################ 
##### Hourly model incl. index after 5000 (which is used in forecasting)
for (i in c(6,7,8,9)){
  hourlydata<-Model_4Data[hour==paste("0",as.character(i),":00",sep="")]
  hourlydata <- hourlydata[,-c(3:38,1,40,2)]
  
  hourlypredict<-Model_4Test[hour==paste("0",as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[,-c(3:38,1,40,2)]
  modelhourly<-lm(production~.,hourlydata)
  #print(summary(modelhourly))
  c1<-predict(modelhourly,newdata = hourlypredict)
  print(c1)
  modelhourly_pred<-modelhourly$fitted.values
  print(i)
  print(wmape_func(hourlydata$production,modelhourly_pred))
  
  
}
for(i in 10:23){
  hourlydata<-Model_4Data[hour==paste(as.character(i),":00",sep="")]
  hourlydata <- hourlydata[,-c(3:38,1,40,2)]
  hourlypredict<-Model_4Test[hour==paste(as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[,-c(3:38,1,40,2)]
  modelhourly<-lm(production~.,hourlydata)
  #print(summary(modelhourly))
  c1<-predict(modelhourly,newdata = hourlypredict)
  print(c1)
  modelhourly_pred<-modelhourly$fitted.values
  print(i)
  print(wmape_func(hourlydata$production,modelhourly_pred))
  
}
#############################################################3


#########
wmape_func <- function(actual,forecasted){
  numerator2<-0
  denumerator2<-0
  for (i in 1:length(forecasted)){
    numerator2 <-abs(actual[i]-forecasted[i])+numerator2
    denumerator2 <- actual[i] + denumerator2
  }
  return(numerator2/denumerator2)
}
model3_fitted_values <- third_model$fitted.values

wmape2<-0
numerator2<-0
denumerator2<-0
for (i in 1:nrow(Model_3Data)){
  numerator2<-abs(Model_3Data[i]$production-model3_fitted_values[i])+numerator2
  denumerator2 <- Model_3Data[i]$production + denumerator2
}

wmape2 <- numerator2/denumerator2
wmape2

######### Evaluation Phase starting from 1 Mar to 24 May for each model. 

#For the first model we used;
##5153,6342
rep(0,85)
table_evaluation<-data.frame(model1=rep(0,85)
                             ,model2=rep(0,85),model3=rep(0,85),model4=rep(0,85))

table_evaluation
(6342-5153+1)/14
x<-c()
for (i in 0:84){
  Tr<-Data_To_Forecasted[1:(5152+i*14)]
  Test<-Data_To_Forecasted[(5153+(i)*14):((5153+(i)*14+13))]
  model1<-lm(production~Temp_Cap+Humidity_Cap+Cloud_Cap+DSWRF_Cap,Tr)
  results<-predict(model1, newdata=Test)
  x[i+1] <- as.numeric(wmape_func(Test$production,results))
  
}
table_evaluation["model1"]<-x
mean(x[1:82])
x<-c()
for (i in 0:84){
  Tr<-Data_To_Forecasted[1:(5152+i*14)]
  Test<-Data_To_Forecasted[(5153+(i)*14):((5153+(i)*14+13))]
  model2<- lm(production~., Tr)
  results<-predict(model2, newdata=Test)
  x[i+1] <- as.numeric(wmape_func(Test$production,results))
  
}
mean(x[1:82])

table_evaluation["model2"]<-x
table_evaluation

x<-c()
for (i in 0:84){
  Tr<-Data_To_Forecasted[5000:(5152+i*14),-c(3:38,1,40)]
  Test<-Data_To_Forecasted[(5153+(i)*14):((5153+(i)*14+13)),-c(3:38,1,40)]
  model3<-lm(production~.,Tr)
  results<-predict(model3, newdata=Test)
  x[i+1] <- as.numeric(wmape_func(Test$production,results))
  
}
mean(x[1:82])
table_evaluation["model3"]<-x
table_evaluation



general<-rep(0,85)
for(k in 0:84){
actual<-c()
forecasted<-c()
for (i in 6:9){
  hourlydata<-Data_To_Forecasted[hour==paste("0",as.character(i),":00",sep="")]
  hourlydata <- hourlydata[310:(368+k),-c(3:38,1,40,2)]
  #print(hourlydata)
  
  hourlypredict<-Data_To_Forecasted[hour==paste("0",as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[(369+k),-c(3:38,1,40,2)] 
  modelhourly<-lm(production~.,hourlydata)
  c1<-predict(modelhourly,newdata = hourlypredict)
  actual<-c(actual,hourlypredict$production)
  forecasted<-c(forecasted,c1)
  
 
  
}
for(i in 10:19){
  hourlydata<-Data_To_Forecasted[hour==paste(as.character(i),":00",sep="")]
  hourlydata <- hourlydata[310:(368+k),-c(3:38,1,40,2)] 
  hourlypredict<-Data_To_Forecasted[hour==paste(as.character(i),":00",sep="")]
  hourlypredict<-hourlypredict[(369+k),-c(3:38,1,40,2)] 
  modelhourly<-lm(production~.,hourlydata)
  
  c1<-predict(modelhourly,newdata = hourlypredict)
  actual<-c(actual,hourlypredict$production)
  forecasted<-c(forecasted,c1)
  
} 

general[k+1]<-as.numeric(wmape_func(actual,forecasted))
}

general
table_evaluation["model4"]<-general
table_evaluation





