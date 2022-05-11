options(warn=-1)
library(RcppRoll)
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
options(warn=0)

path <- "C:/Users/sonef/Desktop/IE360_Spring22_HW2_data.csv"

Homework2_Data <- read.csv(path,colClasses=c('character',rep('numeric',10)))
colnames(Homework2_Data) <- c("Quarters", "UGS", "RNUV","NLPG","PU","PG","NUGV","NDGV","GNPA","GNPC","GNPT")

Homework2_Data$Quarters <- as.Date(as.yearqtr(Homework2_Data$Quarters, format = "%Y_Q%q"))


Homework2_Training <- Homework2_Data[c(1:28),]
Homework2_Forecast <- Homework2_Data[c(29,30,31,32),]

Homework2_Training <- data.table(Homework2_Training)
Homework2_Forecast <- data.table(Homework2_Forecast)


head(Homework2_Training)

str(Homework2_Training)

ggplot(Homework2_Training, aes(x=Quarters,y=UGS, group = 1)) +
geom_point() +
geom_line() +
labs(y ='Gasoline Sale in a Quarter (1000m^3)')+ 
ggtitle('Unleaded Gasoline Sales per Quarter')+
geom_smooth(formula = y ~ x, method = "lm",level=0.9)

mean_series=roll_mean(Homework2_Training$UGS,4,align='left')
var_series=roll_var(Homework2_Training$UGS,4,align='left')
plot(mean_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Mean",
     main = "Mean series")

plot(var_series,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Rolling Variance",
     main = "Variance series")

acf(Homework2_Training$UGS)

Homework2_Training[,trend := 1:.N ]
Homework2_Training[,Quarters_:=as.character(month(Quarters))]

Homework2_Forecast[,trend := 29:32 ]
Homework2_Forecast[,Quarters_:=as.character(month(Quarters))]

Model_1 <- lm(UGS ~ trend+Quarters_, Homework2_Training)
summary(Model_1)

Plot_Model1=copy(Homework2_Training)
Plot_Model1[,actual:=UGS]
Plot_Model1[,predicted_trend:=predict(Model_1,Plot_Model1)]
Plot_Model1[,residual_trend:=actual-predicted_trend]

ggplot(Plot_Model1 ,aes(x=Quarters)) +
        geom_line(aes(y=UGS,color='real')) + 
        geom_line(aes(y=predicted_trend,color='predicted'))

checkresiduals(Model_1$residuals)

ggpairs(Homework2_Training)

Model_2 <- lm(UGS ~ trend+NLPG+PU+PG+NUGV+NDGV+GNPA + Quarters_, Homework2_Training)
summary(Model_2)

Plot_Model2=copy(Homework2_Training)
Plot_Model2[,actual:=UGS]
Plot_Model2[,predicted_trend:=predict(Model_2,Plot_Model2)]
Plot_Model2[,residual_trend:=actual-predicted_trend]

ggplot(Plot_Model2 ,aes(x=Quarters)) +
        geom_line(aes(y=UGS,color='real')) + 
        geom_line(aes(y=predicted_trend,color='predicted'))

checkresiduals(Model_2$residuals)

Homework2_Training$UGSlag1=lag(Homework2_Training$UGS,1)

Homework2_Training$NUGVlag5=lag(Homework2_Training$NUGV,5)
Homework2_Training$NDGVlag5=lag(Homework2_Training$NDGV,5)


Model_3 <-  lm(UGS ~ trend+PU+PG+NUGV+NDGV+GNPA +UGSlag1+NUGVlag5+NDGVlag5 + Quarters_, Homework2_Training)
summary(Model_3)
checkresiduals(Model_3$residuals)

Plot_Model3=copy(Homework2_Training)
Plot_Model3[,actual:=UGS]
Plot_Model3[,predicted_trend:=predict(Model_3,Plot_Model3)]
Plot_Model3[,residual_trend:=actual-predicted_trend]

ggplot(Plot_Model3 ,aes(x=Quarters)) +
        geom_line(aes(y=UGS,color='real')) + 
        geom_line(aes(y=predicted_trend,color='predicted'))

Homework2_Forecast$UGSlag1[1]=Homework2_Training$UGS[28]

Homework2_Forecast$NUGVlag5[1]=Homework2_Training$NUGV[24]
Homework2_Forecast$NDGVlag5[1]=Homework2_Training$NDGV[24]



Homework2_Forecast[1,"UGS"]=as.numeric(predict(Model_3,newdata=Homework2_Forecast[1,]))
Homework2_Forecast$UGSlag1[2]=as.numeric(Homework2_Forecast[1,"UGS"])


Homework2_Forecast[2,"UGS"]=predict(Model_3,newdata=Homework2_Forecast[2,])
Homework2_Forecast$UGSlag1[3]=as.numeric(Homework2_Forecast[2,"UGS"])

Homework2_Forecast[3,"UGS"]=predict(Model_3,newdata=Homework2_Forecast[3,])
Homework2_Forecast$UGSlag1[4]=as.numeric(Homework2_Forecast[3,"UGS"])

Homework2_Forecast[4,"UGS"]=predict(Model_3,newdata=Homework2_Forecast[4,])
Homework2_Forecast[,"UGS"]


