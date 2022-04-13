#install.packages("readxl")
#install.packages("lubridate")
#install.packages("zoo")
#install.packages("ggplot2")
#install.packages("scales")
#install.packages("data.table")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages('GGally')
#install.packages("gtrendsR")

options(warn=-1)

library(readxl)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(data.table)
library(corrplot)
library(gtrendsR)
library(ggcorrplot)

options(warn=0)

#Getting the data

setwd("C:\\Users\\sonef\\Desktop")
Total_Data <- read_excel("EVDS.xlsx")
Total_Data <- data.table(Total_Data)

StartIndex = which(Total_Data$Date=="2012-01")
FinishIndex = which(Total_Data$Date=="2021-12")

Total_Data <- Total_Data[StartIndex:FinishIndex,]
colnames(Total_Data) = c("Dates","Industrial_Production","USD_TL_Exchange_Rate"
                         ,"CPI","General_Expectation")
rownames(Total_Data) = 1:nrow(Total_Data)

Total_Data$Industrial_Production <- as.numeric(Total_Data$Industrial_Production)
Total_Data$USD_TL_Exchange_Rate <- as.numeric(Total_Data$USD_TL_Exchange_Rate)

Total_Data$Dates <- paste(Total_Data$Dates,"-01",sep="")
Total_Data$Dates <- as.Date(Total_Data$Dates)

head(Total_Data)

##General_Expectation
ggplot(data=Total_Data)+
  geom_line(mapping=aes(x=Dates,y=General_Expectation))+
  geom_point(mapping=aes(x=Dates,y=General_Expectation) )+
  labs(title = "General Economic Situation Expectation Level in Turkey", x = "Date",y = "General Economic Situation Expectation Level ",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Source: EVDS")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=General_Expectation) ,formula = y ~ x, method = "lm",level=0)

##USD_TL_Exchange_Rate
ggplot(data=Total_Data,aes(group=1))+
  geom_line(mapping=aes(x=Dates,y=USD_TL_Exchange_Rate))+
  geom_point(mapping=aes(x=Dates,y=USD_TL_Exchange_Rate) )+
  labs(title = "USD_TL_Exchange_Rate level in Turkey", x = "Date",y = "USD TL Exchange",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Source: EVDS")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=USD_TL_Exchange_Rate), formula = y ~ x, method = "lm",level=0 )



Exchange_Rate_Logarithm_Difference <- c(diff(log(Total_Data$USD_TL_Exchange_Rate)),0.00001)
Total_Data <- cbind(Total_Data, Exchange_Rate_Logarithm_Difference)

ggplot(data=Total_Data)+
  geom_line(mapping=aes(x=Dates,y=Exchange_Rate_Logarithm_Difference))+
  geom_point(mapping=aes(x=Dates,y=Exchange_Rate_Logarithm_Difference) )+
  labs(title = "USD TL Exchange Rates Logarithmic Differences", x = "Date",y = "Log Difference",subtitle = "Data from Jan-2012 to Dec-2021")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))





##CPI
ggplot(data=Total_Data)+
  geom_line(mapping=aes(x=Dates,y=CPI))+
  geom_point(mapping=aes(x=Dates,y=CPI) )+
  labs(title = "CPI level in Turkey", x = "Date",y = "CPI",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Source: EVDS")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=CPI), formula = y ~ x, method = "lm",level=0 )


##Industrial_Production
ggplot(data=Total_Data,aes(group=1))+
  geom_line(aes(x=Dates,y=Industrial_Production))+
  geom_point(mapping=aes(x=Dates,y=Industrial_Production) )+
  labs(title = "Industrial Production level in Turkey", x = "Date",y = "Industrial Production Level",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Source: EVDS")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=Industrial_Production), formula = y ~ x, method = "lm",level=0 )



ggplot(data=Total_Data[Dates>='2014-01-01' & Dates<='2016-12-01'],aes(group=1))+
  geom_line(aes(x=Dates,y=Industrial_Production))+
  geom_point(mapping=aes(x=Dates,y=Industrial_Production) )+
  labs(title = "Industrial Production level in Turkey", x = "Date",y = "Industrial Production Level",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Source: EVDS")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 month",labels=date_format("%b"))+
  geom_smooth(mapping=aes(x=Dates,y=Industrial_Production), formula = y ~ x, method = "lm",level=0 )


require(GGally)
ggpairs(Total_Data[,-1,with=FALSE])

correl_info=cor(Total_Data[,2:5])

ggcorrplot(correl_info, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE)


dolar=read.csv("dolar.csv",header=FALSE,sep=",")
colnames(dolar) <- c("Ay","dolar")
dolar[,"Ay"]=as.Date(as.yearmon(dolar[,"Ay"],format="%Y-%m"))
dolar[,"dolar"]=as.numeric(dolar[,"dolar"])

dolar <- data.table(dolar)
dolar <- dolar[Ay>='2012-01-01' & Ay<='2021-12-01']

YurtDisiIs=read.csv("YurtDisiIs.csv",header=FALSE,sep=",")
colnames(YurtDisiIs) <- c("Ay","YurtDisiIs")
YurtDisiIs[,"Ay"]=as.Date(as.yearmon(YurtDisiIs[,"Ay"],format="%Y-%m"))
YurtDisiIs[,"YurtDisiIs"]=as.numeric(YurtDisiIs[,"YurtDisiIs"])

YurtDisiIs <- data.table(YurtDisiIs)
YurtDisiIs <- YurtDisiIs[Ay>='2012-01-01' & Ay<='2021-12-01']

enflasyon=read.csv("enflasyon.csv",header=FALSE,sep=",")
colnames(enflasyon) <- c("Ay","enflasyon")
enflasyon[,"Ay"]=as.Date(as.yearmon(enflasyon[,"Ay"],format="%Y-%m"))
enflasyon[,"enflasyon"]=as.numeric(enflasyon[,"enflasyon"])

enflasyon <- data.table(enflasyon)
enflasyon <- enflasyon[Ay>='2012-01-01' & Ay<='2021-12-01']


ggplot(data=dolar)+
  geom_line(aes(x=Ay,y=dolar))+
  geom_point(mapping=aes(x=Ay,y=dolar) )+
  labs(title = "Search Volume of the word 'Dolar' in Turkey", x = "Date",y = "Search Volume",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Google Trends")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))



ggplot(data=enflasyon)+
  geom_line(aes(x=Ay,y=enflasyon))+
  geom_point(mapping=aes(x=Ay,y=enflasyon) )+
  labs(title = "Search Volume of the word 'Enflasyon' in Turkey", x = "Date",y = "Search Volume",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Google Trends")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))


ggplot(data=Total_Data)+
  geom_line(mapping=aes(x=Dates,y=Exchange_Rate_Logarithm_Difference),color = "red")+
  geom_point(mapping=aes(x=Dates,y=Exchange_Rate_Logarithm_Difference),color = "red" )+
  labs(title = "USD TL Exchange Rates Logarithmic Differences", x = "Date",y = "Search Volume",subtitle = "Data from Jan-2012 to Dec-2021",caption = "Google Trends")+
  theme_bw()+ 
  theme(plot.title = element_text(color = "#512FDC", size = 15, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))

By comparing search volume of "dolar" and "enflasyon" with logarithmic difference curve of the TL-USD exchange rate, it can be clearly seen that change in exchange rate is highly correlated with search volume of these two words. This is expected since USD is a major investment tool and also, prices change in the periods where exchange rate changes so people try to learn more about these changes by looking at the current exchange rate (by searching "dolar") and also by looking at the inflation information (by searching "enflasyon")
ggplot(data=YurtDisiIs)+
  geom_line(aes(x=Ay,y=YurtDisiIs))+
  geom_point(mapping=aes(x=Ay,y=YurtDisiIs) )

ggplot(data=Total_Data)+
  geom_line(mapping=aes(x=Dates,y=General_Expectation),color = "red")+
  geom_point(mapping=aes(x=Dates,y=General_Expectation),color = "red" )

