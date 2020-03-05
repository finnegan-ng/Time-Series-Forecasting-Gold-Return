setwd("your-path")
#install these following packages

library(TTR)
library(ggplot2)
library(plotly)
library(readr)
library(xts)
library(TSA)
library(lubridate)
library(gridExtra)
library(forecast)
library(tseries)
library(Ecdat)
library(lubridate)
library(rugarch)
library(TSPred)
library(fGarch)
library(FinTS)
library(astsa)
library(aTSA)
library(lmtest)
library(dplyr)
library(ggpubr)


#read the data from the csv files
gold <- read.csv("LBMA-GOLDtest.csv",header=TRUE,stringsAsFactors = FALSE)
goldadj <- read.csv("LBMA-GOLD-adjtest.csv",header=TRUE,stringsAsFactors = FALSE)
goldtr <-read.csv("LBMA-GOLD copy.csv",header=TRUE,stringsAsFactors = FALSE)
attach(gold)
head(gold)
class(gold)
colnames(gold)
class(gold$Date)
class(gold$Value)

#make a ts object
ts_gold = ts(data=gold$Value,frequency = 12, start = c(2002,1))
test = diff(log(ts_gold))
test1 <-test

#adjust the time and plot the ts
as.Date(gold[,1])
goldts <- xts(gold[,-1], order.by=as.Date(paste0(gold[,1],origin="2002-01-31")))

temp <- data.frame(index(goldts),stack(as.data.frame(coredata(goldts))))
names(temp)[1] <- "Year"
names(temp)[2] <- "Value"

head(temp)
dim(temp)

quartz(width=10,height=6)
p<-ggplot(temp, aes(x=Year, y=Value)) + geom_line(color = "blue3", size = 0.6) + xlab(" ") + ylab(" ")
p + theme_light() 

#######
#goldadj
#clean data
df1 <- goldadj %>% select(year, month, Value) #drop "no." column
#head(df, n=10)
df1$Value <-as.numeric(df1$Value, na.rm = FALSE) # coerce to proper numeric var
df1$year <-as.factor(df1$year)
df1$month <-as.factor(df1$month)
colnames(df1) <- c("Year", "Month", "Value") #put readable column names
df1$Month <- factor(df1$Month,
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                               "Oct", "Nov", "Dec")) # arrange our month levels from proper Jan to Dec 
head(df1)

#use boxplot to spot outlier
quartz(width=10,height=6)
g <- ggplot(df1, aes(x=Year, y=Value))+ xlab(" ") + ylab(" ") + geom_boxplot(outlier.colour = "red", col = "midnightblue", fill = "lightblue1")
g + theme_light() 
cleangold <-ts_gold

#ACF plot
quartz(width=10,height=6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(ts_gold))

ggAcf(ts_gold) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")


#pacf
quartz(width=10,height=6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(ts_gold))

ggPacf(ts_gold) + ggtitle(" ") + xlab(" ") + scale_x_discrete(limits=c(" ","2"," ","4"," ","6"," ","8"," ","10"," ","12"," ","14"," ","16"," ","18"," ","20"," ","22"," ","24")) + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#adf test for stationary
adf.test(ts_gold, alternative = "stationary")


#splitting data to remove 2018 data (not really split, split after do log return)
gold_training <- ts(ts_gold, start = c(2002,1), end=c(2018,12), frequency = 12)
library(tsbox)
gold_training.df <- ts_df(gold_training)
head(gold_training.df)
gold_training.df["value"]
gold_training.df$value
closing_prices <- gold_training.df[,"value", drop = F]
head(closing_prices)
plot(gold_training.df$value, type="l", col="blue", lwd=2, 
     ylab=" ", main=" ")
#legend(x = 'topleft', legend = 'XAU', lty = 1, lwd = 2, col = 'blue')

gold_prices <-gold_training.df[,"value", drop =FALSE]
n <- nrow(gold_prices)
gold_ret <- ((gold_prices[2:n, 1] - gold_prices[1:(n-1), 1])/gold_prices[1:(n-1), 1])
class(gold_ret)

names(gold_ret) <- gold_training.df[2:n, 1]
head(gold_ret)

#gold_ccret <- diff(log(ts_gold), lag = 12)
gold_ccret <- (log(gold_prices[2:n, 1]) - log(gold_prices[1:(n-1), 1]))
summary(gold_ccret)
names(gold_ccret) <- gold_training.df[2:n, 1]
#head(gold_ccret, n=200)
plot(gold_ccret, type = "l", col = "blue3", lwd = 2, ylab = " ",
     main = " ")

#split 2018 data for testing
quartz(width=10,height=6)
gold_ccret_ts = ts(gold_ccret, frequency = 12, start = c(2002,1), end=c(2017,12))
gold_ccret_test_ts = ts(gold_ccret, frequency = 12, start = c(2002,1), end=c(2018,12))

#log return
plot(gold_ccret_ts, type = "l", col = "blue3", lwd = 2, ylab = " ", xlab = " ",
     main = " ")
abline(h = 0, col = "red")
gold_ccret.df <-ts_df(gold_ccret_ts)
head(gold_ccret.df)
gold_test.df <-ts_df(gold_ccret_test_ts)
# Add a legend
legend(x = "bottomright", legend = c("XAU"), lty = 1, 
       lwd = 2, col = c("blue"))

adf.test(gold_ccret_ts,alternative = "stationary")

Acf(gold_ccret_ts, lag.max = 35, type = c("correlation", "covariance",
                                          "partial"))
#plot lag 1
plot(gold_ccret_ts, x=zlag(gold_ccret_ts), ylab = expression(Y[t], xlab = expression(Y[t-1])))
#plot lag 2
plot(gold_ccret_ts, x=zlag(gold_ccret_ts,2), ylab = expression(Y[t], xlab = expression(Y[t-2])))

#square monthly return
gold_ccret2 <- gold_ccret_ts*gold_ccret_ts
adf.test(gold_ccret2,alternative = "stationary")
plot(gold_ccret2, col="blue3", main=" ", xlab=" ", ylab = " ")
#acf square return
quartz(width=10,height=6)

#+ scale_x_discrete(limits=c(" ","2"," ","4"," ","6"," ","8"," ","10"," ","12"," ","14"," ","16"," ","18"," ","20"," ","22"," ","24")) + ylab("ACF")
ci2 = qnorm((1 + .95)/2)/sqrt(length(gold_ccret2))

ggAcf(gold_ccret2) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#pacf square return
quartz(width=10,height=6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(gold_ccret2))

ggPacf(gold_ccret2) + ggtitle(" ") + xlab(" ") + scale_x_discrete(limits=c(" ","2"," ","4"," ","6"," ","8"," ","10"," ","12"," ","14"," ","16"," ","18"," ","20"," ","22"," ","24")) + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")


#decompose (best)
quartz(width=10,height=6)
gold.stl = stl(gold_training, s.window ="periodic")
plot(gold.stl, col='blue3')
#plot(gold.stl$time.series[,3])

#acf log differenced gold (monthly return)
quartz(width=10,height=6)

#+ scale_x_discrete(limits=c(" ","2"," ","4"," ","6"," ","8"," ","10"," ","12"," ","14"," ","16"," ","18"," ","20"," ","22"," ","24")) + ylab("ACF")
ci2 = qnorm((1 + .95)/2)/sqrt(length(gold_ccret_ts))

ggAcf(gold_ccret_ts) + ggtitle(" ") + xlab(" ") + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")
#pacf log diff gold
quartz(width=10,height=6)
ci2 = qnorm((1 + .95)/2)/sqrt(length(gold_ccret_ts))

ggPacf(gold_ccret_ts) + ggtitle(" ") + xlab(" ") + scale_x_discrete(limits=c(" ","2"," ","4"," ","6"," ","8"," ","10"," ","12"," ","14"," ","16"," ","18"," ","20"," ","22"," ","24")) + ylab(" ") + theme(
  panel.background = element_rect(fill="white"),
  axis.title = element_text(colour="white", size = 12),
  axis.title.x = element_text(colour="white", size = 12),    
  axis.text = element_text(colour="white", size = 12),
  axis.text.y = element_text(colour="white", size = 12),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="#D3D3D3"),
  plot.title = element_text(colour="white"),
  plot.background = element_rect(fill = "#393C4A")
) +
  geom_segment(lineend = "butt", color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_hline(yintercept = c(ci2, -ci2), color = "#0000CD", linetype = "dashed")

#EACF
eacf(gold_ccret_ts)
arimaModel_9 <- auto.arima(gold_ccret_ts)

#fit arima model
arimaModel_1=Arima(gold_ccret_ts,order = c(1,0,2))
arimaModel_2=Arima(gold_ccret_ts,order = c(1,0,1))
arimaModel_3=Arima(gold_ccret_ts,order = c(1,0,3))
arimaModel_4=Arima(gold_ccret_ts,order = c(1,1,3))
arimaModel_5=Arima(gold_ccret_ts,order = c(0,1,1))
arimaModel_6=Arima(gold_ccret_ts,order = c(0,0,1))
arimaModel_7<-Arima(gold_ccret_ts,order = c(0,0,2)) #best
arimaModel_8=Arima(gold_ccret_ts,order = c(0,0,3))
print(arimaModel_1);print(arimaModel_2);print(arimaModel_3);print(arimaModel_4);print(arimaModel_5);print(arimaModel_6);print(arimaModel_7);print(arimaModel_8)

AIC(arimaModel_1,Arima(gold_ccret_ts,order = c(2,0,1)),
    arimaModel_2,Arima(gold_ccret_ts,order = c(2,0,2)),
    arimaModel_3,Arima(gold_ccret_ts,order = c(1,0,2)),
    arimaModel_4,Arima(gold_ccret_ts,order = c(1,1,2)),
    arimaModel_5,Arima(gold_ccret_ts,order = c(1,0,0)),
    arimaModel_6,Arima(gold_ccret_ts,order = c(0,0,2)),
    arimaModel_7,Arima(gold_ccret_ts,order = c(0,0,1)),
    arimaModel_8,Arima(gold_ccret_ts,order = c(0,0,3)))

arimaModel_7<-Arima(gold_ccret_ts,order = c(0,0,2))
arimaModel_7
(1-pnorm(abs(arimaModel_7$coef)/sqrt(diag(arimaModel_7$var.coef))))*2

coeftest(arimaModel_7)

#residual
require(graphics)
nobs(arimaModel_7)
quartz(width=10,height=6)
tsdiag(arimaModel_7, gof=24, omit.initial = F)

quartz(width=10,height=6)
plot(rstandard(arimaModel_7), col = "blue3");abline(h=0, col="red")

quartz(width=10,height=6)
acf(residuals(arimaModel_7), lag = 24)

qqnorm(residuals(arimaModel_7)); qqline(residuals(arimaModel_7))
quartz(width=10,height=6)
ggqqplot(arimaModel_7$residuals)
shapiro.test(arimaModel_7$residuals)

#ARIMA forecasting
pred <- predict(arimaModel_7, n.ahead = 12)
ts.plot(gold_ccret_ts,exp(pred$pred), lty = c(1,3), col = "blue3")
grid(col='blue',lty="dotted")
p <-pred$pred
p


gold_pred<-gold_ccret_ts[length(gold_ccret_ts)]
for(i in 1:length(pred$pred)){
  gold_pred[i+1]<-gold_pred[i]+pred$pred[i]
}
plot(c(gold_ccret_ts,gold_pred),type="l", col = "red")
grid(col='blue',lty="dotted")

#ARIMA predictions against its actual values with prediction intervals
quartz(width=10,height=6)
plotarimapred(gold_ccret_test_ts, arimaModel_7, xlim = c(2002,2018), range.percent = 0.2, xreg = NULL,
              ylab = NULL, xlab = NULL, main = NULL)

summary(arimaModel_7)
#forecast points
fit = arima(gold_ccret_ts,order = c(0,0,2))
fit2<-fit
forecast(fit2,12)
#arimafore$mean
#arimafore$lower
#arimafore$upper

#forecast points
arimaModel_7$fitted

plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(arimaModel_7$residuals)


