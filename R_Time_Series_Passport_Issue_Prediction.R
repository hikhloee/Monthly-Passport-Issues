library(tseries) #jarque.bera.test, adf.test
library(portes) #LjungBox
library(lmtest) #coeftest, dwtest
library(TSA) #dataset
library(astsa) #arima.sim 
library(forecast) #auto.arima

#원본 데이터 korea 2010-2019 
passport<-read.csv("C:/Users/ksc4you/Desktop/korea.csv",header=T)
colnames(passport)<-c("Date","Total")
passport<-passport[,2]
tspassport<-ts(passport,start=c(2010),frequency=12)
tsdisplay(tspassport)

lambda<-BoxCox.lambda(tspassport) #[1] -0.4119157
boxts<-BoxCox(tspassport,lambda) #하지 않는것이 좋음

lts<-log(tspassport) #로그 변환
tsdisplay(lts)
adf.test(lts) 
difflts<-diff(lts) #1차 차분
tsdisplay(difflts)
adf.test(difflts,k=12) 
diff2ts<-diff(difflts,12) #계절 차분(k=12)
adf.test(diff2ts) #p-value 0.01로 더이상 차분 필요하지 않음
tsdisplay(diff2ts,lwd="2")
dev.new()
acf(diff2ts,lwd="2",xlim=c(0,35))

fit<-arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12)) #설정한 모형
fit
coeftest(fit)

#fit_2<-arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12)) #모형 비교 1
#fit_2
#fit_3<-arima(lts,order=c(1,1,0),seasonal=list(order=c(0,1,1),period=12)) #모형 비교 2
#fit_3

#AIC, BIC, AICc 비교
nobs<-length(tspassport)
k<-length(fit_3$coef)+1 #fit, fit_2, fit_3 바꿔가며 해보기 
ll<-logLik(fit_3)
AIC<--2*ll+2*k
BIC<--2*ll+log(nobs)*k
AICc<-AIC+2*k*(k+1)/(nobs-k-1)

AIC
BIC
AICc

#잔차 분석
jarque.bera.test(resid(fit))
LjungBox(fit)

#예측
Fit<-Arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12)) 

fcast1<-forecast(Fit,h=12) 
plot(fcast1)

autoplot(fcast1)+
  autolayer(tspassport,series="Data")
+autolayer(fcast1$mean,series="Forecasts")

#과적합 진단
fit3_1_0<-arima(lts,order=c(3,1,0),seasonal=list(order=c(0,1,1),period=12))
fit3_1_0
coeftest(fit3_1_0)

fit2_1_1<-arima(lts,order=c(2,1,1),seasonal=list(order=c(0,1,1),period=12))
fit2_1_1
coeftest(fit2_1_1)

fitseasonal1_1_1<-arima(lts,order=c(2,1,0),seasonal=list(order=c(1,1,1),period=12))
fitseasonal1_1_1
coeftest(fitseasonal1_1_1)

fitseasonal0_1_2<-arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12))
fitseasonal0_1_2
coeftest(fitseasonal0_1_2)

#실제 값 vs 예측 값 진단
passport<-read.csv("C:/Users/ksc4you/Desktop/korea.csv",header=T)
colnames(passport)<-c("Date","Total")
passport<-passport[,2]
korea2010_2018<-passport[1:108,] #2019년 제외한 데이터
korea2010_2018<-korea2010_2018[,2]
korea2010_2018<-ts(korea2010_2018,start=c(2010),frequency=12)
tsdisplay(korea2010_2018)

ltskorea<-log(korea2010_2018)
fitkorea<-Arima(ltskorea,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))
fitkorea
fcast2<-forecast(fitkorea,h=12)
plot(fcast2)

fit_2<-arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))
fit_2
jarque.bera.test(resid(fit_2))

auto.arima(lts)
fitted<-arima(lts,order=c(2,1,0),seasonal=list(order=c(0,1,1),period=12))
fitted
jarque.bera.test(resid(fitted))
Box.test(resid(fitted),lag=119,fitdf=0,type="Lj")

FC2019<-matrix(c(548274.3,411954.2,419571.5,377085.7,403216.2, 410274.5,435508.1,
                 413714.8,326089.9,387979.9,417386.3,466433.8),ncol=1)

OR2019<-matrix(passport[109:120],ncol=1)
OR2019
diff<-matrix(OR2019-FC2019, ncol=1)
com<-data.frame(OR2019, FC2019,diff)
diff_percentage<-diff/OR2019

#Mean percentage Error
mean(diff_percentage)

#OR2019&FC2019 plot
se<-function(x){se<-sd(x)/sqrt(length(x))}
plot(1:12,OR2019,type="",ylim=c(280000,600000),lwd=1)
arrows(1:12,OR2019-se(FC2019),1:12,OR2019+se(FC2019),code=3,angle=90,length=0.1)
par(new=TRUE)
arrows(1:12,FC2019,1:12,OR2019,code=1,angle=90,length=0.05)

plot(1:12,FC2019,pch=20,ylim=c(280000,600000))
plot(1:12,FC2019,type="o",col=2)


