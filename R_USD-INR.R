data_USD<- read.csv('C:/Users/hrith/OneDrive/Documents/RESUME_PROJECTS_COPY/Linear_regression_TSA/USD-INR/USD-INR.csv')
View(data_USD)
AdjClose<- as.ts(data_USD$Adj.Close)
L_AdjClose<- log(AdjClose)
L_AdjClose<- as.ts(data_USD$Adj.Close)
#visualisation
plot(AdjClose)
plot(L_AdjClose)

#plotting the ACF, PACF and difference
acf(L_AdjClose)
pacf(L_AdjClose,ylim=c(-0.1,0.2))
plot(diff(L_AdjClose))
plot(diff(diff(L_AdjClose)))

#Hypothesis testing for stationarity on L_AdjClose
#install.packages('tseries')
library(tseries)
adf.test(L_AdjClose)
pp.test(L_AdjClose)
kpss.test(L_AdjClose)

#Hypothesis testing for stationarity on diff(L_AdjClose)
#install.packages('tseries')
adf.test(diff(L_AdjClose))
pp.test(diff(L_AdjClose))
kpss.test(diff(L_AdjClose))      #TS became stationary after taking 1st difference

#plotting the ACF, PACF for stationary TS
acf(diff(L_AdjClose))
pacf(diff(L_AdjClose),ylim=c(-0.1,0.2))

#testing whether after differencing once, if we got white noise
Box.test(diff(L_AdjClose),lag=200)      #took lag=200 as a thumb rule of lag=0.2*no.of data points    
Box.test(diff(L_AdjClose),lag=200,type="L")    # not a white noise   

#fitting ARIMA with d=1
arma_aics<-function(x,P,d,Q)
   {
     aics<-matrix(nrow=P+1,ncol=Q+1)
     for(p in 0:P)
       for(q in 0:Q)
         {
           mdl<-arima(x,order=c(p,d,q),method = "ML")
           if( mdl$code==0 ) aics[p+1,q+1]<-mdl$aic
           }
     return(aics)
   }
aic10<-arma_aics(diff(L_AdjClose),10,1,10)
aic10
sort(aic10)
best_ARIMA_model <- arima(diff(L_AdjClose),order=c(6,1,3),method = "ML")
best_ARIMA_model
best_ARIMA_model_modified <- arima(diff(L_AdjClose),order=c(6,1,3),method = "ML", fixed=c(NA,NA,NA,0,0,NA,NA,NA,NA))
best_ARIMA_model_modified
# we did't get any lower AIC hence we will stick with the above model

#checking the assumptions
res<- residuals(best_ARIMA_model_modified)
plot(res)
qqnorm(res)
acf(res,ylim=c(-0.1,0.15))
pacf(res)
tsdiag(best_ARIMA_model)
library(nortest)
p_values_df <- data.frame(
  Test = c("Anderson-Darling", "Shapiro-Wilk", "Cramer-von Mises", "Lilliefors (Kolmogorov-Smirnov)", "Shapiro-Francia"),
  P_Value = c(
    ad.test(res)$p.value,
    shapiro.test(res)$p.value,
    cvm.test(res)$p.value,
    lillie.test(res)$p.value,
    sf.test(res)$p.value
  )
)
p_values_df
#residuals are not normal , but still the estimates remains robust empirically
Box.test(res,lag=20)
Box.test(res,type="L",lag=20)

#analysing res^2
acf(res^2)
pacf(res^2)
Box.test(res^2,lag=20)
Box.test(res^2,type="L",lag=20)      #res^2 is not white noise

#plotting IRFs
 irfplot<-function (irf, s)
   {
     n <- length(irf)
     plot(c(0,n+1), range(c(irf,1)), type = "n",xlab = "Time", ylab = "IRF", main = s)
     lines(c(0,n+1),c(0,0))
 lines(c(0,0),c(0,1))
 for (i in 1:n)
 lines(c(i,i), c(0, irf[i]))
 }
psi<-ARMAtoMA(ar=c(-1.5074,-0.9539,0.0071,0,0,0.0253), ma=c(0.4860,-0.5304,-0.9556),
                lag.max=30)
irfplot(psi,"IRF of underlying ARMA")
irfplot(cumsum(psi)+1,"IRF of ARIMA")

#prediction of July 2023
# Assuming you have already fitted the ARIMA model and named it as arima_model

# Load the forecast package if not already loaded
#install.packages("forecast") # Uncomment and run this line if you haven't installed the package
library(forecast)


