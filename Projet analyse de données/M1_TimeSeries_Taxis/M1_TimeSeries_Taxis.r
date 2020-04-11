#Packages

library(ggplot2)
library(forecast)
library(ggfortify)
library(tseries)
library(KFAS)
library(rugarch)

########################################### PRELIMINARY ####################################################

all_taxi <- read.csv("Weather_central_park_final.csv", sep =";")
all_taxi$date=as.Date(all_taxi$date, format = "%d/%m/%Y") #We convert the date in date format

n=length(all_taxi$pickups)

data <- ts(all_taxi$pickups, frequency = 7, start = 1)   #We convert data in time series
# We take frequency=7 and not 365 to avoid problems with leap year (2016)

true_data<-all_taxi$pickups[1:(n-10)]             #data for train
true_data_test <- all_taxi$pickups[(n-9):n]       #data for test
taxi<-all_taxi[1:(n-10),]


########################## I) PREPROCESSING  ########################################"

mean(true_data) 
sqrt(var(true_data))    

#First representation of our data

ggplot(taxi,aes(date, pickups, group=1))+
  geom_line()


# create a differenced series
data_all <- diff(data, lag=7)/100   #X_t=(D_t-D_(t-7))/100

data_test <- data_all[(length(data_all)-9):length(data_all)] 
data <- data_all[1:(length(data_all)-10)]

taxi$diff<-c(rep(0,7),data)      
ggplot(taxi,aes(date, diff, group=1))+
  geom_line()


######################## II) Model fitting on the time series of interest  ######################

########## 1) MA Model #############

# Find order of MA

ggAcf(data, main='ACF Plot', lag.max=100)  #package forecast
#Last signifcant value at lag 9 

# Fit  MA(9)

fitma <- arima(data,order=c(0,0,9), include.mean = FALSE)
fitma

# Residuals

ggtsdiag(fitma, gof.lag=20)                 

########## 2) AR Model #############

# Find order of AR

pacf(data, lag.max=100)

# Fit AR(8)

fitar<-arima(data,order=c(8,0,0), include.mean = FALSE)
fitar

# Residuals

ggtsdiag(fitar)

########## 3) ARMA Model #############


# Find the parameters of ARMA with AIC

AIC=matrix(0,ncol=9 ,nrow = 9)
for (i in 1:9){
  for (j in 1:9){
    AIC[i,j] = arima(data,order=c(i,0,j), include.mean = FALSE)$aic
  }
}
colnames(AIC) <- c(1,2,3,4,5,6,7,8,9)
rownames(AIC) <- c(1,2,3,4,5,6,7,8,9)
AIC    #We search for the smallest value

p_opt=4 
q_opt=9

#FIT ARMA(4,9)

fitarma<-arima(data,order=c(p_opt,0,q_opt), include.mean = FALSE)
fitarma
ggtsdiag(fitarma)


AIC(fitar,fitma,fitarma) 
# We choose the ARMA model as it has the smaller AIC


#On représente les residus graphiquement : 

eps<-fitarma$residuals
fill_eps<-c(eps,rep(NA,7))
ggplot(taxi, aes(date,fill_eps)) + geom_line() + scale_x_date('month')  + ylab("ARMA Residuals")

eps2 <- eps^2
fill_eps2<-c(eps2,rep(NA,7))
ggplot(taxi, aes(date,fill_eps2)) + geom_line() + scale_x_date('month')  + ylab("ARMA Squared Residuals")


# ACF on residuals and squared residuals
acf(eps,lag.max=100)
acf(eps2,lag.max=100)

qqnorm(eps)
qqline(eps,col=2)
#It's not normally distributed


########## 5) GARCH model  #############

# GARCH(1,1) estimation on the residuals


objf.garch <- function(vartheta,
                       eps,n,sig2init,petit=sqrt(.Machine$double.eps),r0=10){   
  omega <- vartheta[1]
  alpha <- vartheta[2]
  beta <- vartheta[3]
  sig2<-rep(0,n)
  sig2[1]<-sig2init
  for(t in 2:n){
    sig2[t]<-omega+alpha*eps[t-1]^2+beta*sig2[t-1]
  }
  qml <- mean(eps[(r0+1):n]^2/sig2[(r0+1):n]+log(sig2[(r0+1):n]))
  qml }

VarAsymp<- function(omega,alpha,beta,eps,sig2init,petit,r0=10){
  n <- length(eps)
  dersigma2<-matrix(0,nrow=3,ncol=n)
  sig2<-rep(0,n)
  sig2[1]<-sig2init
  for(t in 2:n){
    vec<-c(1,eps[t-1]^2,sig2[t-1])
    sig2[t]<-omega+beta*sig2[t-1]+alpha*eps[t-1]^2
    dersigma2[1:3,t]<-vec/sig2[t]+beta*dersigma2[1:3,(t-1)]
  }
  eta <- eps[(r0+1):n]/sqrt(sig2)[(r0+1):n]
  eta <- eta/sd(eta)
  
  J<-dersigma2[1:3,(r0+1):n]%*%t(dersigma2[1:3,(r0+1):n])/(n-r0)
  kappa4<-mean(eta^4)
  
  {if(kappa(J)<1/petit) inv<-solve(J) else inv<-matrix(0,nrow=3,ncol=3)}
  var<-(kappa4-1)*inv
  list(var=var,residus=eta, sigma2=sig2)
}


estimGARCH<-
  function(omega,alpha,beta,eps,petit=sqrt(.Machine$double.eps),r0=10)
  {
    valinit<-c(omega,alpha,beta)
    n <- length(eps)
    sig2init<-var(eps[1:min(n,5)])
    res <- nlminb(valinit,objf.garch,lower=c(petit,0,0),
                  upper=c(Inf,Inf,1), eps=eps,n=n,sig2init=sig2init)
    omega <- res$par[1]
    alpha<- res$par[2]
    beta <- res$par[3]
    
    var<-VarAsymp(omega,alpha,beta,eps,sig2init,petit=sqrt(.Machine$double.eps),r0=10)
    list(coef=c(omega,alpha,beta),residus=var$residus,var=var$var, sigma2=var$sigma2)
  }

# Estimation

par(mfrow=c(1,1))

omega.init<- 0.01
alpha.init<-0.01
beta.init<- 0.01

fitgarch<-estimGARCH(omega.init,alpha.init,beta.init,eps)
parg<-fitgarch$coef
resg<-fitgarch$residus
qqnorm(resg)
qqline(resg,col=2)

# Once again, residuals seems to not be normally ditributed

acf(resg)
acf(resg^2)
#ACF are much better ! Except for one pick

# Test Beta=0
beta=parg[3]
sigmabeta=fitgarch$var[3,3]
se=sqrt(sigmabeta)/sqrt(T)
t.value=beta/se
p.value=2*pnorm(-t.value) # Asymmetry
print(p.value)

# p-value = 0,925 > 0,05 One cannot reject H0 : beta=0


# Estimation ARCH(1) 

estimARCH<-
  function(omega,alpha,beta=0,eps,petit=sqrt(.Machine$double.eps),r0=10)
  {
    valinit<-c(omega,alpha,beta)
    n <- length(eps)
    sig2init<-var(eps[1:min(n,5)])
    res <- nlminb(valinit,objf.garch,lower=c(petit,0,0),
                  upper=c(Inf,Inf,0), eps=eps,n=n,sig2init=sig2init)
    omega <- res$par[1]
    alpha<- res$par[2]
    
    var<-VarAsymp(omega,alpha,beta,eps,sig2init,petit=sqrt(.Machine$double.eps),r0=10)
    list(coef=c(omega,alpha),residus=var$residus,var=var$var[1:2,1:2])
  }
omega.init<- 0.01
alpha.init<-0.01

fitarch<-estimARCH(omega.init,alpha.init,eps=eps)
par<-fitarch$coef
res<-fitarch$residus
qqnorm(res)
qqline(res,col=2)
acf(res)
acf(res^2)

# Test alpha=0

alpha=par[2]
sigmaalpha=fitarch$var[2,2]
se=sqrt(sigmaalpha)/sqrt(T)
t.value=alpha/se
p.value=2*pnorm(-t.value)
print(p.value)

######### 6) Prediction intervals for the 10 most recent data ##########

# Predictions Intervals given by the two step procedure

sigma2 = fitgarch$sigma2

omega = parg[1]
alpha = parg[2]
beta = parg[3]

sigma2_hat = rep(0,10)
sigma2_hat_init = sigma2[1166] 
sigma2_hat[1] = sigma2_hat_init
for(t in 2:10){
  sigma2_hat[t]<-omega+(alpha+beta)*sigma2_hat[t-1] 
}

# Forecast given by the ARMA model 
pi_forecast <- forecast(data,model = fitarma,h=10)
pi_hat <- pi_forecast$mean

# We compute the prediction intervals given by the two steps procedure
lower_2step <- pi_hat - 2*sqrt(sigma2_hat)
upper_2step <- pi_hat + 2*sqrt(sigma2_hat)



############## Predictions Intervals given by the one step procedure ###########################


model<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,
                                                                         1)),
                  mean.model = list(armaOrder = c(p_opt, q_opt), include.mean =
                                      FALSE),
                  distribution.model = "norm")


modelfit<-ugarchfit(spec=model,data=data)
par(mfrow=c(1,1))

forecast=ugarchforecast(modelfit,n.ahead = 10)@forecast
first_forecast=forecast$series

lower_1step<-forecast$series-2*forecast$sigma
upper_1step<-forecast$series+2*forecast$sigma

pred <- data.frame(onestep_low <- c(rep(NA,20), lower_1step),
                  onestep_up <- c(rep(NA,20), upper_1step),
                  twostep_low <- c(rep(NA,20), lower_2step),
                  twostep_up<- c(rep(NA,20), upper_2step),
                  prediction <- c(rep(NA,20), forecast$series),
                  pickups <- data_all[(length(data_all)-29):length(data_all)],
                  time <- all_taxi$date[(n-29):n])

ggplot(data=pred, aes(x=time, group=1)) + 
  geom_line(data=pred, aes(x=time, y=pickups, group=1))+
  geom_line(data=pred, aes(x=time, y=onestep_low, color='LOWER ONE STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=onestep_up, color='UPPER ONE STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=prediction, color='FORECAST', group=1))+
  geom_line(data=pred, aes(x=time, y=twostep_low, color='LOWER TWO STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=twostep_up, color='UPPER TWO STEP', group=1)) + ylab("")




#We have a prediction on our differenciate series ! Let's look on the prediction of our actual series: 

forecast=ugarchforecast(modelfit,n.ahead = 10)@forecast


forecast$series[1:7]=100*forecast$series[1:7] + true_data[(length(true_data)-6):length(true_data)]
forecast$series[8:10]=100*forecast$series[8:10] + forecast$series[1:3]  
#Xt=(Dt-D_(t-7))/100
#So Dt=100Xt+D_(t-7)

pi_hat[1:7]<-100*pi_hat[1:7]+true_data[(length(true_data)-6):length(true_data)]
pi_hat[8:10] <- 100*pi_hat[8:10] + pi_hat[1:3]

true_lower_1step <- forecast$series-2*100*forecast$sigma
true_upper_1step <- forecast$series+2*100*forecast$sigma

true_lower_2step <- pi_hat - 2*100*sqrt(sigma2_hat)
true_upper_2step <- pi_hat + 2*100*sqrt(sigma2_hat)


true_pred <- data.frame(true_onestep_low <- c(rep(NA,20), true_lower_1step),
                   true_onestep_up <- c(rep(NA,20), true_upper_1step),
                   true_twostep_low <- c(rep(NA,20), true_lower_2step),
                   true_twostep_up<- c(rep(NA,20), true_upper_2step),
                   true_prediction <- c(rep(NA,20), forecast$series),
                   true_pickups <- all_taxi$pickups[(n-29):n],
                   time <- all_taxi$date[(n-29):n])

ggplot(data=true_pred, aes(x=time, group=1)) + 
  geom_line(data=true_pred, aes(x=time, y=true_pickups, group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_onestep_low, color='LOWER ONE STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_onestep_up, color='UPPER ONE STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_prediction, color='FORECAST', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_twostep_low, color='LOWER TWO STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_twostep_up, color='UPPER TWO STEP', group=1)) + ylab("")


######################### III) Training on the times series of interest using explanatory times series #################""""

############### 1) Preprocessing ####################

#covariance matrix
library("corrplot")

mcorr_data <- cor(all_taxi[,2:9])

corrplot(mcorr_data, method="square")

#pickup

pickup<-ts(all_taxi$pickups, frequency = 7, start = 1) 
pickup<-diff(pickup, lag=7)/100

#min_temp

min_temp<-ts(all_taxi$min_temp, frequency = 7, start = 1)
ggplot(all_taxi, aes(date, min_temp))+geom_line()           #without differencing 

min_temp<-diff(min_temp, lag=7)/100
all_taxi$min_temp_diff<-c(rep(0,7),min_temp)
ggplot(all_taxi, aes(date, min_temp_diff))+geom_line()     #with differencing

#wind_speed

wind_speed <- ts(all_taxi$wind_speed, frequency = 7, start = 1)
ggplot(all_taxi, aes(date, wind_speed))+geom_line()           #without differencing 

wind_speed <- diff(wind_speed, lag=7)/100
all_taxi$wind_speed_diff <-c(rep(0,7),wind_speed)
ggplot(all_taxi, aes(date, wind_speed_diff))+geom_line()        #with differencing

#snow_depth

snow_depth<- ts(all_taxi$snow_depth, frequency = 7, start = 1)
ggplot(all_taxi, aes(date, snow_depth))+geom_line()        #without differencing 

snow_depth <- diff(snow_depth, lag=7)/100
all_taxi$snow_depth_diff <-c(rep(0,7),snow_depth)
ggplot(all_taxi, aes(date, snow_depth_diff))+geom_line()    #with differencing

#visibility

visibility<- ts(all_taxi$visibility, frequency = 7, start = 1)
ggplot(all_taxi, aes(date, visibility))+geom_line()          #without differencing 

visibility <- diff(visibility, lag=7)/100
all_taxi$visibility_diff <-c(rep(0,7),visibility)
ggplot(all_taxi, aes(date, visibility_diff))+geom_line()     #with differencing

#precipitation

precipitation<- ts(all_taxi$precipitation, frequency = 7, start = 1)
ggplot(all_taxi, aes(date, precipitation))+geom_line()      #without differencing 

precipitation <- diff(precipitation, lag=7)/100
all_taxi$precipitation_diff <-c(rep(0,7),precipitation)
ggplot(all_taxi, aes(date, precipitation_diff))+geom_line()   #with differencing

#pressure 


pressure<- ts(all_taxi$pressure, frequency = 7, start = 1)
ggplot(all_taxi, aes(date,pressure))+geom_line()      #without differencing 

pressure <- diff(pressure, lag=7)/100
all_taxi$pressure_diff <-c(rep(0,7),pressure)
ggplot(all_taxi, aes(date, pressure_diff))+geom_line()       #with differencing


############# 2) Time varying coefficients ##############"

n<-length(pickup)
y<- ts(pickup, frequency=7, start=1)
ytraining <-ts(y[-((n-9):n)],frequency=7, start=1)

Y<-ts(cbind(ytraining,lag(ytraining,1), lag(ytraining,7),lag(ytraining,8),lag(ytraining,9),lag(ytraining,10),lag(ytraining,11),lag(ytraining,12),lag(ytraining,13),lag(ytraining,14),lag(ytraining,15),lag(ytraining,16),lag(ytraining,17)))
Y <- Y[-c(1:17,(length(Y[,1])-6):length(Y)),]


model <- SSModel (Y[,-c(1,2,3)] ~-1+SSMregression(~ min_temp[-c(1:7,(n-9):n)] + wind_speed[-c(1:7,(n-9):n)] + snow_depth[-c(1:7,(n-9):n)]+ visibility[-c(1:7,(n-9):n)]+ Y[,3]+Y[,2]+Y[,1], Q=diag(NA,7),R=t(matrix(rep(diag(1,7),10),nrow=7))), H = diag(1,10))


fit <- fitSSM(model, inits = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1), method = "BFGS")


############ 3) QLIK  #########################

model <- fit$model
model$Q     
model$H   
kal <- KFS(model,smoothing = "none",filtering=c("mean","state"))



# the parameters for the 1 step prediction


kalman_data <- data.frame(kal_temp <- as.vector(kal$a[,1]),
                           kal_wind <- as.vector(kal$a[,2]),
                           kal_snow <- as.vector(kal$a[,3]),
                           kal_visibility <- as.vector(kal$a[,4]),
                           kal_previous_lag_1 <- as.vector(kal$a[,5]), 
                           kal_previous_lag_7 <- as.vector(kal$a[,6]),
                           kal_previous_lag_8 <- as.vector(kal$a[,7]),
                           date <- all_taxi$date[15:1174])

# The parameters for the 1 step prediction
ggplot(data=kalman_data, aes(x=date, y=kal_temp)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_wind)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_snow)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_visibility)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_previous_lag_1)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_previous_lag_7)) + geom_line()
ggplot(data=kalman_data, aes(x=date, y=kal_previous_lag_8)) + geom_line()

#Frame with variances

kalman_var <- data.frame(var_temp <- kal$P[1,1,],
                         var_wind <- kal$P[2,2,],
                         var_snow <- kal$P[3,3,],
                         var_visibility <- kal$P[4,4,],
                         var_previous_lag_1 <- kal$P[5,5,],
                         var_previous_lag_7 <- kal$P[6,6,],
                         var_previous_lag_8 <- kal$P[7,7,],
                         var_date <- all_taxi$date[15:1174])

# The associated variances of the parameters
ggplot(data=kalman_var, aes(x=date, y=var_temp)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_wind)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_snow)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_visibility)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_previous_lag_1)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_previous_lag_7)) + geom_line()
ggplot(data=kalman_var, aes(x=date, y=var_previous_lag_8)) + geom_line()

############ 4) Prediction  #########################


yhat<-ts(kal$m[,1],frequency =7, start=1)
ts.plot(yhat)

vol<-ts(kal$F[1,],frequency = 7, start = 1)
ts.plot(vol)
# There is a high volatility effect due to the time varying coefficients! At the begining the high volatility is due to the uncertainty (risky) prediction
# yhat due to the lack of observations

# intervals of predictions in sample
upperCI<-ts(yhat+2*sqrt(vol),frequency = 7, start = 1)
lowerCI<-ts(yhat-2*sqrt(vol),frequency = 7, start = 1)

ts.plot(ytraining,yhat,upperCI,lowerCI,col=1:4)


ypred<-kal$m[length(Y[,1]),] 
volpred<-kal$F[,length(Y[,1])]
ts.plot(cbind(pickup[(n-9):n],as.numeric(ypred),as.numeric(ypred+2*sqrt(volpred)),as.numeric(ypred-2*sqrt(volpred))),col=
          1:4)
legend("topright",c("real","predict_Kalman"), col = c(1,2), lty=1)

true_ypred=rep(0,10)
true_ypred[1:7]=100*ypred[1:7] + true_data[(length(true_data)-6):length(true_data)]
true_ypred[8:10]=100*ypred[8:10] + true_ypred[1:3]      

ts.plot(cbind(true_data_test,true_ypred,true_ypred+2*100*sqrt(volpred),true_ypred-2*100*sqrt(volpred)),col=
          1:4)
legend("topright",c("real","predict_Kalman"), col = c(1,2), lty=1)

################################## IV) CONCLUSION #########################

pred$prediction_kalman <- c(rep(NA,20), ypred)
pred$lower_kalman <- c(rep(NA,20), ypred-2*sqrt(volpred))
pred$upper_kalman <- c(rep(NA,20), ypred+2*sqrt(volpred))

ggplot(data=pred, aes(x=time, group=1)) + 
  geom_line(data=pred, aes(x=time, y=pickups, group=1))+
  geom_line(data=pred, aes(x=time, y=prediction_kalman, color='PREDICTION KAL', group=1))+
  geom_line(data=pred, aes(x=time, y=lower_kalman, color='LOWER KAL', group=1))+
  geom_line(data=pred, aes(x=time, y=upper_kalman, color='UPPER KAL', group=1))+
  geom_line(data=pred, aes(x=time, y=onestep_low, color='LOWER ONE STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=onestep_up, color='UPPER ONE STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=prediction, color='FORECAST', group=1))+
  geom_line(data=pred, aes(x=time, y=twostep_low, color='LOWER TWO STEP', group=1))+
  geom_line(data=pred, aes(x=time, y=twostep_up, color='UPPER TWO STEP', group=1)) + ylab("")

  
true_pred$prediction_kalman <- c(rep(NA,20), true_ypred)
true_pred$lower_kalman <- c(rep(NA,20), true_ypred-2*100*sqrt(volpred))
true_pred$upper_kalman <- c(rep(NA,20), true_ypred+2*100*sqrt(volpred))


ggplot(data=true_pred, aes(x=true_pred$time, group=1)) + 
  geom_line(data=true_pred, aes(x=time, y=true_pickups, group=1))+
  geom_line(data=true_pred, aes(x=time, y=prediction_kalman, color='PREDICTION KAL', group=1))+
  geom_line(data=true_pred, aes(x=time, y=lower_kalman, color='LOWER KAL', group=1))+
  geom_line(data=true_pred, aes(x=time, y=upper_kalman, color='UPPER KAL', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_prediction, color='FORECAST GARCH', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_onestep_low, color='LOWER ONE STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_onestep_up, color='UPPER ONE STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_twostep_low, color='LOWER TWO STEP', group=1))+
  geom_line(data=true_pred, aes(x=time, y=true_twostep_up, color='UPPER TWO STEP', group=1)) + ylab("")


  
  