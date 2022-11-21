library(forecast)
library(dplyr)
library(urca)
library(orcutt)



aapl

aapl.ts <- as.ts(aapl$Close)

plotts.wge(aapl.ts)
acf(aapl.ts)

aic5.wge(aapl.ts)
aic5.wge(aapl.ts, type = 'bic')
aic5.wge(aapl.ts, type = 'aicc')

aapl$index <- 1:nrow(aapl)

# OLS: time appears to be significant (rejected the null)
fit = lm(Close~index, data = aapl)
summary(fit)

# Cochrane-Orcutt: time does NOT appear to be significant (failed to reject the null)
# Small chance (or evidence of a deterministic trend in the data)
cfit = cochrane.orcutt(fit)
summary(cfit)


aic.wge(aapl.ts, p = 0:10, q = 0:5)
aic5.wge(aapl.ts, p = 0:10, q = 0:5)



aapl.ts2 = aapl.ts[(length(aapl.ts)-364):length(aapl.ts)]
#aapl.ts
#aapl.ts2


aapl.pred1 = fore.arma.wge(aapl.ts, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                           theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130), n.ahead = 5, lastn = T, limits = F)

aapl.pred1.1 <- fore.arma.wge(aapl.ts, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                           theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130), n.ahead = 5, lastn = F, limits = T)


ASE1 = mean((aapl.pred1$f - aapl.ts2[361:365])^2)
ASE1 #53.289


aapl.pred2 = fore.arma.wge(aapl.ts, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                           theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130), n.ahead = 30, lastn = T, limits = F)

aapl.pred2.1 = fore.arma.wge(aapl.ts, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                           theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130), n.ahead = 30, lastn = F, limits = T)

ASE2 = mean((aapl.pred2$f - aapl.ts[336:365])^2)
ASE2 #71.685


#1.079
roll.win.rmse.wge(aapl.ts, horizon = 5, d=0, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                  theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130))

#2.605
roll.win.rmse.wge(aapl.ts, horizon = 30, d=0, phi = c(-0.322309522, 1.057487180, -0.055477750, -0.844429717, 0.488404731, 0.610026678, -0.002045086, -0.002878810, 0.046165984, 0.024858653), 
                  theta = c(-1.2731792, -0.1666368, -0.2501280, -1.1441992, -0.6634130))



aapl.ts
begin = aapl.ts[(length(aapl.ts)-4):length(aapl.ts)]
begin

aapl.pred1

#closeFcst = as.list(aapl.pred1$f+begin, aapl.pred1$ll+begin, aapl.pred1$ul+begin)
closeFcst = data.frame(cbind(aapl.pred1$f, aapl.pred1$ll, aapl.pred1$ul))+begin

closeFcst

plot(seq(1,365,1),aapl.ts[(length(aapl.ts)-364):length(aapl.ts)], type='l', xlim=c(0,365), ylab='Stock Prediction', main='5 day stock prediction') + lines(seq(361,365,1), closeFcst[,1], type='l', col='red')
plot(closeFcst[,1], type = 'l') + lines(seq(361,365,1), closeFcst[,2], type='l', col='blue') + lines(seq(361,365,1), closeFcst[,3], type='l', col='blue')

plot(aapl.ts[(length(aapl.ts)-364):length(aapl.ts)], type='l')
aapl.ts[(length(aapl.ts)-4):length(aapl.ts)]
