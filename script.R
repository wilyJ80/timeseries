# install.packages('tseries')

# Ler dados
require (tseries)
dados=read.table("baggagecomplaints.csv",header=T,sep=",")
# Ler data corretamente de acordo com formato
dados$Date = as.Date(paste("01/", dados$Date, sep = ""), format = "%d/%m/%Y")
attach(dados)
par(mfrow = c(2, 1))
plot(dados$Date, dados$Cancelled, col="3",xlab="Date", ylab="Cancelled")
plot(dados$Date, dados$Cancelled, type="l", col="3",xlab="Date", ylab="Cancelled")

# Decomposição STL
# Ajuste com base nas datas iniciais e finais
Sbag<-ts(Cancelled,frequency=12, start = c(min(Year), min(Month)), end = c(max(Year), max(Month)))
Sbag
plot(Sbag)
log(Sbag)
plot(stl(Sbag, s.window="periodic"),main=" Decomposição de vôos cancelados via Loess- f=12")

# FAC/ACF: Função de Autocorrelação

# Determinar valores de FAC
par(mfrow=c(1,2))
plot(Sbag,main='Cancelados ',xlab='Anos',ylab='Cancelados')
acf (Sbag,xlab='defasagem',ylab='fac',main='',na.action=na.pass)

# Diferenciação: induzir estacionaridade na série
plot(diff(Sbag),main='Série Diferenciada (1)',xlab='Anos',ylab=' (Cancelled) ')
min.m = acf(diff(Sbag),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferença', xlab='defasagem',ylab='fac')

# Gráficos até agora
par(mfrow=c(2,2))
plot(Sbag,main='',xlab='Anos',ylab='Cancelled')
acf (Sbag,xlab='defasagem',ylab='fac',main='',na.action=na.pass)
plot(diff(Sbag),main='Série Diferenciada (1)',xlab='Anos',ylab=' Cancelled ')
min.m = acf(diff(Sbag),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferença', xlab='defasagem',ylab='fac')

# Diferenciação simples e sazonal
plot(diff(diff(Sbag),lag=12),main='Série 1 Diferenca Simples e 1 Sazonal',xlab='Anos',ylab=' Cancelled ')
min.m = acf(diff(diff(Sbag),lag=12), lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main='1 Diferença Simples e 1 Sazonal')

# Diferenciação sazonal
par(mfrow=c(2,2))
plot(diff(diff(Sbag),lag=12),main='',xlab='Anos',ylab=' Cancelled ')
min.m = acf(diff(diff(Sbag),lag=12), lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main='')
plot(diff(diff(Sbag),lag=12),main='',xlab='Anos',ylab='Cancelled')
pmin.m = pacf(diff(diff(Sbag),lag=12), lag.max=36, plot=F, na.action=na.pass)
pmin.m$lag = pmin.m$lag*12
plot(pmin.m, main='')

# Modelo ARIMA
m1 = arima(Sbag,order=c(1,1,1),include.mean=F)
m1# determina para o modelo os coeficientes de p(AR) e q(Ma)

# Modelo SARIMA
m2=arima(x = Sbag, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)))
m2# determina para o modelo os coeficientes de p(AR) e q(Ma) da parte sazonal

# Diagnósticos
tsdiag(m1)# diagnostico ARIMA
tsdiag(m2)# diagnostico SARIMA

# Análise dos resíduos após induzir estacionaridade

# ARIMA
par(mfrow=c(2,1))
z=m1$residuals
hist(z,freq=F)
d = seq(range(z)[1]-3*sd(z),range(z)[2]+3*sd(z),0.001)
lines(d,dnorm(d,0,sd(z)))
qqnorm(z)
qqline(z)
#Teste de normalidade Shapiro-Wilk
shapiro.test(z)
#The BoxPierce (and LjungBox)
Box.test(m1$residuals,lag=1)# para ARIMA

# SARIMA
z1=m2$residuals
hist(z1,freq=F)
d1 = seq(range(z1)[1]-3*sd(z1),range(z1)[2]+3*sd(z1),0.001)
lines(d1,dnorm(d1,0,sd(z)))
qqnorm(z)
qqline(z)
#Teste de normalidade Shapiro-Wilk
shapiro.test(z1)
#Teste da idependencia dos residuos
#The BoxPierce (and LjungBox)
Box.test(m2$residuals,lag=1)# para SARIMA

# Previsões

# ARIMA
par(mfrow=c(2,1))
plot(Sbag,main="Previsão cancelados. ar-ARIMA", ylab="(Cancelled)", xlim=c(max(Year),2020),ylim=c(min(Cancelled),max(Cancelled) + 40))
pred1<-predict(m1,n.ahead=240)
lines(pred1$pred,col="red")
lines(pred1$pred+2*pred1$se,col="red",lty=3)
lines(pred1$pred-2*pred1$se,col="red",lty=3)

# SARIMA
plot(Sbag,main="Previsão cancelados-SARIMA",ylab="(Cancelled)", xlim=c(max(Year),2020),ylim=c(min(Cancelled),max(Cancelled) + 40))
pred1<-predict(m2,n.ahead=240)
lines(pred1$pred,col="blue")
lines(pred1$pred+2*pred1$se,col="blue",lty=3)
lines(pred1$pred-2*pred1$se,col="blue",lty=3)
