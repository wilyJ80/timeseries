# Palestra
#https://youtu.be/gJlGCYwfSRE?feature=shared
#Modelo Box-Jenkins (ARIMA) usando o R.

#Procedimentos para aplica��o do Box-Jenkins, utilizando o ARIMA 
#� definido por duas caracter�sticas, a primeira � identificar 
#o modelo e a segunda estimar o mesmo.
#Inicialmente para a aplica��o do ARIMA:
#1-Inicialmente a s�rie deve ser estacion�ria, 
#com caracter�sticas estoc�sticas semelhantes ao longo da s�rie,
#como m�dia, vari�ncia e covari�ncia, com os valores em torno da
# m�dia e vari�ncia constante. 
#2-Caso a s�rie n�o seja estacion�ria, desta forma � necess�rio
# realizar as integra��es ou diferencia��es, induzindo a mesma
# ao ponto de torn�-la estacion�ria.

require (tseries)
dados=read.table("dados-co2.txt",header=T,sep="",dec=",")# entrada de dados. Pasta Dose NA
names(dados)
dados
attach(dados)

#Gr�fico co2
par(mfrow = c(2, 1))
plot(dados$ano, dados$co2, col="3",xlab="ano", ylab="CO2-ppm")#grafico de dispers�o
plot(dados$ano, dados$co2, type="l", col="3",xlab="ano", ylab="CO2-ppm")#grafico de dispers�o

##############################################################

# Este pr�ximo passo realizar� a decomposi��o "stl" da s�rie, 
#ao qual mostrar� a s�rie de dados, tend�ncia, sazonalidade e valores aleat�rios. 
#a frequencia tem que ser maior que um.

# Fun��o STL
#co2
Sco2<-ts(co2,frequency=12, start = c(1958, 1), end = c(2020, 5))
Sco2
plot(Sco2)
log(Sco2)
plot(stl(Sco2, s.window="periodic"),main=" Decomposi��o da CO2 via Loess- f=12")


#Neste pr�ximo passo, determina-se os valores de FAC,
# com o objetivo de identificar se a s�rie utilizada � estacion�ria, 
#mediante ao gr�fico abaixo. A FAC ou ACF (FUN�AO DE AUTOCORRELA��O) 
#de lag 1 permite constatar ou verificar 
#a necessidade de INDUZIR A ESTACIONARIDADE (retirar a sazonalidade)
# atrav�s de uma diferencia��o 
#visto que a FAC verifica a ciclicidade ou sazonalidade
# quando ao sinal e que possui valores menores que o intervalo de confian�a
# sendo muito pr�ximo a zero, verificando a necessidade de diferencia-la 
#para induzir a mesma a estacionaridade. 

# co2
par(mfrow=c(1,2))
plot(Sco2,main='CO2 ',xlab='Anos',ylab='(CO2-ppm)')
acf (Sco2,xlab='defasagem',ylab='fac',main='',na.action=na.pass)


#Fun��o de autocorrela��o (ACF) 
#Essa fun��o � utilizada para determinar se a s�rie � estacion�ria:
#Utiliza-se o gr�fico da fun��o de autocorrela��o (FAC) 
#e de seu intervalo de confian�a. Se a FAC apresenta um decr�scimo acentuado
#nos seus primeiros valores, significa que a s�rie � estacion�ria e d = 0. 
#Se o decr�scimo for suave at� atingir zero, significa que n�o � estacion�ria
#e ter� o valor do termo d determinado pelo n�mero de diferencia��o. 

#A cada diferencia��o realiza-se e novo teste de estacionaridade. 

#Neste pr�ximo passo ser� realizado a primeira diferencia��o de 1� ordem
#que consiste entre a diferen�a do m�s atual menos o anterior
# Isso visa induzir estacionaridade na s�rie

#Estudo da co2

plot(diff(Sco2),main='S�rie Diferenciada da co2eratura (1)',xlab='Anos',ylab=' (co2-ppm) ')
min.m = acf(diff(Sco2),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferen�a', xlab='defasagem',ylab='fac')

# Os gr�ficos de FAC e 1 diferencia��o
par(mfrow=c(2,2))
plot(Sco2,main='',xlab='Anos',ylab='CO2')
acf (Sco2,xlab='defasagem',ylab='fac',main='',na.action=na.pass)
plot(diff(Sco2),main='S�rie Diferenciada (1)',xlab='Anos',ylab=' IUV ')
min.m = acf(diff(Sco2),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferen�a', xlab='defasagem',ylab='fac')


#Diferencia��o do termo sazonal
#Foi feita uma diferencia��o simples e outra sazonal 
#possibilitando a estacionalidade da s�rie como mostra o gr�fico da ACF

#An�lise da ACF e PACF da s�rie diferenciada simples e sazonal.

#1 diferencia��o simples: diff(Sco2) e depois 1 diferencia��o sazonal:(diff(diff(Sco2),lag=12)

plot(diff(diff(Sco2),lag=12),main='S�rie 1 Diferenca Simples e 1 Sazonal',xlab='Anos',ylab=' co2-ppm ')
min.m = acf(diff(diff(Sco2),lag=12), lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main='1 Diferen�a Simples e 1 Sazonal')

#O procedimento do SARIMA � semelhante ao ARIMA, mas de car�ter sazonal,
# � um modelo ARIMA sazonal multiplicativo.
# Neste passo ser� aplicado a diferencia��o de maneira sazonal,
#sendo esta com o lag 12, ao qual esta refer�ncia busca o valor do m�s 
#atual frente ao mesmo m�s do ano anterior.

#1 diferencia��o simples e depois 1 diferencia��o sazonal para ACF e depois PACF

par(mfrow=c(2,2))
plot(diff(diff(Sco2),lag=12),main='',xlab='Anos',ylab=' CO2-ppm ')
min.m = acf(diff(diff(Sco2),lag=12), lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main='')
plot(diff(diff(Sco2),lag=12),main='',xlab='Anos',ylab='co2-ppm')
pmin.m = pacf(diff(diff(Sco2),lag=12), lag.max=36, plot=F, na.action=na.pass)
pmin.m$lag = pmin.m$lag*12
plot(pmin.m, main='')

#Termo da parte autoregressiva-Ver PACF
#Valor de p no Modelo ARIMA (p,d,q)
#Determinar a ordem da parte autoregressiva (valor de p) 
#atrav�s do gr�fico da fun��o de autocorrela��o parcial, FACP. 
#O n�mero de valores da FACP que estiverem acima do intervalo de confian�a
#ser� considerado o valor m�ximo que p poder� atingir

#Ordem da parte de m�dia m�vel-Ver ACF
# Valor de q no Modelo ARIMA (p,d,q)
#Determinar a ordem da parte de m�dia m�vel 
#(valor de q) atrav�s do gr�fico da fun��o de autocorrela��o (FAC). 
#O n�mero de valores da FAC que estiverem acima do seu intervalo de confian�a
# representa o valor m�ximo que q poder� atingir.

#Modelos ARIMA, ARMA, AR ou MA
# Assim, o modelo pode ser expresso por: ARIMA (1, 1, 1), 
#onde p representa a ordem da parte autoregressiva, 
#q a ordem da parte de m�dia m�vel e
# d o n�mero de diferencia��es realizadas para estacionariz�-la.
# Os modelos, quando n�o apresentam diferencia��o em suas s�ries,
# podem ainda ser denominados como ARMA (p, q); quando s� apresentam 
#a parte autoregressiva, como AR (p),
#ou quando apresentam apenas a parte de m�dias m�veis, MA (q).
#da determina��o das ordens de p (parte autoregressiva), 
#de d (quantidade de diferencia��o) 
#e q (parte dos termos de erros defasados � m�dia m�vel)

# Resultados do ARIMA

#Resultados a=(?) e q=(?) e (?) diferencia��es ( n�o usados no modelo)

#Modelo ARIMA 

m1 = arima(Sco2,order=c(1,1,1),include.mean=F)
m1# determina para o modelo os coeficientes de p(AR) e q(Ma)

#Modelo SARINA
m2=arima(x = Sco2, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)))
m2# determina para o modelo os coeficientes de p(AR) e q(Ma) da parte sazonal 

#Diagnostic checking
tsdiag(m1)# diagnostico ARIMA
tsdiag(m2)# diagnostico SARIMA


#An�lise dos Res�duos da S�rie ap�s induzir estacionalidade.
# para ARIMA
par(mfrow=c(2,1))
z=m1$residuals
hist(z,freq=F)
d = seq(range(z)[1]-3*sd(z),range(z)[2]+3*sd(z),0.001)
lines(d,dnorm(d,0,sd(z)))
qqnorm(z)
qqline(z)
#Teste de normalidade Shapiro-Wilk
shapiro.test(z)
#The Box�Pierce (and Ljung�Box)
Box.test(m1$residuals,lag=1)# para ARIMA


# para SARIMA
z1=m2$residuals
hist(z1,freq=F)
d1 = seq(range(z1)[1]-3*sd(z1),range(z1)[2]+3*sd(z1),0.001)
lines(d1,dnorm(d1,0,sd(z)))
qqnorm(z)
qqline(z)
#Teste de normalidade Shapiro-Wilk
shapiro.test(z1)
#Teste da idependencia dos residuos
#The Box�Pierce (and Ljung�Box)
Box.test(m2$residuals,lag=1)# para SARIMA

#Prediction of ARIMA�Models

par(mfrow=c(2,1))
plot(Sco2,main="Previs�o co2. ar-ARIMA", ylab="(CO2-ppm)", xlim=c(2020,2030),ylim=c(400,430))
pred1<-predict(m1,n.ahead=240)
lines(pred1$pred,col="red")
lines(pred1$pred+2*pred1$se,col="red",lty=3)
lines(pred1$pred-2*pred1$se,col="red",lty=3)

#Prediction of SARIMA�Models

plot(Sco2,main="Previs�o CO2-SARIMA",ylab="(CO2-ppm)", xlim=c(2020,2030),ylim=c(400,440))
pred1<-predict(m2,n.ahead=240)
lines(pred1$pred,col="blue")
lines(pred1$pred+2*pred1$se,col="blue",lty=3)
lines(pred1$pred-2*pred1$se,col="blue",lty=3)







