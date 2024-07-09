# Palestra
#https://youtu.be/gJlGCYwfSRE?feature=shared
#Modelo Box-Jenkins (ARIMA) usando o R.

#Procedimentos para aplicação do Box-Jenkins, utilizando o ARIMA 
#é definido por duas características, a primeira é identificar 
#o modelo e a segunda estimar o mesmo.
#Inicialmente para a aplicação do ARIMA:
#1-Inicialmente a série deve ser estacionária, 
#com características estocásticas semelhantes ao longo da série,
#como média, variância e covariância, com os valores em torno da
# média e variância constante. 
#2-Caso a série não seja estacionária, desta forma é necessário
# realizar as integrações ou diferenciações, induzindo a mesma
# ao ponto de torná-la estacionária.

require (tseries)
dados=read.table("dados-co2.txt",header=T,sep="",dec=",")# entrada de dados. Pasta Dose NA
names(dados)
dados
attach(dados)

#Gráfico co2
par(mfrow = c(2, 1))
plot(dados$ano, dados$co2, col="3",xlab="ano", ylab="CO2-ppm")#grafico de dispersão
plot(dados$ano, dados$co2, type="l", col="3",xlab="ano", ylab="CO2-ppm")#grafico de dispersão

##############################################################

# Este próximo passo realizará a decomposição "stl" da série, 
#ao qual mostrará a série de dados, tendência, sazonalidade e valores aleatórios. 
#a frequencia tem que ser maior que um.

# Função STL
#co2
Sco2<-ts(co2,frequency=12, start = c(1958, 1), end = c(2020, 5))
Sco2
plot(Sco2)
log(Sco2)
plot(stl(Sco2, s.window="periodic"),main=" Decomposição da CO2 via Loess- f=12")


#Neste próximo passo, determina-se os valores de FAC,
# com o objetivo de identificar se a série utilizada é estacionária, 
#mediante ao gráfico abaixo. A FAC ou ACF (FUNÇAO DE AUTOCORRELAÇÃO) 
#de lag 1 permite constatar ou verificar 
#a necessidade de INDUZIR A ESTACIONARIDADE (retirar a sazonalidade)
# através de uma diferenciação 
#visto que a FAC verifica a ciclicidade ou sazonalidade
# quando ao sinal e que possui valores menores que o intervalo de confiança
# sendo muito próximo a zero, verificando a necessidade de diferencia-la 
#para induzir a mesma a estacionaridade. 

# co2
par(mfrow=c(1,2))
plot(Sco2,main='CO2 ',xlab='Anos',ylab='(CO2-ppm)')
acf (Sco2,xlab='defasagem',ylab='fac',main='',na.action=na.pass)


#Função de autocorrelação (ACF) 
#Essa função é utilizada para determinar se a série é estacionária:
#Utiliza-se o gráfico da função de autocorrelação (FAC) 
#e de seu intervalo de confiança. Se a FAC apresenta um decréscimo acentuado
#nos seus primeiros valores, significa que a série é estacionária e d = 0. 
#Se o decréscimo for suave até atingir zero, significa que não é estacionária
#e terá o valor do termo d determinado pelo número de diferenciação. 

#A cada diferenciação realiza-se e novo teste de estacionaridade. 

#Neste próximo passo será realizado a primeira diferenciação de 1ª ordem
#que consiste entre a diferença do mês atual menos o anterior
# Isso visa induzir estacionaridade na série

#Estudo da co2

plot(diff(Sco2),main='Série Diferenciada da co2eratura (1)',xlab='Anos',ylab=' (co2-ppm) ')
min.m = acf(diff(Sco2),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferença', xlab='defasagem',ylab='fac')

# Os gráficos de FAC e 1 diferenciação
par(mfrow=c(2,2))
plot(Sco2,main='',xlab='Anos',ylab='CO2')
acf (Sco2,xlab='defasagem',ylab='fac',main='',na.action=na.pass)
plot(diff(Sco2),main='Série Diferenciada (1)',xlab='Anos',ylab=' IUV ')
min.m = acf(diff(Sco2),lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main = 'Primeira Diferença', xlab='defasagem',ylab='fac')


#Diferenciação do termo sazonal
#Foi feita uma diferenciação simples e outra sazonal 
#possibilitando a estacionalidade da série como mostra o gráfico da ACF

#Análise da ACF e PACF da série diferenciada simples e sazonal.

#1 diferenciação simples: diff(Sco2) e depois 1 diferenciação sazonal:(diff(diff(Sco2),lag=12)

plot(diff(diff(Sco2),lag=12),main='Série 1 Diferenca Simples e 1 Sazonal',xlab='Anos',ylab=' co2-ppm ')
min.m = acf(diff(diff(Sco2),lag=12), lag.max=36, plot=F, na.action=na.pass)
min.m$lag = min.m$lag*12
plot(min.m, main='1 Diferença Simples e 1 Sazonal')

#O procedimento do SARIMA é semelhante ao ARIMA, mas de caráter sazonal,
# é um modelo ARIMA sazonal multiplicativo.
# Neste passo será aplicado a diferenciação de maneira sazonal,
#sendo esta com o lag 12, ao qual esta referência busca o valor do mês 
#atual frente ao mesmo mês do ano anterior.

#1 diferenciação simples e depois 1 diferenciação sazonal para ACF e depois PACF

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
#através do gráfico da função de autocorrelação parcial, FACP. 
#O número de valores da FACP que estiverem acima do intervalo de confiança
#será considerado o valor máximo que p poderá atingir

#Ordem da parte de média móvel-Ver ACF
# Valor de q no Modelo ARIMA (p,d,q)
#Determinar a ordem da parte de média móvel 
#(valor de q) através do gráfico da função de autocorrelação (FAC). 
#O número de valores da FAC que estiverem acima do seu intervalo de confiança
# representa o valor máximo que q poderá atingir.

#Modelos ARIMA, ARMA, AR ou MA
# Assim, o modelo pode ser expresso por: ARIMA (1, 1, 1), 
#onde p representa a ordem da parte autoregressiva, 
#q a ordem da parte de média móvel e
# d o número de diferenciações realizadas para estacionarizá-la.
# Os modelos, quando não apresentam diferenciação em suas séries,
# podem ainda ser denominados como ARMA (p, q); quando só apresentam 
#a parte autoregressiva, como AR (p),
#ou quando apresentam apenas a parte de médias móveis, MA (q).
#da determinação das ordens de p (parte autoregressiva), 
#de d (quantidade de diferenciação) 
#e q (parte dos termos de erros defasados – média móvel)

# Resultados do ARIMA

#Resultados a=(?) e q=(?) e (?) diferenciações ( não usados no modelo)

#Modelo ARIMA 

m1 = arima(Sco2,order=c(1,1,1),include.mean=F)
m1# determina para o modelo os coeficientes de p(AR) e q(Ma)

#Modelo SARINA
m2=arima(x = Sco2, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1)))
m2# determina para o modelo os coeficientes de p(AR) e q(Ma) da parte sazonal 

#Diagnostic checking
tsdiag(m1)# diagnostico ARIMA
tsdiag(m2)# diagnostico SARIMA


#Análise dos Resíduos da Série após induzir estacionalidade.
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
#The Box–Pierce (and Ljung–Box)
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
#The Box–Pierce (and Ljung–Box)
Box.test(m2$residuals,lag=1)# para SARIMA

#Prediction of ARIMA–Models

par(mfrow=c(2,1))
plot(Sco2,main="Previsão co2. ar-ARIMA", ylab="(CO2-ppm)", xlim=c(2020,2030),ylim=c(400,430))
pred1<-predict(m1,n.ahead=240)
lines(pred1$pred,col="red")
lines(pred1$pred+2*pred1$se,col="red",lty=3)
lines(pred1$pred-2*pred1$se,col="red",lty=3)

#Prediction of SARIMA–Models

plot(Sco2,main="Previsão CO2-SARIMA",ylab="(CO2-ppm)", xlim=c(2020,2030),ylim=c(400,440))
pred1<-predict(m2,n.ahead=240)
lines(pred1$pred,col="blue")
lines(pred1$pred+2*pred1$se,col="blue",lty=3)
lines(pred1$pred-2*pred1$se,col="blue",lty=3)







