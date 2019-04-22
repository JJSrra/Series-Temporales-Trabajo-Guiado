## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(tseries)

## ------------------------------------------------------------------------
serie = scan("Datos/pasajeros_1949_1959.dat")

## ------------------------------------------------------------------------
nTest = 12
nPred = 12

## ------------------------------------------------------------------------
serie.ts = ts(serie, frequency=12)
plot(decompose(serie.ts))

## ------------------------------------------------------------------------
serie.ts.log = log(serie.ts)
serie.log = log(serie)
plot(decompose(serie.ts.log))

## ------------------------------------------------------------------------
serie.train = serie.log[1:(length(serie.log) - nTest)]
tiempo.train = 1:length(serie.train)

serie.test = serie.log[(length(serie.log) - nTest+1):length(serie.log)]
tiempo.test = (length(serie.train)+1):length(serie.log)

## ------------------------------------------------------------------------
plot.ts(serie.train, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, serie.test, col="red")

## ------------------------------------------------------------------------
parametros.H1 = lm(serie.train ~ tiempo.train)

tendencia.train.H1 = parametros.H1$coefficients[1]+tiempo.train*parametros.H1$coefficients[2]
tendencia.test.H1 = parametros.H1$coefficients[1]+tiempo.test*parametros.H1$coefficients[2]

## ------------------------------------------------------------------------
plot.ts(serie.train, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, serie.test, col="red")
lines(tiempo.train, tendencia.train.H1, col="blue")
lines(tiempo.test, tendencia.test.H1, col="green")

## ------------------------------------------------------------------------
JB.train = jarque.bera.test(parametros.H1$residuals)
JB.test = jarque.bera.test(tendencia.test.H1-serie.test)

JB.train
JB.test

## ------------------------------------------------------------------------
TT = t.test(c(parametros.H1$residuals, tendencia.test.H1-serie.test))
TT

## ------------------------------------------------------------------------
serie.train.sinTend = serie.train - tendencia.train.H1
serie.test.sinTend = serie.test - tendencia.test.H1

plot.ts(serie.train.sinTend, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, serie.test.sinTend, col="red")

## ------------------------------------------------------------------------
k = 12
estacionalidad = decompose(serie.ts.log)$seasonal[1:k]
estacionalidad

## ------------------------------------------------------------------------
# Aprovecho el reciclaje de R para no tener que crear una variable auxiliar
serie.train.sinTendSinEst = serie.train.sinTend - estacionalidad
serie.test.sinTendSinEst = serie.test.sinTend - estacionalidad

plot.ts(serie.train.sinTendSinEst, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, serie.test.sinTendSinEst, col="red")

## ------------------------------------------------------------------------
acf(serie.train.sinTendSinEst)

## ------------------------------------------------------------------------
adf.serie.train = adf.test(serie.train.sinTendSinEst)
adf.serie.train

## ------------------------------------------------------------------------
serie.train.sinTendSinEstDif = diff(serie.train.sinTendSinEst)
serie.test.sinTendSinEstDif = diff(serie.test.sinTendSinEst)

adf.serie.train.2 = adf.test(serie.train.sinTendSinEstDif)
adf.serie.train.2

## ------------------------------------------------------------------------
acf(serie.train.sinTendSinEstDif)
pacf(serie.train.sinTendSinEstDif)

## ------------------------------------------------------------------------
modelo = arima(serie.train.sinTendSinEst, order = c(4,1,0))

## ------------------------------------------------------------------------
valoresReconstruidos = serie.train.sinTendSinEst + modelo$residuals

## ------------------------------------------------------------------------
predicciones = predict(modelo, n.ahead = nPred)
valoresPredichos = predicciones$pred
valoresPredichos

## ------------------------------------------------------------------------
error.train = sum(modelo$residuals^2)
error.train

error.test = sum((valoresPredichos-serie.test.sinTendSinEst)^2)
error.test

## ------------------------------------------------------------------------
plot.ts(serie.train.sinTendSinEst, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(valoresReconstruidos, col="blue")
lines(tiempo.test, serie.test.sinTendSinEst, col="red")
lines(tiempo.test, valoresPredichos, col="green")

## ------------------------------------------------------------------------
boxPierce.test = Box.test(modelo$residuals)
boxPierce.test

## ------------------------------------------------------------------------
jarqueBera.test = jarque.bera.test(modelo$residuals)
jarqueBera.test

shapiroWilk.test = shapiro.test(modelo$residuals)
shapiroWilk.test

## ------------------------------------------------------------------------
hist(modelo$residuals, col="blue", prob=TRUE, ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo$residuals))

## ------------------------------------------------------------------------
# Sumamos estacionalidad
# De nuevo aprovecho el reciclaje de R
valoresReconstruidos.Est = valoresReconstruidos + estacionalidad
valoresPredichos.Est = valoresPredichos + estacionalidad

# Sumamos tendencia
valoresReconstruidos.EstTend = valoresReconstruidos.Est + tendencia.train.H1
valoresPredichos.EstTend = valoresPredichos.Est + tendencia.test.H1

# Deshacemos transformación logarítmica
valoresReconstruidos.EstTendExp = exp(valoresReconstruidos.EstTend)
valoresPredichos.EstTendExp = exp(valoresPredichos.EstTend)

## ------------------------------------------------------------------------
plot.ts(serie)
lines(tiempo.train, valoresReconstruidos.EstTendExp, col="blue")
lines(tiempo.test, valoresPredichos.EstTendExp, col="green")

## ------------------------------------------------------------------------
serie.train = serie.log
tiempo.train = 1:length(serie.log)
tiempo.test = (length(tiempo.train)+1):(length(tiempo.train)+nPred)

## ------------------------------------------------------------------------
parametros.H1 = lm(serie.train ~ tiempo.train)

tendencia.train.H1 = parametros.H1$coefficients[1]+tiempo.train*parametros.H1$coefficients[2]
tendencia.test.H1 = parametros.H1$coefficients[1]+tiempo.test*parametros.H1$coefficients[2]

## ------------------------------------------------------------------------
serie.train.sinTend = serie.train - tendencia.train.H1
plot.ts(serie.train.sinTend, xlim=c(1, tiempo.train[length(tiempo.train)]))

## ------------------------------------------------------------------------
serie.train.sinTendSinEst = serie.train.sinTend - estacionalidad
plot.ts(serie.train.sinTendSinEst, xlim=c(1, tiempo.train[length(tiempo.train)]))

## ------------------------------------------------------------------------
modelo = arima(serie.train.sinTendSinEst, order = c(4,1,0))

## ------------------------------------------------------------------------
predicciones = predict(modelo, n.ahead = nPred)
valoresPredichos = predicciones$pred

plot.ts(serie.train.sinTendSinEst, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, valoresPredichos, col="red")

## ------------------------------------------------------------------------
# Sumamos estacionalidad
# De nuevo aprovecho el reciclaje de R
valoresPredichos.Est = valoresPredichos + estacionalidad

# Sumamos tendencia
valoresPredichos.EstTend = valoresPredichos.Est + tendencia.test.H1

# Deshacemos transformación logarítmica
valoresPredichos.EstTendExp = exp(valoresPredichos.EstTend)

## ------------------------------------------------------------------------
plot.ts(serie, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, valoresPredichos.EstTendExp, col="red")

## ------------------------------------------------------------------------
serie.1960 = scan("Datos/pasajeros_1960.predict")

plot.ts(serie, xlim=c(1, tiempo.test[length(tiempo.test)]))
lines(tiempo.test, serie.1960, col="blue")
lines(tiempo.test, valoresPredichos.EstTendExp, col="red")

