library(readxl)
xl<-read.csv(file.choose(),sep=";",dec=",")

library(forecast)
val2 <- log(xl$Receita)
val2<-ts(val2,frequency=12,start=c(2016,1))

decompose(val2)

library(TTR)

fitted3<- HoltWinters(val2)
fitted3
fitted3$SSE
plot(fitted3)

fore<-forecast(fitted3,h=12,level=95)
plot(fore,main = "Previsão de Receita de Diárias",sub="Intervalo de 95% de confiança",ylab="Receita")
val2

ar<-auto.arima(diff(val2),trace=TRUE)
plot(diff(val2))
plot(val2)
acf(diff(val2),lag.max = 20)
far<-forecast(ar,h=12,level=95)
plot(far)
