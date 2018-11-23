library(readxl)
xl<-read.csv(file.choose(),sep=";")

library(forecast)
val2 <- xl$Receita
val2<-ts(val2,frequency=12,start=c(2016,1))

library(TTR)

fitted3<- HoltWinters(val2,gamma=FALSE)
fitted3
fitted3$SSE
plot(fitted3)

fore<-forecast(fitted3,h=12,level=95)
plot(fore,main = "Previsão de Receita de Diárias",sub="Intervalo de 95% de confiança",ylab="Receita")

