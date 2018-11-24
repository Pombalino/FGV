xl<-read.csv("C:/Users/Pohlmann/Documents/FGV/5º Semestre/Logística/Hotel/analisar.csv",sep=";",dec=",")

library(forecast)
val2 <- log(xl$Receita)
val2<-ts(val2,frequency=12,start=c(2016,1))

plot(decompose(val2))

library(TTR)

fitted3<- HoltWinters(val2)
fitted3
fitted3$SSE
plot(fitted3,main = "Fit do modelo de Holt-Winter",col.predicted = "red2")

fore<-forecast(fitted3,h=12,level=95)
plot(fore,main = "Previsão de Receita de Diárias",sub="Utilizando ln(Receita)",ylab="Receita",
     showgap = FALSE,shadecols = "tan1",fcol = "sienna4")
val2

sse<-fitted3$SSE
sqrt(sse/(length(val2)-1))

prev<- as.data.frame(fore$mean)

# https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
# https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/HoltWinters