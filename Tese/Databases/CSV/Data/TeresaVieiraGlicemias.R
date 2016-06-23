setwd("~/Tese/Tese/Databases/CSV/Data")

joaquim <- read.csv("TeresaVieira.csv")

#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")


joaquim$DateTime <- NULL

for(i in 1:nrow(joaquim)){
  if(strptime(joaquim$Period[i], "%H:%M:%S") >= strptime("06:00:00", "%H:%M:%S") & strptime(joaquim$Period[i], "%H:%M:%S") < strptime("12:00:00", "%H:%M:%S"))
  {
    joaquim$Period[i] = 1
  } else if(strptime(joaquim$Period[i], "%H:%M:%S") >= strptime("12:00:00", "%H:%M:%S") & strptime(joaquim$Period[i], "%H:%M:%S") < strptime("20:00:00", "%H:%M:%S")){
    joaquim$Period[i] = 2
  } else{
    joaquim$Period[i] = 3
  }
}



################################################
###############################################
setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("TeresaVieira.csv")



#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

for(i in 1:nrow(joaquim)){
  if(joaquim$Value_Glucose[i]==0 || is.na(joaquim$Value_Glucose[i])) {
    joaquim$Value_Glucose[i] <- NA
  }
}
joaquim$DateTime <- NULL

#Gráficos por hora
joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,300), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")




setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("TeresaVieira.csv")



#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL

#Gráficos por hora
joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,300), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")


par(mfrow=c(3,3)) 
#por hora e dia da semana
joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period[joaquim$Day=="Domingo"], joaquim$Value_Glucose[joaquim$Day=="Domingo"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Domingo")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Segunda"], joaquim$Value_Glucose[joaquim$Day=="Segunda"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Segunda")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Terça"], joaquim$Value_Glucose[joaquim$Day=="Terça"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Terça")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Quarta"], joaquim$Value_Glucose[joaquim$Day=="Quarta"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Quarta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Quinta"], joaquim$Value_Glucose[joaquim$Day=="Quinta"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Quinta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Sexta"], joaquim$Value_Glucose[joaquim$Day=="Sexta"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Sexta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

plot(joaquim$Period[joaquim$Day=="Sábado"], joaquim$Value_Glucose[joaquim$Day=="Sábado"], xaxt="n", ylim = c(0,300), ylab="Valor de glicose", xlab="Sábado")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")

par(mfrow=c(2,2))
