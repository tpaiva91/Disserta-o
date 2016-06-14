setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("VitorFerreira.csv")

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

joaquim$Value_Carbs <- NULL
joaquim$Value_Insulin <- NULL
joaquim$Target_BG <- NULL
joaquim$Exercise <- NULL
####
#Linhas para a tarde
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50,340))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 340))


####
#Linhas para a manhã
plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 340))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 340))

####
#Linhas para a noite
plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 340))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 340))


####
#Hiperglicemias
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(180,340))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(180, 340))

plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: tarde", col="red", ylim=c(180,340))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: tarde", col="red", ylim=c(50, 340))

plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: tarde", col="green", ylim=c(180,340))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 340))

###################################################################################
###################################################################################
#Gráficos por hora

joaquim <- read.csv("VitorFerreira.csv")

#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL

joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,350), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")


#por hora e dia da semana
plot(joaquim$Period[joaquim$Day=="Domingo"], joaquim$Value_Glucose[joaquim$Day=="Domingo"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Domingo")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Segunda"], joaquim$Value_Glucose[joaquim$Day=="Segunda"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Segunda")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Terça"], joaquim$Value_Glucose[joaquim$Day=="Terça"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Terça")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Quarta"], joaquim$Value_Glucose[joaquim$Day=="Quarta"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Quarta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Quinta"], joaquim$Value_Glucose[joaquim$Day=="Quinta"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Quinta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Sexta"], joaquim$Value_Glucose[joaquim$Day=="Sexta"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Sexta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Sábado"], joaquim$Value_Glucose[joaquim$Day=="Sábado"], xaxt="n", ylim = c(0,350), ylab="Valor de glicose", xlab="Sábado")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-05 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=180, col="blue")


#média de glicose por dia
mean(joaquim$Value_Glucose[joaquim$Day=="Domingo"])


#todos os periodos no mesmo gráfico
plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Manhã, tarde e noite", col="red", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))
points(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))
points(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50,230))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 230))


##################################################################################
##################################################################################
#Regras apenas com glicemia
setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("VictorFerreiraGlicemias.csv")



joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Glucose[i]<60){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >60 & joaquim$Value_Glucose[i]<80){
      joaquim$Value_Glucose[i]=2
      
    } else if(joaquim$Value_Glucose[i] >= 80 & joaquim$Value_Glucose[i]<130){
      joaquim$Value_Glucose[i]=3
      
    } else if(joaquim$Value_Glucose[i] >=130 & joaquim$Value_Glucose[i]<180){
      joaquim$Value_Glucose[i]=4
      
    } else if(joaquim$Value_Glucose[i]>=180){
      joaquim$Value_Glucose[i]=5
      
    }
}

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

joaquim$Value_Carbs <- NULL
joaquim$Value_Insulin <- NULL
joaquim$Exercise <- NULL
joaquim$Variation <- NULL


joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)

library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.05))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")