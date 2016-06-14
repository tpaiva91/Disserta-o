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
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,250), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-07-02 23:00"), by="hour"), format="%H:%M")
abline(h=60, col="red")
abline(h=150, col="blue")
