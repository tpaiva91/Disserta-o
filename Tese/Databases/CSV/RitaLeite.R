setwd("~/Tese/Tese/Databases/CSV/Data")



joaquim <- read.csv("RitaLeite.csv")





joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL


for(i in 1:nrow(joaquim)){
  
  joaquim$Calculated_Insulin[i] = joaquim$Value_Carbs[i]/10 + ((joaquim$Value_Glucose[i] - 120)/40)
  
}

for(i in 1:nrow(joaquim)){
  if(joaquim$Day[i]==joaquim$Day[i+1] & joaquim$Value_Glucose[i] > joaquim$Value_Glucose[i+1]){
    joaquim$Variation[i+1]=1
  } else if(joaquim$Day[i]==joaquim$Day[i+1] & joaquim$Value_Glucose[i] < joaquim$Value_Glucose[i+1]) {
    joaquim$Variation[i+1]= 2
  }
}




for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Glucose[i]<50){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >=50 & joaquim$Value_Glucose[i]<70){
      joaquim$Value_Glucose[i]=2
      
    } else if(joaquim$Value_Glucose[i] >= 70 & joaquim$Value_Glucose[i]<140){
      joaquim$Value_Glucose[i]=3
      
    } else if(joaquim$Value_Glucose[i] >=140 & joaquim$Value_Glucose[i]<190){
      joaquim$Value_Glucose[i]=4
      
    } else if(joaquim$Value_Glucose[i]>=190){
      joaquim$Value_Glucose[i]=5
      
    }
}


for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Insulin[i]<2){
    
    joaquim$Value_Insulin[i]=1
  } else
    if(joaquim$Value_Insulin[i] >=2.0 & joaquim$Value_Insulin[i]<3.0){
      joaquim$Value_Insulin[i]=2
    } else if(joaquim$Value_Insulin[i] >= 3.0 & joaquim$Value_Insulin[i]<4.0){
      joaquim$Value_Insulin[i]=3
    } else if(joaquim$Value_Insulin[i] >=4.0 & joaquim$Value_Insulin[i]<5.5){
      joaquim$Value_Insulin[i]=4
    } else if(joaquim$Value_Insulin[i]>=5.5){
      joaquim$Value_Insulin[i]=5
    }
}

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Carbs[i]<25){
    joaquim$Value_Carbs[i]=1
  } else
    if(joaquim$Value_Carbs[i] >=25 & joaquim$Value_Carbs[i]<30){
      joaquim$Value_Carbs[i]=2
    } else if(joaquim$Value_Carbs[i] >=30 & joaquim$Value_Carbs[i]<35){
      joaquim$Value_Carbs[i]=3
    } else if(joaquim$Value_Carbs[i] >=35 & joaquim$Value_Carbs[i]<40){
      joaquim$Value_Carbs[i]=4
    } else if(joaquim$Value_Carbs[i]>=40){
      joaquim$Value_Carbs[i]=5
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





for(i in 1:nrow(joaquim)){
  if(joaquim$Exercise[i]!=0){
    joaquim$Had_Exercise[i]=1
  }
}

for(i in 1:nrow(joaquim)){
  if(((joaquim$Day[i] == joaquim$Day[i+1]) & joaquim$Exercise[i]!=0) || (joaquim$Had_Exercise[i]==1 & joaquim$Day[i]==joaquim$Day[i+1])){
    joaquim$Had_Exercise[i+1]=1
  }
}




joaquim$Had_Exercise <- as.factor(joaquim$Had_Exercise)
joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)
joaquim$Value_Carbs <- as.factor(joaquim$Value_Carbs)
joaquim$Value_Insulin <- as.factor(joaquim$Value_Insulin)
joaquim$Exercise <- as.factor(joaquim$Exercise)
joaquim$Variation <- as.factor(joaquim$Variation)

joaquim$Variation <- NULL
joaquim$Exercise <- NULL
joaquim$Had_Exercise <- NULL
joaquim$Calculated_Insulin <- NULL




rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.05))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")
write(rules.sub, file="RulesRitaLeite")

joaquim$Calculated_Insulin <- NULL
joaquim$Had_Exercise <- NULL
joaquim$Exercise <- NULL
joaquim$Disease <- NULL
joaquim$Exercise = "No";
hist(as.numeric(as.character(joaquimClean$Value)))

library(arules)
rules <- apriori(joaquim)
inspect(rules)


####
#Linhas para a tarde
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue")
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue")

####
#Linhas para a manhã
plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red")
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red")

####
#Linhas para a noite
plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green")
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green")


####
#Hiperglicemias
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(180,240))

plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(180,240))

plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(180,240))

#Gráficos por hora
joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,220), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-13 0:00"), to=as.POSIXct("2017-02-20 23:00"), by="hour"), format="%H:%M")
abline(h=50, col="red")
abline(h=190, col="blue")

#média de glicose por dia
mean(joaquim$Value_Glucose[joaquim$Day=="Domingo"])


#todos os periodos no mesmo gráfico
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Manhã, tarde e noite", col="blue", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))
points(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))
points(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: tarde", col="red", ylim=c(50,230))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 230))











##################################################################################
#Regras apenas com glicemia
setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("RitaLeite.csv")



joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Glucose[i]<50){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >=50 & joaquim$Value_Glucose[i]<70){
      joaquim$Value_Glucose[i]=2
      
    } else if(joaquim$Value_Glucose[i] >= 70 & joaquim$Value_Glucose[i]<140){
      joaquim$Value_Glucose[i]=3
      
    } else if(joaquim$Value_Glucose[i] >=140 & joaquim$Value_Glucose[i]<190){
      joaquim$Value_Glucose[i]=4
      
    } else if(joaquim$Value_Glucose[i]>=190){
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
joaquim$Had_Exercise <- NULL


joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)

library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.05))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")
