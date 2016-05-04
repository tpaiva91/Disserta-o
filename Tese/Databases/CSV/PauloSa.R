setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("PauloSa.csv")



joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL



for(i in 1:nrow(joaquim)){
  if(joaquim$Day[i]==joaquim$Day[i+1] & joaquim$Value_Glucose[i] > joaquim$Value_Glucose[i+1]){
    joaquim$Variation[i+1]=1
  } else if(joaquim$Day[i]==joaquim$Day[i+1] & joaquim$Value_Glucose[i] < joaquim$Value_Glucose[i+1]) {
    joaquim$Variation[i+1]= 2
  }
}



for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Glucose[i]<70){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >70 & joaquim$Value_Glucose[i]<80){
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
  
  if(joaquim$Value_Insulin[i]<3.0){
    
    joaquim$Value_Insulin[i]=1
  } else
    if(joaquim$Value_Insulin[i] >=3.0 & joaquim$Value_Insulin[i]<5.0){
      joaquim$Value_Insulin[i]=2
    } else if(joaquim$Value_Insulin[i] >= 5.0 & joaquim$Value_Insulin[i]<7.0){
      joaquim$Value_Insulin[i]=3
    } else if(joaquim$Value_Insulin[i] >=7.0 & joaquim$Value_Insulin[i]<10.0){
      joaquim$Value_Insulin[i]=4
    } else if(joaquim$Value_Insulin[i]>=10.0){
      joaquim$Value_Insulin[i]=5
    }
}

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Carbs[i]<20){
    joaquim$Value_Carbs[i]=1
  } else
    if(joaquim$Value_Carbs[i] >=20 & joaquim$Value_Carbs[i]<40){
      joaquim$Value_Carbs[i]=2
    } else if(joaquim$Value_Carbs[i] >=40 & joaquim$Value_Carbs[i]<70){
      joaquim$Value_Carbs[i]=3
    } else if(joaquim$Value_Carbs[i] >=70 & joaquim$Value_Carbs[i]<100){
      joaquim$Value_Carbs[i]=4
    } else if(joaquim$Value_Carbs[i]>=100){
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






library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.01))

rules.sub <- subset(rules, subset = rhs %in% "Hyperglycemia=1")
write(rules.sub, file="RulesPauloSa")


joaquim$Exercise <- NULL
joaquim$Disease <- NULL
joaquim$Exercise = "No";
hist(as.numeric(as.character(joaquimClean$Value)))

library(arules)
rules <- apriori(joaquim)
inspect(rules)



for(i in 1:nrow(joaquim)){
  if(joaquim$Day[i]=="Segunda"){
    joaquim$Day[i]=1;
  } else if(joaquim$Day[i]=="Terça"){
    joaquim$Day[i]=2;
    
  } else if(joaquim$Day[i]=="Quarta"){
    joaquim$Day[i]=3;
  } else if(joaquim$Day[i]=="Quinta"){
    joaquim$Day[i]=4;
  } else if(joaquim$Day[i]=="Sexta"){
    joaquim$Day[i]=5;
  } else if(joaquim$Day[i]=="Sábado"){
    joaquim$Day[i]=6;
  } else{
    joaquim$Day[i]=7;
  }
}


joaquim <- read.csv("PauloDaily.csv")
joaquim$Day <- weekdays(as.Date(joaquim$Day))

library(arules)
rules <- apriori(paulo)


for(i in 1:nrow(joaquim)){
  
  if(joaquim$Max_Value[i]<50){
    
    joaquim$Max_Value[i]=1
    
  } else
    if(joaquim$Max_Value[i] >=50 & joaquim$Max_Value[i]<70){
      joaquim$Max_Value[i]=2
      
    } else if(joaquim$Max_Value[i] >= 70 & joaquim$Max_Value[i]<130){
      joaquim$Max_Value[i]=3
      
    } else if(joaquim$Max_Value[i] >=130 & joaquim$Max_Value[i]<170){
      joaquim$Max_Value[i]=4
      
    } else if(joaquim$Max_Value[i]>=170){
      joaquim$Max_Value[i]=5
      
    }
}

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Min_Value[i]<50){
    
    joaquim$Min_Value[i]=1
    
  } else
    if(joaquim$Min_Value[i] >=50 & joaquim$Min_Value[i]<70){
      joaquim$Min_Value[i]=2
      
    } else if(joaquim$Min_Value[i] >= 70 & joaquim$Min_Value[i]<130){
      joaquim$Min_Value[i]=3
      
    } else if(joaquim$Min_Value[i] >=130 & joaquim$Min_Value[i]<170){
      joaquim$Min_Value[i]=4
      
    } else if(joaquim$Min_Value[i]>=170){
      joaquim$Min_Value[i]=5
      
    }
}

joaquim$Had_Exercise <- as.factor(joaquim$Had_Exercise)
joaquim$Day <- as.factor(joaquim$Day)
joaquim$Max_Value <- as.factor(joaquim$Max_Value)
joaquim$Min_Value <- as.factor(joaquim$Min_Value)
joaquim$Had_ExerciseDayBefore <- as.factor(joaquim$Had_ExerciseDayBefore)
joaquim$Hyperglycemia <- as.factor(joaquim$Hyperglycemia)
joaquim$Hypoglycemia <- as.factor(joaquim$Hypoglycemia)

rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.03))

rules.sub <- subset(rules, subset = rhs %in% "Hyperglycemia=1")


####
#Linhas para a tarde
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50,230))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 230))
                                                                                                                  

####
#Linhas para a manhã
plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: manhã", col="red", ylim=c(50, 230))

####
#Linhas para a noite
plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: noite", col="green", ylim=c(50, 230))


####
#Hiperglicemias
plot(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(180,240))
lines(joaquim$Value_Glucose[joaquim$Period==2], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(180, 240))

plot(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: tarde", col="red", ylim=c(180,240))
lines(joaquim$Value_Glucose[joaquim$Period==1], ylab="Valor de glicose", xlab="Período: tarde", col="red", ylim=c(50, 230))

plot(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: tarde", col="green", ylim=c(180,240))
lines(joaquim$Value_Glucose[joaquim$Period==3], ylab="Valor de glicose", xlab="Período: tarde", col="blue", ylim=c(50, 230))


#Gráficos por hora
joaquim$Period <- as.POSIXct(joaquim$Period, format="%H:%M")
plot(joaquim$Period, joaquim$Value_Glucose, xaxt="n", ylim=c(0,250), ylab="Valor de glicose", xlab="Hora")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")


#por hora e dia da semana

plot(joaquim$Period[joaquim$Day=="Domingo"], joaquim$Value_Glucose[joaquim$Day=="Domingo"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Domingo")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Segunda"], joaquim$Value_Glucose[joaquim$Day=="Segunda"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Segunda")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Terça"], joaquim$Value_Glucose[joaquim$Day=="Terça"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Terça")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Quarta"], joaquim$Value_Glucose[joaquim$Day=="Quarta"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Quarta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Quinta"], joaquim$Value_Glucose[joaquim$Day=="Quinta"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Quinta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Sexta"], joaquim$Value_Glucose[joaquim$Day=="Sexta"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Sexta")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
abline(h=180, col="blue")

plot(joaquim$Period[joaquim$Day=="Sábado"], joaquim$Value_Glucose[joaquim$Day=="Sábado"], xaxt="n", ylim = c(0,250), ylab="Valor de glicose", xlab="Sábado")
axis.POSIXct(1, joaquim$Period, seq(from=as.POSIXct("2016-02-02 0:00"), to=as.POSIXct("2016-05-02 23:00"), by="hour"), format="%H:%M")
abline(h=70, col="red")
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
