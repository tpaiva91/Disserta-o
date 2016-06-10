setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("PauloSa.csv")



joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL


for(i in 1:nrow(joaquim)){
  
  if(i==0){
    joaquim$Calculated_Insulin = 0;
  } else {
    joaquim$Calculated_Insulin[i] = joaquim$Value_Carbs[i]/28 + ((joaquim$Value_Glucose[i] - 120)/95)
    joaquim$Calculated_Insulin[i] = round(joaquim$Calculated_Insulin[i] / 0.5)*0.5
  }
}

for(i in 1:nrow(joaquim)){
  joaquim$Diferença_Insulin[i] = joaquim$Value_Insulin[i] - joaquim$Calculated_Insulin[i]
  
  if(joaquim$Diferença_Insulin[i] >=3.0) {
    joaquim$Diferença_Insulin[i] = 5
  } else if(joaquim$Diferença_Insulin[i] >= 0.5 && joaquim$Diferença_Insulin[i] < 3.0) {
    joaquim$Diferença_Insulin[i] = 4
  } else if(joaquim$Diferença_Insulin[i] == 0.0) {
    joaquim$Diferença_Insulin[i] = 3
  } else if(joaquim$Diferença_Insulin[i] <= - 0.5 && joaquim$Diferença_Insulin[i] > -3.0 ) {
    joaquim$Diferença_Insulin[i] = 2
  } else if(joaquim$Diferença_Insulin[i] <= -3.0) {
    joaquim$Diferença_Insulin[i] = 1
  }
}

joaquim$Exercise <- NULL
joaquim$Had_Exercise <- NULL
joaquim$Variation <- NULL
joaquim$Calculated_Insulin <- NULL



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
  
  if(joaquim$Value_Insulin[i]<1.0){
    
    joaquim$Value_Insulin[i]=1
  } else
    if(joaquim$Value_Insulin[i] >=1.0 & joaquim$Value_Insulin[i]<2.5){
      joaquim$Value_Insulin[i]=2
    } else if(joaquim$Value_Insulin[i] >= 2.5 & joaquim$Value_Insulin[i]<3.5){
      joaquim$Value_Insulin[i]=3
    } else if(joaquim$Value_Insulin[i] >=3.5 & joaquim$Value_Insulin[i]<5.0){
      joaquim$Value_Insulin[i]=4
    } else if(joaquim$Value_Insulin[i]>=5.0){
      joaquim$Value_Insulin[i]=5
    }
}

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Carbs[i]<20){
    joaquim$Value_Carbs[i]=1
  } else
    if(joaquim$Value_Carbs[i] >=20 & joaquim$Value_Carbs[i]<60){
      joaquim$Value_Carbs[i]=2
    } else if(joaquim$Value_Carbs[i] >=60 & joaquim$Value_Carbs[i]<100){
      joaquim$Value_Carbs[i]=3
    } else if(joaquim$Value_Carbs[i] >=100 & joaquim$Value_Carbs[i]<140){
      joaquim$Value_Carbs[i]=4
    } else if(joaquim$Value_Carbs[i]>=140){
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
joaquim$Diferença_Insulin <- as.factor(joaquim$Diferença_Insulin)


library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.01))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")
write(rules.sub, file="RulesPauloSa")



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

rules <- apriori(joaquim, parameter=list(confidence=0.7, support=0.03))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")





