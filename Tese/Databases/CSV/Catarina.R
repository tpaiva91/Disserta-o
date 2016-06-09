setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("CatarinaVale.csv")





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
  
  if(joaquim$Value_Glucose[i]<60){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >=60 & joaquim$Value_Glucose[i]<90){
      joaquim$Value_Glucose[i]=2
      
    } else if(joaquim$Value_Glucose[i] >= 90 & joaquim$Value_Glucose[i]<150){
      joaquim$Value_Glucose[i]=3
      
    } else if(joaquim$Value_Glucose[i] >=150 & joaquim$Value_Glucose[i]<200){
      joaquim$Value_Glucose[i]=4
      
    } else if(joaquim$Value_Glucose[i]>=200){
      joaquim$Value_Glucose[i]=5
      
    }
}


for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Insulin[i]<3.0){
    
    joaquim$Value_Insulin[i]=1
  } else
    if(joaquim$Value_Insulin[i] >=3.0 & joaquim$Value_Insulin[i]<5.0){
      joaquim$Value_Insulin[i]=2
    } else if(joaquim$Value_Insulin[i] >= 5.0 & joaquim$Value_Insulin[i]<10.0){
      joaquim$Value_Insulin[i]=3
    } else if(joaquim$Value_Insulin[i] >=10.0 & joaquim$Value_Insulin[i]<15.0){
      joaquim$Value_Insulin[i]=4
    } else if(joaquim$Value_Insulin[i]>=15.0){
      joaquim$Value_Insulin[i]=5
    }
}

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Carbs[i]<30){
    joaquim$Value_Carbs[i]=1
  } else
    if(joaquim$Value_Carbs[i] >=30 & joaquim$Value_Carbs[i]<40){
      joaquim$Value_Carbs[i]=2
    } else if(joaquim$Value_Carbs[i] >=40 & joaquim$Value_Carbs[i]<60){
      joaquim$Value_Carbs[i]=3
    } else if(joaquim$Value_Carbs[i] >=60 & joaquim$Value_Carbs[i]<80){
      joaquim$Value_Carbs[i]=4
    } else if(joaquim$Value_Carbs[i]>=80){
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




joaquim$Had_Exercise <- NULL
joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)
joaquim$Value_Carbs <- as.factor(joaquim$Value_Carbs)
joaquim$Value_Insulin <- as.factor(joaquim$Value_Insulin)
joaquim$Exercise <- NULL
joaquim$Variation <- as.factor(joaquim$Variation)


joaquim$Calculated_Insulin <- NULL
joaquim$Had_Exercise <- NULL
joaquim$Exercise <- NULL




rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.03))
rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")
rules.sub <- subset(rules, subset = rhs %in% "Hyperglycemia=1")

write(rules.sub, file="RulesCatarina")

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






##################################################################################
#Regras apenas com glicemia
setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("CatarinaVale.csv")



joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL

for(i in 1:nrow(joaquim)){
  
  if(joaquim$Value_Glucose[i]<60){
    
    joaquim$Value_Glucose[i]=1
    
  } else
    if(joaquim$Value_Glucose[i] >=60 & joaquim$Value_Glucose[i]<90){
      joaquim$Value_Glucose[i]=2
      
    } else if(joaquim$Value_Glucose[i] >= 90 & joaquim$Value_Glucose[i]<150){
      joaquim$Value_Glucose[i]=3
      
    } else if(joaquim$Value_Glucose[i] >=150 & joaquim$Value_Glucose[i]<200){
      joaquim$Value_Glucose[i]=4
      
    } else if(joaquim$Value_Glucose[i]>=200){
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
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.01))

rules.sub <- subset(rules, subset = rhs %in% "Value_Glucose=5")