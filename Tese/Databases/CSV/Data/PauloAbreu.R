##################################################################################
#Regras apenas com glicemia
setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("PauloAbreu.csv")



#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL


for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Value_Insulin[i]) || is.na(joaquim$Value_Carbs[i])){
    joaquim$Calculated_Insulin[i] <- NA
  } else
    if(i==0){
      joaquim$Calculated_Insulin = 0;
    } else {
      joaquim$Calculated_Insulin[i] = joaquim$Value_Carbs[i]/10 + ((joaquim$Value_Glucose[i] - joaquim$Target_BG[i])/45)
      joaquim$Calculated_Insulin[i] = round(joaquim$Calculated_Insulin[i] / 0.5)*0.5
    }
}

for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Calculated_Insulin[i])) {
    joaquim$Diferença_Insulin[i] <- NA
  } else {
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
}

joaquim$Calculated_Insulin <- NULL


for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Value_Glucose[i])){
    joaquim$Value_Glucose[i] <- NA
  } else
    
    if(joaquim$Value_Glucose[i]<70){
      
      joaquim$Value_Glucose[i]=1
      
    } else
      if(joaquim$Value_Glucose[i] >=70 & joaquim$Value_Glucose[i]<90){
        joaquim$Value_Glucose[i]=2
        
      } else if(joaquim$Value_Glucose[i] >= 90 & joaquim$Value_Glucose[i]<150){
        joaquim$Value_Glucose[i]=3
        
      } else if(joaquim$Value_Glucose[i] >=150 & joaquim$Value_Glucose[i]<180){
        joaquim$Value_Glucose[i]=4
        
      } else if(joaquim$Value_Glucose[i]>=180){
        joaquim$Value_Glucose[i]=5
        
      }
}



for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Next_Glucose[i])){
    joaquim$Next_Glucose[i] <- NA
  } else
    
    if(joaquim$Next_Glucose[i]<70){
      
      joaquim$Next_Glucose[i]=1
      
    } else
      if(joaquim$Next_Glucose[i] >=70 & joaquim$Next_Glucose[i]<90){
        joaquim$Next_Glucose[i]=2
        
      } else if(joaquim$Next_Glucose[i] >= 90 & joaquim$Next_Glucose[i]<150){
        joaquim$Next_Glucose[i]=3
        
      } else if(joaquim$Next_Glucose[i] >=150 & joaquim$Next_Glucose[i]<180){
        joaquim$Next_Glucose[i]=4
        
      } else if(joaquim$Next_Glucose[i]>=180){
        joaquim$Next_Glucose[i]=5
        
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


for(i in 1:nrow(joaquim)) {
  
  if(is.na(joaquim$Value_Insulin[i])){
    joaquim$Value_Insulin[i] <- NA
    
  } else if(joaquim$Value_Insulin[i] < 3) {
    joaquim$Value_Insulin[i] = 1
  } else if(joaquim$Value_Insulin[i] >= 3 && joaquim$Value_Insulin[i]<5){
    joaquim$Value_Insulin[i] = 2
  } else if(joaquim$Value_Insulin[i] >=5 && joaquim$Value_Insulin[i] < 7) {
    joaquim$Value_Insulin[i] = 3
  } else if(joaquim$Value_Insulin[i] >=7 && joaquim$Value_Insulin[i] < 8) {
    joaquim$Value_Insulin[i] = 4
  } else {
    joaquim$Value_Insulin = 5
    
  }
}


for (i in 1:nrow(joaquim)){
  if(is.na(joaquim$Value_Carbs[i])) {
    joaquim$Value_Carbs[i] <- NA
  } else 
    if(joaquim$Value_Carbs[i] < 15) {
      joaquim$Value_Carbs[i] = 1
    } 
  else if(joaquim$Value_Carbs[i] >= 15 && joaquim$Value_Carbs[i] < 25) {
    joaquim$Value_Carbs[i] = 2
  } 
  else if(joaquim$Value_Carbs >= 25 && joaquim$Value_Carbs[i] < 35) {
    joaquim$Value_Carbs[i] = 3
  }
  else if(joaquim$Value_Carbs >= 35 && joaquim$Value_Carbs[i] < 55) {
    joaquim$Value_Carbs[i] = 4
  } else {
    joaquim$Value_Carbs[i] = 5
  }
  
}


joaquim$Target_BG <- NULL

joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Carbs <- as.factor(joaquim$Value_Glucose)
joaquim$Value_Insulin <- as.factor(joaquim$Value_Glucose)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)
joaquim$Exercise <- as.factor(joaquim$Exercise)
joaquim$Next_Glucose <- as.factor(joaquim$Next_Glucose)
joaquim$Diferença_Insulin <- as.factor(joaquim$Diferença_Insulin)
joaquim$Value_Glucose <- NULL
library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.01))

rules.sub <- subset(rules, subset = rhs %in% "Next_Glucose=5")