setwd("~/Tese/Tese/Databases/CSV/Data")


joaquim <- read.csv("VitorFerreira.csv")



#joaquim[is.na(joaquim)] <-0
joaquim$Day <- weekdays(as.Date(joaquim$DateTime))
joaquim$Period <- format(as.POSIXlt(joaquim$DateTime), "%H:%M:%S")

joaquim$DateTime <- NULL
joaquim$Calculated_Insulin = 1
joaquim$Exercise <- NULL

for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Value_Insulin[i]) || is.na(joaquim$Value_Carbs[i])) {
    joaquim$Calculated_Insulin <- NA
  } else
  if(i==0){
    joaquim$Calculated_Insulin = 0;
  } else {
    joaquim$Calculated_Insulin[i] = joaquim$Value_Carbs[i]/10 + ((joaquim$Value_Glucose[i] - 120)/40)
    joaquim$Calculated_Insulin[i] = round(joaquim$Calculated_Insulin[i] / 0.5)*0.5
  }
}

for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Calculated_Insulin[i])){
    joaquim$Diferença_Insulin[i] <- NA
  }
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

joaquim$Calculated_Insulin <- NULL




for(i in 1:nrow(joaquim)){
  
  
  
  if(is.na(joaquim$Value_Glucose[i])) {
    joaquim$Value_Glucose[i] <- NA
  } else
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
  
  
  
  if(is.na(joaquim$Next_Glucose[i])) {
    joaquim$Next_Glucose[i] <- NA
  } else
    if(joaquim$Next_Glucose[i]<60){
      
      joaquim$Next_Glucose[i]=1
      
    } else
      if(joaquim$Next_Glucose[i] >60 & joaquim$Next_Glucose[i]<80){
        joaquim$Next_Glucose[i]=2
        
      } else if(joaquim$Next_Glucose[i] >= 80 & joaquim$Next_Glucose[i]<130){
        joaquim$Next_Glucose[i]=3
        
      } else if(joaquim$Next_Glucose[i] >=130 & joaquim$Next_Glucose[i]<180){
        joaquim$Next_Glucose[i]=4
        
      } else if(joaquim$Next_Glucose[i]>=180){
        joaquim$Next_Glucose[i]=5
        
      }
}


for(i in 1:nrow(joaquim)){
  
  if(is.na(joaquim$Value_Insulin[i]) || joaquim$Value_Carbs[i]==0) {
    joaquim$Value_Insulin[i] <- NA
  } else
  if(joaquim$Value_Insulin[i]<3.0 && joaquim$Value_Insulin[i]!=0){
    
    joaquim$Value_Insulin[i]=1
  } else
    if(joaquim$Value_Insulin[i] >=3.0 & joaquim$Value_Insulin[i]<4.3){
      joaquim$Value_Insulin[i]=2
    } else if(joaquim$Value_Insulin[i] >= 4.3 & joaquim$Value_Insulin[i]<10.0){
      joaquim$Value_Insulin[i]=3
    } else if(joaquim$Value_Insulin[i] >=10.0 & joaquim$Value_Insulin[i]<15.0){
      joaquim$Value_Insulin[i]=4
    } else if(joaquim$Value_Insulin[i]>=15.0){
      joaquim$Value_Insulin[i]=5
    }
}

for(i in 1:nrow(joaquim)){
  if(is.na(joaquim$Value_Carbs[i]) || joaquim$Value_Carbs[i]==0) {
    joaquim$Value_Carbs[i] <- NA
  } else
  if(joaquim$Value_Carbs[i]<25){
    joaquim$Value_Carbs[i]=1
  } else
    if(joaquim$Value_Carbs[i] >=25 & joaquim$Value_Carbs[i]<40){
      joaquim$Value_Carbs[i]=2
    } else if(joaquim$Value_Carbs[i] >=40 & joaquim$Value_Carbs[i]<80){
      joaquim$Value_Carbs[i]=3
    } else if(joaquim$Value_Carbs[i] >=80 & joaquim$Value_Carbs[i]<120){
      joaquim$Value_Carbs[i]=4
    } else if(joaquim$Value_Carbs[i]>=120){
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




joaquim$Had_Exercise <- as.factor(joaquim$Had_Exercise)
joaquim$Day <- as.factor(joaquim$Day)
joaquim$Period <- as.factor(joaquim$Period)
joaquim$Value_Glucose <- as.factor(joaquim$Value_Glucose)
joaquim$Value_Carbs <- as.factor(joaquim$Value_Carbs)
joaquim$Value_Insulin <- as.factor(joaquim$Value_Insulin)
joaquim$Exercise <- as.factor(joaquim$Exercise)
joaquim$Variation <- as.factor(joaquim$Variation)
joaquim$Diferença_Insulin <- as.factor(joaquim$Diferença_Insulin)
joaquim$Next_Glucose <- as.factor(joaquim$Next_Glucose)
joaquim$Value_Glucose <- NULL
joaquim$Calculated_Insulin <- NULL
joaquim$Had_Exercise <- NULL
joaquim$Target_BG <- NULL



library(arules)
rules <- apriori(joaquim, parameter=list(confidence=0.6, support=0.01))

rules.sub <- subset(rules, subset = rhs %in% "Next_Glucose=5")