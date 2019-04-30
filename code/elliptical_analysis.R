  library(airtabler)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(plyr)
  
  ## Load Airtable data into DataFrame and query top rows
  Sys.setenv("AIRTABLE_API_KEY"="keypchGI5tV3dEicT")
  airtable <- airtabler::airtable("app3gYedYvqJRvElw", "Elliptical Workouts") 
  airtable <- airtable$'Elliptical Workouts'$select_all()
  head(airtable)
  
 # Convert columns into numerals
  airtable$Level <- as.integer(airtable$Level)
  airtable$`Time (Minutes)`<- as.integer(airtable$`Time (Minutes)`)
  airtable$Calories <- as.integer(airtable$Calories)
  airtable$`Distance (Miles)` <- as.integer(airtable$`Distance (Miles)`)
  airtable$`Distance Climbed` <- as.integer(airtable$`Distance Climbed`)
  airtable$`Speed(mph)` <- as.numeric(airtable$`Speed(mph)`)
  airtable$`Pace (min/mile)` <- ms(airtable$`Pace (min/mile)`)
  airtable$`Pace (min/mile)` <- as.numeric(airtable$`Pace (min/mile)`)
  airtable$`Elliptical Heart Rate (bpm)`<- as.integer(airtable$`Elliptical Heart Rate (bpm)`)
  airtable$`Runkeeper Heart Rate (bpm)`<- as.integer(airtable$`Runkeeper Heart Rate (bpm)`)
  airtable$`AppleWatch Heart Rate (bpm)`<- as.integer(airtable$`AppleWatch Heart Rate (bpm)`)
  
  ## Line Graph of Various Heart Rates vs. Time
  airtable <- airtable %>% mutate(Date = as.Date(Date))
  airtable <- na.omit(airtable)
  heart_rate <- c(airtable$`Elliptical Heart Rate (bpm)`, airtable$`Runkeeper Heart Rate (bpm)`, airtable$`AppleWatch Heart Rate (bpm)`)
  plot(airtable$Date, heart_rate, type="l")
  
  #Bin Calories
  calories <- airtable$Calories
  calorieLab <- c("100-200", "200-300", "300-400", "400-500", "500-600", "600-700", "700-800", "800-900")
  calorieBins <- cut(calories, pretty(calories), labels=calorieLab)
  count(calorieBins)
  plot(count(calorieBins), type="pie")
  
  #Bin Distance Climbed
  distanceClimbed <- airtable$`Distance Climbed`
  climbedLab <- c("2000-4000", "4000-6000", "6000-8000", "8000-10000")
  climbedBins <- cut(distanceClimbed, pretty(distanceClimbed), labels=climbedLab)
  count(climbedBins)
  plot(count(climbedBins), type="pie")
  
  #Bin Speed
  speed <- airtable$`Speed(mph)`
  speedLab <- c("5-5.5", "5.5-6", "6-6.5", "6.5-7", "7-7.5")
  speedBins <- cut(speed, pretty(speed), labels=speedLab)
  count(speedBins)
  plot(count(speedBins), type="pie")
  
  # Bin Pace
  pace <- airtable$`Pace (min/mile)`
  paceLab <- c("400-450", "450-500", "500-550", "550-600", "600-650", "650-700")
  paceBins <- cut(pace, pretty(pace), labels=paceLab)
  count(paceBins)
  plot(count(paceBins), type="pie")
  
  
  ## Calculate mean heart rate
  airtable$'Mean Heart Rate (bpm)' <- (as.integer(airtable$`Elliptical Heart Rate (bpm)`) + as.integer(airtable$`Runkeeper Heart Rate (bpm)`) + as.integer(airtable$`AppleWatch Heart Rate (bpm)`))/3
  airtable$`Mean Heart Rate (bpm)` <- as.integer(airtable$`Mean Heart Rate (bpm)`)
  
  # Plot heart rate over time
  x <- as.integer(c(airtable$`Elliptical Heart Rate (bpm)`, airtable$`Runkeeper Heart Rate (bpm)`, airtable$`AppleWatch Heart Rate (bpm)`))
  plot(as.integer(airtable$`Pace (min/mile)`), as.integer(airtable$`Mean Heart Rate (bpm)`))
  
  # Calculate instantaneous standard deviation.
  airtable$'standard deviation' <- sd(x)
  airtable$`standard deviation`
  
  # Boxplots
  boxplot(airtable$`Mean Heart Rate (bpm)`~airtable$Level, xlab="Level", ylab="Mean Heart Rate (bpm)", main="Level")
  boxplot(airtable$`Mean Heart Rate (bpm)`~airtable$`Time (Minutes)`, xlab="Time", ylab="Mean Heart Rate (bpm)", main="Time")
  boxplot(airtable$`Mean Heart Rate (bpm)`~calorieBins, xlab="Calories", ylab="Mean Heart Rate (bpm)", main="Calories")
  boxplot(airtable$`Mean Heart Rate (bpm)`~airtable$`Distance (Miles)`, xlab="Distance (Miles)", ylab="Mean Heart Rate (bpm)", main="Distance")
  boxplot(airtable$`Mean Heart Rate (bpm)`~climbedBins, xlab="Distance Climbed", ylab="Mean Heart Rate (bpm)", main="Distance Climbed")
  boxplot(airtable$`Mean Heart Rate (bpm)`~speedBins, xlab="Speed (mph)", ylab="Mean Heart Rate (bpm)", main="Speed")
  boxplot(airtable$`Mean Heart Rate (bpm)`~paceBins, xlab="Pace (min/mile)", ylab="Mean Heart Rate (bpm)", main="Pace")
  
  # Pearson Correlation with Mean Heart Rate
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$Level, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$`Time (Minutes)`, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, calorieBins, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$`Distance (Miles)`, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$`Distance Climbed`, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$`Speed(mph)`, method = "pearson")
  cor(airtable$`Mean Heart Rate (bpm)`, airtable$`Pace (min/mile)`, method = "pearson")
  
  # ANOVA
  anova - aov(airtable$'Mean Heart Rate (bpm)'~airtable$Level*airtable$`Time (Minutes)`*airtable$Calories*airtable$`Distance (Miles)`*airtable$`Distance Climbed`*airtable$`Speed(mph)`*airtable$`Pace (min/mile)`, data = airtable)
  summary(anova)