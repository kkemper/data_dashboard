  library(airtabler)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(plyr)
  library(viridis)
  
  ## Load Airtable data into DataFrame and query top rows
  Sys.setenv("AIRTABLE_API_KEY"="keypchGI5tV3dEicT")
  airtable <- airtabler::airtable("app3gYedYvqJRvElw", "Elliptical Workouts") 
  airtable <- airtable$'Elliptical Workouts'$select_all()
  airtable <- na.omit(airtable)
  
  # Set  Variables
  level <- airtable$Level
  time <- airtable$`Time (Minutes)`
  calories <- airtable$Calories
  miles <- airtable$`Distance (Miles)`
  distanceClimbed <- airtable$`Distance Climbed`
  speed <- airtable$`Speed(mph)`
  pace <- airtable$`Pace (min/mile)`
  ehr <- airtable$`Elliptical Heart Rate (bpm)`
  rhr <- airtable$`Runkeeper Heart Rate (bpm)`
  awhr <- airtable$`AppleWatch Heart Rate (bpm)`
  
 # Convert columns into numerals
  level <- as.integer(level)
  time<- as.integer(time)
  calories <- as.integer(calories)
  miles <- as.numeric(miles)
  distanceClimbed <- as.integer(distanceClimbed)
  speed <- as.numeric(speed)
  pace <- ms(pace)
  pace <- as.numeric(pace)
  ehr<- as.integer(ehr)
  rhr<- as.integer(rhr)
  awhr<- as.integer(awhr)
  
  ## Calculate mean heart rate
  airtable$'Mean Heart Rate (bpm)' <- (ehr + rhr + awhr)/3
  mhr <- airtable$`Mean Heart Rate (bpm)`
  
  ## Convert Date Column
  airtable <- airtable %>% mutate(Date = as.Date(date))
  date <- airtable$Date
  
  ## Bin Calories
  calorieLab <- c("100-200", "200-300", "300-400", "400-500", "500-600", "600-700", "700-800", "800-900")
  calorieBins <- cut(calories, pretty(calories), labels=calorieLab)
  countCB <- count(calorieBins)
  calorieBP <- ggplot(calorieBins, aes(x="", y=countCB, fill=calorieBins))+
    geom_bar(width = 1, stat = "calorieBins")
  calorieBP
  
  ## Bin Distance Climbed
  climbedLab <- c("2000-4000", "4000-6000", "6000-8000", "8000-10000")
  climbedBins <- cut(distanceClimbed, pretty(distanceClimbed), labels=climbedLab)
  count(climbedBins)
  plot(count(climbedBins), type="pie")
  
  ## Bin Speed
  speedLab <- c("5-5.5", "5.5-6", "6-6.5", "6.5-7", "7-7.5")
  speedBins <- cut(speed, pretty(speed), labels=speedLab)
  count(speedBins)
  plot(count(speedBins), type="pie")
  
  ## Bin Pace
  paceLab <- c("400-450", "450-500", "500-550", "550-600", "600-650", "650-700")
  paceBins <- cut(pace, pretty(pace), labels=paceLab)
  count(paceBins)
  plot(count(paceBins), type="pie")
  
  ## Bin Mean Heart Rate
  mhrLab <- c("120-125", "125-130", "130-135", "135-140", "140-145", "145-150", "150-155", "155-160")
  mhrBins <- cut(mhr, pretty(mhr), labels=mhrLab)
  count(mhrBins)
  plot(count(mhrBins), type="p")
  
  ## Bin Distance (Miles)
  milesLab <- c("1-2", "3-4", "4-5", "5-6", "6-7")
  milesBins <- cut(miles, pretty(miles))
  count(milesBins)
  plot(count(milesBins), type="p")
  
  ## Plot Heart Rates over time and against mean
  heart_rate <- c(ehr, rhr, awhr)
  plot(mhr, type="l", lwd = 2, xaxt='n', ylim=c(110, 160), col="purple", xlab="Date", ylab="Heart Rate (bpm)", main="Heart Rate Readings by Device")
  axis(1,at=1:length(date),labels=date)
  lines(ehr, col="red", type="l", lwd = 2)
  lines(rhr, col="blue", type="l", lwd = 2)
  lines(awhr, col="orange", type="l", lwd = 2)
  legend("bottomright", legend=c("Elliptical", "Wahoo Tickr", "AppleWatch", "Average"), lty=1, lwd=2, pch=21, col=c("orange", "red", "blue", "purple"), ncol=2, bty="n", cex=0.8, text.col=c("orange", "red", "blue", "purple"), inset=0.01)
  
  ## Plot heart rate against pace
 ggplot(airtable, aes(x = pace, y = mhr)) + stat_density_2d(geom = "tile", aes(fill = ..density..), contour = FALSE) + scale_fill_viridis()
  
  ## Calculate instantaneous standard deviation.
  airtable$'standard deviation' <- sd(heart_rate)
  airtable$`standard deviation`
  
  ## Boxplots
  boxplot(mhr~level, xlab="Level", ylab="Mean Heart Rate (bpm)", main="Level")
  boxplot(mhr~time, xlab="Time", ylab="Mean Heart Rate (bpm)", main="Time")
  boxplot(mhr~calorieBins, xlab="Calories", ylab="Mean Heart Rate (bpm)", main="Calories")
  boxplot(mhr~milesBins, xlab="Distance (Miles)", ylab="Mean Heart Rate (bpm)", main="Distance")
  boxplot(mhr~climbedBins, xlab="Distance Climbed", ylab="Mean Heart Rate (bpm)", main="Distance Climbed")
  boxplot(mhr~speedBins, xlab="Speed (mph)", ylab="Mean Heart Rate (bpm)", main="Speed")
  boxplot(mhr~paceBins, xlab="Pace (min/mile)", ylab="Mean Heart Rate (bpm)", main="Pace")
  
  ## Pearson Correlation with Mean Heart Rate
  cor(mhr, level, method = "pearson")
  cor(mhr, time, method = "pearson")
  cor(mhr, calories, method = "pearson")
  cor(mhr, miles, method = "pearson")
  cor(mhr, distanceClimbed, method = "pearson")
  cor(mhr, speed, method = "pearson")
  cor(mhr, pace, method = "pearson")
  
  ## Scatter Plots
  pairs(~ level + time + calories + speed + pace + ehr + rhr + awhr + mhr)
  
  ## ANOVA
  anova <- aov(mhr ~ level * time * calories * miles * distanceClimbed * speed * pace)
  summary(anova)