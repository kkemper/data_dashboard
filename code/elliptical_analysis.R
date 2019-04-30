library(airtabler)
library(dplyr)
library(ggplot2)

## Load Airtable data into DataFrame and query top rows
Sys.setenv("AIRTABLE_API_KEY"="keypchGI5tV3dEicT")
airtable <- airtabler::airtable("app3gYedYvqJRvElw", "Elliptical Workouts") 
airtable <- airtable$'Elliptical Workouts'$select_all()
head(airtable)

## Line Graph of Various Heart Rates vs. Time
airtable <- airtable %>% mutate(Date = as.Date(Date))
airtable <- na.omit(airtable)
heart_rate <- c(airtable$`Elliptical Heart Rate (bpm)`, airtable$`Runkeeper Heart Rate (bpm)`, airtable$`AppleWatch Heart Rate (bpm)`)
plot(airtable$Date, heart_rate, type="l")

## Calculate mean heart rate
airtable$'Mean Heart Rate (bpm)' <- (as.integer(airtable$`Elliptical Heart Rate (bpm)`) + as.integer(airtable$`Runkeeper Heart Rate (bpm)`) + as.integer(airtable$`AppleWatch Heart Rate (bpm)`))/3
summary(airtable)
head(airtable)
x <- as.integer(c(airtable$`Elliptical Heart Rate (bpm)`, airtable$`Runkeeper Heart Rate (bpm)`, airtable$`AppleWatch Heart Rate (bpm)`, airtable$`Mean Heart Rate (bpm)`))
plot(as.integer(airtable$`Pace (min/mile)`), as.integer(airtable$`Mean Heart Rate (bpm)`))