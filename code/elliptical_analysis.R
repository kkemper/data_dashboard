library(airtabler)
library(dplyr)

Sys.setenv("AIRTABLE_API_KEY"="<keypchGI5tV3dEicT")
airtable <- airtabler::airtable("app3gYedYvqJRvElw", "Elliptical Workouts") 

airtable <- airtable$'Elliptical Workouts'$select_all()

airtable

