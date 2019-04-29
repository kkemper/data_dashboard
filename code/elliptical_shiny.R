library(shiny)
library(airtabler)
library(dplyr)

Sys.setenv("AIRTABLE_API_KEY"="<Your API key") #example key**************
airtable <- airtabler::airtable("<base key>", "<Tab/sheet name>") 

ui <- fluidPage(
      
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)