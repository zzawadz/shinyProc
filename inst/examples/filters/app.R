

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Filters app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         filtersUI("filters")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  callModule(makeFilters,"filters", data = iris, columnsToFilter = "Species")
})

# Run the application 
shinyApp(ui = ui, server = server)

