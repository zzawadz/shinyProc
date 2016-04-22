library(shiny)
library(dplyr)
library(shinyAce)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Filters app"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        actionButton("loadFilters", "Load filters"),
        filtersUI("filters")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  loadedFilters = eventReactive(input$loadFilters,
  {
    structure(list(col = c("Species", "Species"), levels = c("\"setosa\", \"versicolor\", \"virginica\"", 
                 "\"versicolor\", \"virginica\"")), .Names = c("col", "levels"
                  ), row.names = 1:2, class = c("tbl_df", "tbl", "data.frame"))
  })
  
  filters = callModule(makeFilters,"filters", 
                       data = iris, 
                       columnsToFilter = "Species", 
                       defaultFilters = loadedFilters)
  
  observe({ print(filters$filters) })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

