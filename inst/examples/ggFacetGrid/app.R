library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   sidebarLayout(
      sidebarPanel(
        ggFacetGridUI("facet")
      ),
      
   mainPanel(
      plotOutput("plot")
   )
   )
))

server <- shinyServer(function(input, output) {
  
  facetGrid = callModule(ggMakeFacetGrid, "facet", columns = c("vs","am","gear","carb"))
   
  output$plot <- renderPlot({
    data("mtcars")
    
    p = ggplot(mtcars, aes(mpg, disp)) + geom_point()
    
    
    if(!is.null(facetGrid$call))
      p = p + facet_grid(facetGrid$call)
    
    return(p)
  })
})

shinyApp(ui = ui, server = server)

