filtersUI = function(id) {
  ns = NS(id)
  
  tagList(
    tags$style(type='text/css', "#levels { height: 200px; }"),
    uiOutput(ns("columnToFilterUI")),
    uiOutput(ns("levelsUI")),
    actionButton(ns("makeFilter"), "Filter!"),
    actionButton(ns("RemoveLast"), "Remove last filter"),
    dataTableOutput(ns("filtersTable")))
}


makeFilters <- function(input, output, session, data, columnsToFilter)
{
  filters = reactiveValues(filters = data_frame(col = character(0), levels = character(0)))
  
  output$columnToFilterUI = renderUI({
    ns = session$ns
    selectInput(ns("columnToFilter"), "Column to filter", choices = columnsToFilter, selected = columnsToFilter[1])
  })
  
  output$levelsUI = renderUI({
    if(is.null(input$columnToFilter)) return(NULL)
    if(is.null(data)) return()
    
    ns = session$ns
    col = input$columnToFilter
    
    levels = levels(as.factor(data[,col][[1]]))
    selectInput(ns("levels"), "Select:", levels, selected = levels, multiple = TRUE, selectize = FALSE)
  })
  
  observeEvent(input$makeFilter,
  {
    if(is.null(data)) return()
    
    dt = data_frame(col = input$columnToFilter, levels = paste(sprintf('"%s"',input$levels), collapse = ", "))
    filters$filters = rbind(filters$filters, dt)
  })

  observeEvent(input$RemoveLast,{ 
    if(is.null(data)) return()
    filters$filters = head(filters$filters, -1)
  })
  
  output$filtersTable = renderDataTable({
    filters$filters
  })
  
  return(filters)
}
