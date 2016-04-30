ggFacetGridUI = function(id) {
  ns = NS(id)
  
  tagList(fluidRow(
    
    column(6, uiOutput(ns("gridRowUI"))),
    
    column(6, uiOutput(ns("gridColUI"))
    )))
}

ggMakeFacetGrid = function(input, output, session, columns)
{
  values = reactiveValues(gridCol = "None", gridRow = "None", call = function(p) return(p))
  status = reactiveValues(rowReady = FALSE, colReady = FALSE)
  
  output$gridRowUI = renderUI({
    ns = session$ns
    status$rowReady = TRUE
    ui = radioButtons(ns("gridRow"), "Grid row", c("None", columns))
    return(ui)
  })
  
  output$gridColUI = renderUI({
    ns = session$ns
    status$colReady = TRUE
    ui = radioButtons(ns("gridCol"), "Grid col", c("None", columns))
    return(ui)
  })
  
  observe(
    {
      flog.trace("[FacetGrid] Entering observer.")
      if(is.null(input$gridRow) || 
         is.null(input$gridCol) || 
         !status$rowReady ||
         !status$colReady) 
      {
      
        flog.trace(sprintf("[FacetGrid] Exiting observer. UI is not ready. GR: %s, GC: %s, RR: %s, CR %s",
                           is.null(input$gridRow),
                           is.null(input$gridCol),
                           status$rowReady,
                           status$colReady))
        return()
      }
      
      
      # both are "None" so we don't need to create grid call 
      if(input$gridCol == "None" && input$gridRow == "None")
      {
        flog.trace("[FacetGrid] Exiting observer. Both values is NONE.")
        
        values$call = function(p) return(p)
        return()
      }
      
      col = input$gridCol
      row = input$gridRow
      
      if(col == "None") col = "."
      if(row == "None") row = ""
      
      call = paste(row, "~", col)
      call = as.formula(call)
      
      values$gridCol = col
      values$gridRow = row
      values$call = function(p) { p + facet_grid(call) }
      
      
      
      flog.trace(sprintf("[FacetGrid] Exiting observer. Values updated. Row: %s, Col: %s", input$gridRow, input$gridCol))
      
    }
  )
  
  return(values)
}
