fillZeroBars = function(dt, X, fill, call = NULL)
{
  dt = as_data_frame(dt)
  
  if(!is.null(call))
  {
    gridNames = as.character(call)
    gridNames = gridNames[!(gridNames %in% c("~","."))]
  } else
  {
    gridNames = character()
  }
  
  
  if(length(gridNames) == 0)
  {
    dtx = dt %>% group_by_(X, fill) %>% summarise(count = n())
    dtx2 = expand.grid(unique(dtx[[1]]), unique(dtx[[2]]))
    
  } else if(length(gridNames) == 1)
  {
    dtx = dt %>% group_by_(X, fill, gridNames[[1]]) %>% summarise(count = n())
    dtx2 = expand.grid(unique(dtx[[1]]), unique(dtx[[2]]), unique(dtx[[3]]))
    
  } else if(length(gridNames) == 2)
  {
    dtx = dt %>% group_by_(X, fill, gridNames[[1]], gridNames[[2]]) %>% summarise(count = n())
    dtx2 = expand.grid(unique(dtx[[1]]), unique(dtx[[2]]), unique(dtx[[3]]), unique(dtx[[4]]))
    
  }
  
  colnames(dtx2) = head(colnames(dtx),-1)
  
  
  dtx = suppressWarnings(full_join(dtx, dtx2))
  dtx$count[is.na(dtx$count)] = 0
  return(dtx)
}

ggFacetGridUI = function(id) {
  ns = NS(id)
  
  tagList(fluidRow(
    
    column(6, uiOutput(ns("gridRowUI"))),
    
    column(6, uiOutput(ns("gridColUI"))
    )))
}

ggMakeFacetGrid = function(input, output, session, columns)
{
  values = reactiveValues(gridCol = "None", gridRow = "None", call = function(p) return(p), fillZeroBars = fillZeroBars )
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
      values$fillZeroBars = function(dt, X, fill) fillZeroBars(dt, X, fill, call = call)
      
      
      flog.trace(sprintf("[FacetGrid] Exiting observer. Values updated. Row: %s, Col: %s", input$gridRow, input$gridCol))
      
    }
  )
  
  return(values)
}
