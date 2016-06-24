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
  
  if(fill == "None") fill = NULL
  
  groupArgs = unique(c(X, fill, gridNames))
  
  summaryData = do.call(group_by_, 
                        c(list(dt), lapply(groupArgs, function(x) x))) %>% 
                        summarise(count = n())
  levelsGrid = do.call(expand.grid, 
                       lapply(summaryData %>% select(-count), function(x) unique(x)))
  
  colnames(levelsGrid) = head(colnames(summaryData),-1)
  
  dtx = suppressWarnings(full_join(summaryData, levelsGrid))
  dtx$count[is.na(dtx$count)] = 0
  return(dtx)
}

groupByFacet = function(dt, x, fill, call = NULL)
{
  if(!is.null(call))
  {
    gridNames = as.character(call)
    gridNames = gridNames[!(gridNames %in% c("~","."))]
  } else
  {
    gridNames = character()
  }
  
  if(fill == "None") fill = x
  
  if(length(gridNames) == 0)
  {
    dtx = dt %>% group_by_(x, fill)
    
  } else if(length(gridNames) == 1)
  {
    dtx = dt %>% group_by_(x, fill, gridNames[[1]])
  } else if(length(gridNames) == 2)
  {
    dtx = dt %>% group_by_(x, fill, gridNames[[1]], gridNames[[2]])
  }
  
  return(dtx)
  
}

ggFacetGridUI = function(id) {
  ns = NS(id)
  
  tagList(fluidRow(
    
    column(6, uiOutput(ns("gridRowUI"))),
    
    column(6, uiOutput(ns("gridColUI"))
    )))
}

ggMakeFacetGrid = function(input, output, session, columns, init = emptyListFnc)
{
  values = reactiveValues(gridCol = "None", gridRow = "None", call = function(p) return(p), fillZeroBars = fillZeroBars, groupByFacet = groupByFacet)
  status = reactiveValues(rowReady = FALSE, colReady = FALSE)
  
  output$gridRowUI = renderUI({
    ns = session$ns
    status$rowReady = TRUE
    ui = radioButtons(ns("gridRow"), "Grid row", c("None", columns), selected = init()$row)
    return(ui)
  })
  
  output$gridColUI = renderUI({
    ns = session$ns
    status$colReady = TRUE
    ui = radioButtons(ns("gridCol"), "Grid col", c("None", columns), selected = init()$col)
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
      values$groupByFacet = function(data, x, fill) groupByFacet(data, x, fill, call = call)
      
      flog.trace(sprintf("[FacetGrid] Exiting observer. Values updated. Row: %s, Col: %s", input$gridRow, input$gridCol))
      
    }
  )
  
  return(values)
}
