simplePlotUI = function(id) {
  ns = NS(id)
  
  tagList(
    fluidRow(
      column(6, uiOutput(ns("XUI"))),
      column(6, uiOutput(ns("FillUI")))
    ),
    uiOutput(ns("YUI")))
}

makeSimplePlotUI <- function(input, output, session, xcols = nullFnc,
                             ycols = nullFnc,
                             fillCols = nullFnc,
                             initVals = emptyListFnc)
{
  ### Create XUI
  output$XUI = renderUI({
    if(is.null(xcols())) return(NULL)
    ns = session$ns
    selectInput(ns("X"), "X axis", xcols(), selected = initVals()$x)
  })
  
  output$YUI = renderUI({
    if(is.null(ycols())) return(NULL)
    ns = session$ns
    selectInput(ns("Y"), "Y axis", ycols(), selected = initVals()$y)
  })
  
  output$FillUI = renderUI({
    if(is.null(fillCols())) return(NULL)
    ns = session$ns
    
    init = initVals()$fill
    if(is.null(init)) init = "None"
    
    selectInput(ns("fill"), "Fill", choices = c("None", fillCols()), selected = init)
  })
  
  return(reactive({
    flog.trace("[SIMPLE PLOT UI] X: %s, Y: %s, Fill: %s", input$X, input$Y, input$fill)
    res = list(x = input$X, y = input$Y, fill = input$fill)
  }))
  
  
}


