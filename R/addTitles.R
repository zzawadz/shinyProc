

plotTitlesUI = function(id) {
  ns = NS(id)
  
  bsModal(ns("titleModal"), "Titles", ns("trigger"), size = "small",
          textInput(ns("xlab"), "X label", ""),
          textInput(ns("ylab"), "Y label", ""),
          textInput(ns("title"), "Plot title", "")
          )
}

makePlotTitles = function(input, output, session)
{
  vals = reactiveValues(call = function(p, xlab = "", ylab = "", title = "")
  {
    p + xlab(xlab) + ylab(ylab) + ggtitle(title)
  },
  modalId = NULL)
  
  observe({
    flog.debug("[TITLE] Open modal")
    ns = session$ns
    vals$modalId = ns("titleModal")
  })

 observe({
   
   if(is.null(input$xlab)) xlabNew = ""   else xlabNew = input$xlab
   if(is.null(input$ylab)) ylabNew = ""   else ylabNew = input$ylab
   if(is.null(input$title)) titleNew = "" else titleNew = input$title
   
   fnc = function(p, xlab = "", ylab = "", title = "")
   {
     xlab  = ifelse(xlabNew  != "", xlabNew,  xlab)
     ylab  = ifelse(ylabNew  != "", ylabNew,  ylab)
     title = ifelse(titleNew != "", titleNew, title)
     
     p + xlab(xlab) + ylab(ylab) + ggtitle(title)
   }
   
   vals$call = fnc
   
 })
  
  return(vals)
}

