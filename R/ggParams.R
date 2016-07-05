

ggThemeParamsUI = function(id) {
  ns = NS(id)
  
  tagList(
    fluidRow(
    selectInput(ns("legendPosition"), "Legend position:",
                      choices = c("none", "left", "right", "bottom", "top"),
                      selected = "bottom"),
          column(6,sliderInput(ns("fontSizePlot"), label = "Font size:", min = 0, max = 36, value = 4, step = 1)),
          column(6, sliderInput(ns("fontSizeSmall"), label = "Font size small:", min = 9, max = 36, value = 16, step = 1)),
           column(6, sliderInput(ns("fontSizeLarge"), label = "Font size large:", min = 9, max = 36, value = 24, step = 1)),
            column(6, sliderInput(ns("xaxisTextAngle"), label = "X-axis text angle",value = 45, min = 45, max = 90, step = 1)),
            column(6, numericInput(ns("leftMargin"), min = 4.5, label = "Left margin:", value = 5.5, step = 0.1)),
            column(6, sliderInput(ns("pointSize"), label = "Point size",value = 2, min = 0.1, max = 5, step = 0.1))
          
  ))
}

ggMakeThemeParams = function(input, output, session)
{
  themes = reactiveValues(axis.text.x = theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)),
                          axis.text = theme(axis.text = element_text(colour = "darkblue", face = "bold", size = 16)),
                          axis.title  = theme(axis.title.y = element_text(colour = "darkblue", face = "bold", size = 24)),
                          legend.position = theme(legend.position = "bottom"),
                          plot.margin = theme(plot.margin = margin(5.5,5.5,5.5, 5.5))
                          )
  
  call = reactiveValues(call = function(p) { return(p) }, textFontSize = 4, pointSize = 2)
  
   
  # text angle
  observe({

    f = function(x) x*(-0.5/45) + 1.5
    val = f(as.numeric(input$xaxisTextAngle))
    themes$axis.text.x = theme(axis.text.x = element_text(angle = input$xaxisTextAngle, 
                                                            vjust = val, 
                                                            hjust=1))
  })
  
  # font size
  observe({
    themes$axis.text  = theme(axis.text = element_text(colour = "darkblue", 
                                                       face = "bold", 
                                                       size = input$fontSizeSmall))
    
    themes$axis.title = axis.title  = theme(axis.title = element_text(colour = "darkblue", 
                                                                        face = "bold", 
                                                                        size = input$fontSizeLarge))
  })
  
  # legend position
  observe({
    themes$legend.position = theme(legend.position = input$legendPosition)
  })
  
  # left margin
  observe({
    themes$plot.margin = theme(plot.margin = margin(5.5,5.5,5.5, input$leftMargin)) 
  })

  observe({
    thm = reactiveValuesToList(themes) 
    
    fnc = function(p)
    {
      for(t in thm) p = p + t
      return(p)
    }
    
    call$call = fnc
    
  })
  
  observe({
    call$textFontSize = input$fontSizePlot
  })
  
  observe({
    call$pointSize = input$pointSize
  })
  
  
  
  
  
  return(call)
}

