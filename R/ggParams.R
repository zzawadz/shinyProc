

ggThemeParamsUI = function(id) {
  ns = NS(id)
  
  tagList(selectInput(ns("legendPosition"), "Legend position:",
                      choices = c("none", "left", "right", "bottom", "top"),
                      selected = "bottom"),
          sliderInput(ns("fontSizeSmall"), label = "Font size small:", min = 9, max = 36, value = 16, step = 1),
          sliderInput(ns("fontSizeLarge"), label = "Font size large:", min = 9, max = 36, value = 24, step = 1),
          selectInput(ns("xaxisTextAngle"), label = "X-axis text angle",choices = c("45","90"),selected = "45"),
          numericInput(ns("leftMargin"), min = 4.5, label = "Left margin:", value = 5.5, step = 0.1)
  )
}

ggMakeThemeParams = function(input, output, session)
{
  themes = reactiveValues(axis.text.x = theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)),
                          axis.text = theme(axis.text = element_text(colour = "darkblue", face = "bold", size = 16)),
                          axis.title  = theme(axis.title.y = element_text(colour = "darkblue", face = "bold", size = 24)),
                          legend.position = theme(legend.position = "bottom"),
                          plot.margin = theme(plot.margin = margin(5.5,5.5,5.5, 5.5))
                          )
  
  call = reactiveValues(call = function(p) { return(p) })
  
   
  # text angle
  observe({

    if(input$xaxisTextAngle == "45")
    {
      themes$axis.text.x = theme(axis.text.x = element_text(angle = 45, 
                                                            vjust = 1, 
                                                            hjust=1))

    } else {
      themes$axis.text.x = theme(axis.text.x = element_text(angle = 90, 
                                                            vjust = 0.5, 
                                                            hjust=1))
    }
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
  
  
  return(call)
}

