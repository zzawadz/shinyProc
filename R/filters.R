filtersUI = function(id) {
  ns = NS(id)
  
  tagList(
    tags$style(type='text/css', "#levels { height: 200px; }"),
    uiOutput(ns("slidersUI")),
    uiOutput(ns("columnToFilterUI")),
    uiOutput(ns("levelsUI")),
    actionButton(ns("makeFilter"), "Filter!"),
    actionButton(ns("RemoveLast"), "Remove last filter"),
    dataTableOutput(ns("filtersTable")))
}


makeFilters <- function(input, output, session, data, columnsToFilter, sliders = NULL, defaultFilters = function() NULL)
{
  filters = reactiveValues(filteredData = data, filters = NULL, sliders = list())
  
  status = reactiveValues(slidersCreated = FALSE)
  
  observeEvent(defaultFilters(),
  {
    filters$filters = defaultFilters()
  })
  
  ######## Sliders
  
  output$slidersUI = renderUI({
    
    flog.trace("Entering slidersUI.")
    
    if(is.null(sliders)) return(NULL)
    ns = session$ns
    
    iter = counter()
    slNameIt = iterator(names(sliders))
    
    slAll = lapply(sliders, function(sl)
    {
      sliderId = paste0("slider", iter())
      columnName = slNameIt()
      
      if(is.null(sl$range))
      {
        range = range(data[[columnName]], na.rm = TRUE)
      } else
      {
        range = sl$range
      }
      
      if(is.null(sl$value))
      {
        value = range
      } else
      {
        value = sl$value
      }
      
      isolate({ filters$sliders[[sliderId]] = list(columnName = columnName,
                                           value = sl$value,
                                           range = range)
              })
      
      
      sliderInput(ns(sliderId), 
                  label = sl$label, 
                  min = range[[1]], 
                  max = range[[2]], 
                  value = value, 
                  step = sl$step)
      

    })
    
    flog.trace("Exiting slidersUI.")
    status$slidersCreated = TRUE
    
    return(slAll)
  })
  
  observe({
    
    flog.trace("Entering sliders value update.")
    if(!status$slidersCreated) return()
    
    slidersIds = isolate(names(filters$sliders))
    
    lapply(slidersIds, function(id)
    {
      value = input[[id]]
      isolate({ filters$sliders[[id]]$value = value })
    })
    flog.trace("Exiting sliders value update.")
  })
  
  ######## Factors filters
  
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
    if(is.null(filters$filters)) return()
    filters$filters = head(filters$filters, -1)
  })
  
  output$filtersTable = renderDataTable({
    if(is.null(data)) return(data_frame(col = character(0), levels = character(0)))
    filters$filters
  })
  
  observe({
            dt = filterDataByFactors(data, filters$filters)
            dt = filterDataBySliders(dt, filters$sliders)
            
            filters$filteredData = dt
            
            
          })
  
  return(filters)
}


#'
#'
#' @examples 
#' 
#' filters = structure(list(col = c("Species", "Species"), levels = c("\"setosa\", \"versicolor\", \"virginica\"", 
#' "\"versicolor\", \"virginica\"")), .Names = c("col", "levels"
#' ), row.names = 1:2, class = c("tbl_df", "tbl", "data.frame"))
#' 
filters2yaml = function(filters)
{
  as.yaml(filters) %>% cat
}

filterDataByFactors = function(data, filters)
{
  
  if(!is.null(filters) && nrow(filters) > 0)
  {
    flts = sapply(1:nrow(filters), function(i)
    {
      col = filters[i,1][[1]]
      lvl = filters[i,2][[1]]
      sprintf('filter(%s %%in%% c(%s))', col, lvl)
    })
    
    flts = paste0(flts, collapse = " %>% ")
    code = paste("data %>% ", flts)
    code = parse(text = code)
    data = eval(code)
  }
  
  data
}

filterDataBySliders = function(data, sliders)
{
  if(length(sliders) == 0) return(data)
  flog.trace("Entering filterDataBySliders.")
  expr = NULL
  
  for(sl in sliders)
  {
    if(is.null(sl$value)) next;
    if(all(sl$value == sl$range)) next;
    
    tmpCode = sprintf("%s >= %s, %s <= %s", sl$columnName, sl$value[1], sl$columnName, sl$value[2])
    expr = paste(expr, tmpCode,  sep  = ",")
  }
  
  if(is.null(expr)) return(data)
  
  expr = substring(expr, 2)
  
  code = paste("data %>% ", sprintf("filter(%s)",expr))
  code = parse(text = code)
  data = eval(code)
  
  flog.trace("Exiting filterDataBySliders.")
  data
}
