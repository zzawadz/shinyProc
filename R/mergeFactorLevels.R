createGroups = function(x, cutoff = 0.1)
{
  vals = x %>% unique() %>% sort
  ux = expand.grid(vals, vals, stringsAsFactors = FALSE)
  
  dlDistance = stringdist(tolower(ux$Var1), tolower(ux$Var2))

  maxLength = apply(as.matrix(ux), 1, function(x) max(nchar(as.character(x))))
  
  scaledDistance = 1 - dlDistance / maxLength
  
  dstMat = matrix(scaledDistance, ncol = sqrt(length(dlDistance)))
  
  rownames(dstMat) = vals
  colnames(dstMat) = vals  

  dstMat = as.dist(1 - dstMat)
  
  fit = hclust(dstMat)
  
  tree = cutree(fit, h = cutoff)
  groups = lapply(sort(tree) %>% unique, function(i) sort(names(tree[tree == i])))
  names(groups) = sapply(groups, "[[", 1)
  groups
}

mergeFactorUI = function(id) {
  ns = NS(id)
  
  tagList(
    sliderInput(ns("cutOff"), label = "Cutoff level:", min = 0, max = 1, value = 0.1, step = 0.01),
    actionButton(ns("addTable"), label = "Add group", width = "100%"),
    tags$br(),
    tags$br(),
    uiOutput(ns("levelsTables"))
    )
}

makeMergeFactor = function(input, output, session, x, defaultGroups = NULL)
{
  vars = reactiveValues(groups = defaultGroups)
  init = reactiveValues(init = FALSE)
  
  allNames = reactive({
    x %>% unique %>% sort
  })
  
  grouped = eventReactive(input$cutOff, {
    
    flog.debug("[MERGE LEVELS] Recalculate groups - start.")
    if(!is.null(defaultGroups) && !isolate(init$init))
    {
      flog.debug("[MERGE LEVELS] Init default groups.")
      isolate({init$init = TRUE})
      return()
    }
    
    isolate({init$init = TRUE})
    
    grp = createGroups(x, cutoff = input$cutOff)
    flog.debug("[MERGE LEVELS] Recalculate groups - end.")
    
    return(grp)
  })
  
  observeEvent(grouped(),{
    vars$groups = grouped()
  })
  
  output$levelsTables = renderUI({
    
    req(vars$groups)
    req(allNames())
    
    ns = session$ns
    
    flog.debug("[MERGE LEVELS] Update tables - start")
    
    iter = icount()
    
    allNames()
    
    names = vars$groups %>% unlist()
    diffNames = setdiff(allNames(), names)
    
    tabs = lapply(vars$groups, function(x)
    {
      selectizeInput(ns(sprintf("var%s",nextElem(iter))), label = NULL, choices = c(x, diffNames), selected = x, multiple = TRUE, options = list(plugins = list('remove_button',"drag_drop")))
    })
    
    flog.debug("[MERGE LEVELS] Update tables - end")
    return(tabs)
    
  })
  
  observeEvent(input$addTable,
               {
                 flog.debug("[MERGE LEVELS] Add group - start.")
                 vars$groups = c("a",vars$groups)
                 vars$groups[[1]] = character()
                 flog.debug("[MERGE LEVELS] Add group - end.")
               })
  
  observe({
    
    flog.debug("[MERGE LEVELS] Update groups observer - start.")
    print(input$addTable)
    req(init$init)
    
    isolate(req(vars$groups))
    allGroups = isolate(vars$groups)
    
    
    if(is.null(sapply(seq_along(allGroups), function(i)
    {
      input[[sprintf("var%s", i)]]
    }) %>% unlist))
    {
      flog.debug("[MERGE LEVELS] Update groups observer - all nulls - end.")
      return()
    }
    
    lapply(seq_along(allGroups), function(i)
    {
      levels = input[[sprintf("var%s", i)]]
      print(levels)
      
      if(is.null(levels)) levels = character()
      
      isolate({ vars$groups[[i]] = levels })
    })
    
    flog.debug("[MERGE LEVELS] Update groups observer - end.")
    
  })
  
}





