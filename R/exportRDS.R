


exportRDSUI = function(id, defFileName = "", mainButtonTitle = "Export to R file", doneButtonTitle = "Done!")
{
  ns = NS(id)
  
  tagList(
    actionButton(ns("runModal"), mainButtonTitle),
    bsModal(ns("exportToRModal"), "Export to R file", ns("runModal"), size = "small",
            textInput(ns("fileName"), label = "File name:", defFileName),
            downloadButton(ns("exportToRFileButton"), label = doneButtonTitle))
  )
}

makeExportRDS = function(input, output, session, object)
{
  output$exportToRFileButton = downloadHandler(
    filename = function() {
      paste0(input$fileName,".rds")
    },
    content = function(file) {
      if(is.null(object())) return(NULL)
      
      src = paste0(tempfile(),".rds")
      saveRDS(object(), file = src)
      
      #toggleModal(session, "exportToRModal", toggle = "hide")
      file.rename(src, file)
    })
}

