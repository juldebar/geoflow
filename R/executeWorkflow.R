#' @name executeWorkflow
#' @aliases executeWorkflow
#' @title executeWorkflow
#' @description \code{executeWorkflow} allows to execute a workflow
#'
#' @usage executeWorkflow(file)
#'                 
#' @param file a JSON geoflow configuration file
#' 
#' @author Emmanuel Blondel, \email{emmanuel.blondel1@@gmail.com}
#' @export
#' 
executeWorkflow <- function(file){
  
  #options
  .defaultOptions <- options()
  options(stringsAsFactors = FALSE)
  
  #1. Init the workflow based on configuration file
  CFG <- initWorkflow(file)
  
  #2. Inits workflow job (create directories)
  jobDir <- initWorkflowJob(CFG)
  
  #3. Execute the workflow job
  exec <- try(executeWorkflowJob(CFG))
  if(class(exec)=="try-error"){
    setwd(CFG$wd)
  }
  
  #4. close workflow
  closeWorkflow(CFG)
  
  options(.defaultOptions)
  
}