#'geoflow_subject
#'@export
geoflow_subject <- R6Class("geoflow_subject",
  public = list(
    name = NULL,
    uri = NULL,
    keywords = list(),
    initialize = function(str = NULL){
      if(!is.null(str)){
        subject_kvp <- extract_kvp(str)
        self$setName(subject_kvp$key)
        self$setUri(attr(subject_kvp$key,"uri"))
        invisible(lapply(subject_kvp$values, self$addKeyword))
      }
    },
    
    #setName
    setName = function(name){
      self$name <- name
    },
    
    #setUri
    setUri = function(uri){
      self$uri <- uri
    },
    
    #addKeyword
    addKeyword = function(keyword, uri = NULL){
      if(!is.null(uri)){
        attr(keyword, "uri") <- uri
      }
      if(!any(sapply(self$keywords, function(x){
        kwd_added <- x$name == keyword
        if(!is.null(x$uri) & !is.null(attr(keyword,"uri"))){
          kwd_added <- kwd_added & x$uri == attr(keyword, "uri")
        }
        return(kwd_added) 
      }))){
        uri <- attr(keyword,"uri")
        attr(keyword, "uri") <- NULL
        kwd <- geoflow_keyword$new()
        kwd$setName(keyword)
        kwd$setUri(uri)
        self$keywords <- c(self$keywords, kwd)
      }
    }
  )                                  
)