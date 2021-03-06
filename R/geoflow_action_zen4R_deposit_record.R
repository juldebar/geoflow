zen4R_deposit_record <- function(entity, config, options){
  
  if(!require("zen4R")){
    stop("This action requires the 'zen4R' package")
  }
  
  ZENODO <- config$software$zenodo
  
  if(is.null(ZENODO)){
    errMsg <- "This action requires the Zenodo API to be declared in the configuration"
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #options
  depositWithFiles <- if(!is.null(options$depositWithFiles)) options$depositWithFiles else FALSE
  publish <- if(!is.null(options$publish) & depositWithFiles) options$publish else FALSE
  deleteOldFiles <- if(!is.null(options$deleteOldFiles)) options$deleteOldFiles else TRUE
  communities <- if(!is.null(options$communities)) options$communities else NULL
  
  #create empty record
  #how to deal with existing records / new versions
  #this approach that the Zenodo record has a related identifier as URN
  #e.g. urn:my-metadata-identifier
  zenodo_metadata <- NULL
  deposits <- ZENODO$getDepositions()
  if(length(deposits)>0){
    invisible(lapply(deposits, function(deposit){
     related_identifiers <- deposit$metadata$related_identifiers
     for(related_identifier in related_identifiers){
       if(startsWith(related_identifier$identifier,"urn")){
         related_id <- unlist(strsplit(related_identifier$identifier, "urn:"))[2]
         if(related_id == entity$identifiers[["id"]] &
            related_identifier$relation == "isIdenticalTo"){
           zenodo_metadata <<- deposit
           break
         }
       }
     }
    }))
  }
  
  if(is.null(zenodo_metadata)){
    zenodo_metadata <- ZENODO$createEmptyRecord()
    zenodo_metadata$addRelatedIdentifier("isIdenticalTo", paste("urn", entity$identifiers[["id"]], sep=":"))
  }
  
  #if entity already comes with a DOI, we set it (this might be a preset DOI from Zenodo or elsewhere)
  if(!is.null(entity$identifiers[["doi"]])){
    zenodo_metadata$setDOI(entity$identifiers[["doi"]])
  }
  
  #basic record description
  zenodo_metadata$setTitle(entity$title)
  zenodo_metadata$setDescription(entity$descriptions[["abstract"]])
  
  #upload type
  #TODO think on how to map upload types between Dublin core, ISO/OGC metadata, Zenodo  
  zenodo_metadata$setUploadType("dataset")
  
  #contacts
  #TODO think if correct handle all contacts (whatever roles) as creators (author/co-authors)
  zenodo_metadata$metadata$creators <- list()
  for(contact in entity$contacts){
    contact_names <- unlist(strsplit(contact$individualName, " "))
    
    #manage orcid?
    orcid <- NULL
    contact_ids <- contact$identifiers
    contact_ids <- contact_ids[sapply(contact_ids, function(x){x$key=="orcid"})]
    if(length(contact_ids)>0) orcid <- contact_ids[[1]]$value
    
    #add/update creators
    zenodo_metadata$addCreator(
      firstname = contact_names[1], 
      lastname = contact_names[2], 
      affiliation = contact$organizationName,
      orcid = orcid
    )
  }
  
  #TODO myrec$setLicense
  #TODO myrec$setAccessRight
  
  #communities
  if(!is.null(communities)){
    for(community in communities) zenodo_metadata$addCommunity(community)
  }
  
  #file uploads
  if(depositWithFiles){
    if(deleteOldFiles){
      config$logger.info("Zenodo: deleting old files...")
      zen_files <- ZENODO$getFiles(zenodo_metadata$id)
      if(length(zen_files)>0){
        for(zen_file in zen_files){
          ZENODO$deleteFile(zenodo_metadata$id,zen_file$id)
        }
      }
    }
    config$logger.info("Zenodo: uploading files...")
    #upload data files, if any
    data_files <- list.files(file.path(getwd(),"data"))
    if(length(data_files)>0){
      data_files <- data_files[regexpr(entity$identifiers[["id"]],data_files)>0]
      if(length(data_files)>0) data_files <- data_files[!endsWith(data_files, ".rds")]
      if(length(data_files)>0){
        config$logger.info("Zenodo: uploading data files...")
        for(data_file in data_files){
          config$logger.info(sprintf("Zenodo: uploading data file '%s'", data_file))
          ZENODO$uploadFile(file.path(getwd(), "data", data_file), zenodo_metadata$id)
        }
      }
    }
    #upload metadata files, if any
    metadata_files <- list.files(file.path(getwd(),"metadata"))
    if(length(metadata_files)>0){
      metadata_files <- metadata_files[regexpr(entity$identifiers[["id"]],metadata_files)>0]
      if(length(metadata_files)>0) metadata_files <- metadata_files[!endsWith(metadata_files, ".rds")]
      if(length(metadata_files)>0){
        config$logger.info("Zenodo: uploading metadata files...")
        for(metadata_file in metadata_files){
          config$logger.info(sprintf("Zenodo: uploading metadata file '%s'", metadata_file))
          ZENODO$uploadFile(file.path(getwd(), "metadata",metadata_file), zenodo_metadata$id)
        }
      }
    }
  }

  #deposit (and publish, if specified in options)
  if(publish){
    #double verification for publish action, need to have the DOI specified in the entity table
    if(is.null(entity$identifiers[["doi"]])){
      config$logger.warn("No DOI specified in entity. Zenodo 'publish' action ignored!")
      publish <- FALSE
    }
    if(!is.null(entity$identifiers[["doi"]])){
      if(entity$identfiers[["doi"]] != zenodo_metadata$metadata$prereserve_doi$doi){ 
        config$logger.warn(sprintf("DOI specified (%s) in entity doesn't match Zenodo record DOI (%s). Zenodo 'publish' action ignored!", 
                                   entity$identfiers[["doi"]], zenodo_metadata$metadata$prereserve_doi$doi))
        publish <- FALSE
      }
    }
  }
  out <- ZENODO$depositRecord(zenodo_metadata, publish = publish)
  if(!is(out,"ZenodoRecord")){
    errMsg <- sprintf("Zenodo: %s", out$errors[[1]]$message)
    config$logger.error(errMsg)
    stop(errMsg)
  }
  
  #we set the (prereserved) doi to the entity in question
  invisible(lapply(1:length(config$metadata$content$entities), function(i){
    ent <- config$metadata$content$entities[[i]] 
    if(ent$identifiers[["id"]]==entity$identifiers[["id"]]){
      config$metadata$content$entities[[i]]$identifiers[["doi"]] <<- zenodo_metadata$metadata$prereserve_doi$doi
    }
  }))
  
}