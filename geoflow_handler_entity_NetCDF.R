#' handle_entities_netcdf
#' @export
#  => check mapping https://docs.google.com/spreadsheets/d/1qwvJPKb4YBl_G8PRScyOfOaXNnyMlf33mTFN3yslTRQ/edit#gid=978299607

handle_entities_netcdf <- function(config, source){
  
  if(!require("ncdf4")){
    stop("This action requires the 'knitr' package")
  }
  
  Thredds_catalog <- get_catalog(source)
  sub_catalogs <- Thredds_catalog$get_catalogs()
  All_datasets_for_metadata <- Thredds_catalog$get_datasets()

  number_datasets<-length(All_datasets_for_metadata)
  # logger.info("START THE WORKFLOW WITH THE MAIN LOOP : ITERATE ON EACH LINE OF THE METADATA TABLE => CREATE ONE METADATA PER SHEET")
  entities <- list()
  for (i in 1:number_datasets ) {
    dataset <- All_datasets_for_metadata[[i]]
    
    # logger.info("SELECT SUR LE TYPE MIME ".nc" ou ".ncml"")
    if(grepl(".ncml",dataset$url)){
      
      opendap_url <- gsub("catalog", "dodsC", dataset$url)
      config$logger.info(paste0("Read ",opendap_url))
      ncin <- nc_open(opendap_url)
      source_entity <- ncin
      
      # config$logger.error(PRINT THE HEADER TO SEE THE METADATA)
      entity <- geoflow_entity$new()
      
      #identifier
      # identifiers <- entity$setIdentifier("id", source_entity$filename)
      identifiers <- entity$setIdentifier("id", paste0("toto_",i))
        
      if(ncatt_get(source_entity,0,"title")$value!=0){entity$setTitle(ncatt_get(source_entity,0,"title")$value)}
      if(ncatt_get(source_entity,0,"summary")$value!=0){entity$setDescription("abstract", ncatt_get(source_entity,0,"summary")$value)}
      # if(ncatt_get(source_entity,0,"comment")$value!=0){entity$setDescription("abstract", ncatt_get(source_entity,0,"summary")$value)}
      
      # dates
      if(ncatt_get(source_entity,0,"date_created")$value!=0){entity$setDate(ncatt_get(source_entity,0,"date_created")$value)}
      #   if(ncatt_get(source_entity,0,"date_issued")$value!=0){entity$setDate(ncatt_get(source_entity,0,"date_issued")$value)}
      #   if(ncatt_get(source_entity,0,"date_modified")$value!=0){entity$setDate(ncatt_get(source_entity,0,"date_modified")$value)}
      
      #temporal coverage
      if(ncatt_get(source_entity,0,"time_coverage_start")$value!=0){start=str_to_posix(ncatt_get(source_entity,0,"time_coverage_start")$value)}
      if(ncatt_get(source_entity,0,"time_coverage_end")$value!=0){end=str_to_posix(ncatt_get(source_entity,0,"time_coverage_end")$value)}
      entity$setTemporalExtent(paste0(start,"/",end))
      
      #spatial coverage
      if(ncatt_get(source_entity,0,"geospatial_lon_min")$value!=0){xmin=as.numeric(ncatt_get(source_entity,0,"geospatial_lon_min")$value)}
      if(ncatt_get(source_entity,0,"geospatial_lat_min")$value!=0){ymin=as.numeric(ncatt_get(source_entity,0,"geospatial_lat_min")$value)}
      if(ncatt_get(source_entity,0,"geospatial_lon_max")$value!=0){xmax=as.numeric(ncatt_get(source_entity,0,"geospatial_lon_max")$value)}
      if(ncatt_get(source_entity,0,"geospatial_lat_max")$value!=0){ymax=as.numeric(ncatt_get(source_entity,0,"geospatial_lat_max")$value)}
      wkt_bouding_box=paste0("POLYGON((",xmin," ",ymin,",",xmin," ",ymax,",",xmax," ",ymax,",",xmax," ",ymin,",",xmin," ",ymin,"))")
      entity$setSrid("4326")
      entity$setSpatialExtent(wkt_bouding_box, crs = 4326)
      
      #subjects
      if(ncatt_get(source_entity,0,"keywords")$value!=0){
        list_of_keywords <- gsub("=", ":", ncatt_get(source_entity,0,"keywords")$value)
        subjects <- unlist(strsplit(sanitize_str(list_of_keywords), ";"))
        }
      subjects
      invisible(lapply(subjects, function(subject){
        subject_obj <- geoflow_subject$new(str = subject)
        entity$addSubject(subject_obj)
        }))
      
      #contacts
  #     contacts <- unlist(strsplit(sanitize_str(source_entity[,"Creator"]), ";"))
  #     invisible(lapply(contacts, function(contact){
  #       contact_splits <- unlist(strsplit(contact, ":"))
  #       contact_obj <- geoflow_contact$new()
  #       contact_obj$setId(contact_splits[2])
  #       contact_obj$setRole(contact_splits[1])
  #       entity$addContact(contact_obj)
  #     }))
      
      #provenance
      # if(ncatt_get(source_entity,0,"history")$value!=0){entity$setProvenance(ncatt_get(source_entity,0,"history")$value)}
      
      entities <- c(entities, entity)
    }
  }
  return(entities)
}


# RECURSIVE FUNCTION TO BROWSE A THREDDS CATALOG AND RELATED SUB-CATALOGS
handle_entities_thredds_catalogs <- function(config,source){
  
  entities <- list()
  Thredds_catalog <- get_catalog(source)
  sub_catalogs <- Thredds_catalog$get_catalogs()
  All_datasets_for_metadata <- Thredds_catalog$get_datasets()
  
  if (!is.null(All_datasets_for_metadata)==TRUE){
    # handle_entities_netcdf(config,source)
    entity <- handle_entities_netcdf(config,source)
    entities <- c(entities, entity)
    print("\n write metadata \n")
  }
  if (!is.null(sub_catalogs)==TRUE){
    number_catalogs<-length(sub_catalogs)
    for (i in 1:number_catalogs ) {
      sub_catalog <- sub_catalogs[[i]]
      entity <- handle_entities_thredds_catalogs(config,sub_catalog$url)
      entities <- c(entities, entity)
    }
  } else {
    print("\n no more sub catalogs\n")
  }
  
  return(entities)
}