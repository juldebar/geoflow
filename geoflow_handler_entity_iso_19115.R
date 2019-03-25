#' handle_entities_netcdf
#' @export
#  => check mapping https://docs.google.com/spreadsheets/d/1qwvJPKb4YBl_G8PRScyOfOaXNnyMlf33mTFN3yslTRQ/edit#gid=978299607
# https://github.com/juldebar/R_Metadata/blob/master/metadata_workflow_google_doc_Dublin_Core/scripts/write_R_metadata_from_OGC.R
# source <- "http://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/xml.metadata.get?uuid=fao-rfb-map-swiofc"
# source <- "/tmp/xml_metadata.xml"
handle_entities_iso_19115 <- function(config, source){
  
  if(!require("geometa")){
    stop("This action requires the 'geometa' package")
  }
  
  config$logger.info("----------------------------------------------------")  
  config$logger.info("Read OGC metadata with geometa package")  
  config$logger.info("----------------------------------------------------")  
  
  OGC_metadata <- source
  xml <- xmlParse(OGC_metadata)
  md <- ISOMetadata$new(xml = xml)
  md
  
  config$logger.info("----------------------------------------------------")  
  config$logger.info("Mapping OGC metadata with R metadata using geometa package")  
  config$logger.info("----------------------------------------------------")  
  
  R_metadata <- NULL
  
  entities <- list()
  
  entity <- geoflow_entity$new()
      
      #identifier
      entity$setIdentifier("id", md$identificationInfo[[1]]$citation$identifier$code)
      entity$setTitle(md$identificationInfo[[1]]$citation$title)
      entity$setDescription("abstract", md$identificationInfo[[1]]$abstract)
      entity$setDate(md$identificationInfo[[1]]$citation$date[[1]]$date)
#       # if(md$language){
#       if(md$identificationInfo[[1]]$language[[1]]$valueDescription){
#         entity$setLanguage(md$identificationInfo[[1]]$language[[1]]$valueDescription)
#       }
#       if(md$hierarchyLevel[[1]]$value){
#         # entity$setType(md$hierarchyLevel[[1]]$value)
#         # Type non géré par geoflow ?
#       }
#       if(md$identificationInfo[[1]]){
#         # entity$addRight()
#       }
      
      config$logger.info("----------------------------------------------------")  
      config$logger.info("Data frame for spatial extent")
      config$logger.info("----------------------------------------------------")  
      #       #spatial coverage
      SRID<-md$referenceSystemInfo[[1]]$referenceSystemIdentifier$code
      xmin <- md$identificationInfo[[1]]$extent[[1]]$geographicElement[[1]]$eastBoundLongitude[1]
      xmax <- md$identificationInfo[[1]]$extent[[1]]$geographicElement[[1]]$westBoundLongitude[1]
      ymin <- md$identificationInfo[[1]]$extent[[1]]$geographicElement[[1]]$southBoundLatitude[1]
      ymax <- md$identificationInfo[[1]]$extent[[1]]$geographicElement[[1]]$northBoundLatitude[1]
      wkt_bouding_box=paste0("POLYGON((",xmin," ",ymin,",",xmin," ",ymax,",",xmax," ",ymax,",",xmax," ",ymin,",",xmin," ",ymin,"))")
      # entity$setSrid(SRID)
      entity$setSrid("4326")
      entity$setSpatialExtent(wkt_bouding_box, crs = 4326)
      
#       spatial_metadata$Spatial_resolution<-NULL
#       if(class(md$spatialRepresentationInfo[[1]])[1]=='ISOVectorSpatialRepresentation'){
#         spatial_metadata$SpatialRepresentationType <- "vector"
#         spatial_metadata$GeometricObjectType=md$spatialRepresentationInfo[[1]]$geometricObjects[[1]]$geometricObjectType$value
#       }
      # spatial_metadata$dynamic_metadata_count_features <-NULL
      # spatial_metadata$geographic_identifier="Mauritius"
      
      
      
      config$logger.info("----------------------------------------------------")  
      config$logger.info("Data frame for temporal extent")  
      config$logger.info("----------------------------------------------------")
      
      for (l in md$identificationInfo[[1]]$extent[[1]]$temporalElement){
      #       if(md$identificationInfo[[1]]$language[[1]]$valueDescription){
      start <- as.POSIXct(md$identificationInfo[[1]]$extent[[1]]$temporalElement[[1]]$extent$beginPosition$value,format='%Y')
      end <- as.POSIXct(md$identificationInfo[[1]]$extent[[1]]$temporalElement[[1]]$extent$endPosition$value,format='%Y')
      entity$setTemporalExtent(paste0(start,"/",end))
      }
      
      config$logger.info("----------------------------------------------------")  
      config$logger.info("Contacts")  
      config$logger.info("----------------------------------------------------")  
      #add contacts
      
      # for(entity_contact in entity$contacts){
      
      the_contact <- md$identificationInfo[[1]]$citation$citedResponsibleParty
      contact_obj <- handle_contacts_19115(config, the_contact)
      entity$addContact(contact_obj)
      
      # }
      
      config$logger.info("----------------------------------------------------")  
      config$logger.info("keywords")  
      config$logger.info("----------------------------------------------------")  
      #subjects
#       for(i in 1:length(md$identificationInfo[[1]]$descriptiveKeywords)){
#         thesaurusName <- md$identificationInfo[[1]]$descriptiveKeywords[[i]]$thesaurusName$title
#         keywords <- md$identificationInfo[[1]]$descriptiveKeywords[[i]]$keyword
#         for (k in keywords){
#           all_keywords[nrow(all_keywords)+1,] <- c(k, thesaurusName)
#         }
#         keywords_metadata$all_keywords <- all_keywords
#       }
#       
#       #subjects
#       for(i in 1:length(md$identificationInfo[[1]]$descriptiveKeywords)){
#         subjects <- md$identificationInfo[[1]]$descriptiveKeywords[[i]]$keyword
#       }
#       invisible(lapply(subjects, function(subject){
#         subject_obj <- geoflow_subject$new(str = subject)
#         entity$addSubject(subject_obj)
#       }))
      
      
      entities <- c(entities, entity)
    
  return(entities)
}


# coco <- handle_contacts_19115(cfg, md$identificationInfo[[1]]$citation$citedResponsibleParty)

handle_contacts_19115 <- function(config, source){
  
#   if(!is(source, "geoflow_contact")){
#     errMsg <- "The argument should be an object of class 'geoflow_contact'"
#     config$logger.error(errMsg)
#     stop(errMsg)
#   }
  the_contact <- source
  contact_obj <- geoflow_contact$new()
  
  contact_obj$setRole(the_contact$role$value)
  if(is.null(the_contact$contactInfo$address$electronicMailAddress)==FALSE) contact_obj$setId(the_contact$contactInfo$address$electronicMailAddress) else contact_obj$setId("-")
  if(is.null(the_contact$contactInfo$address$electronicMailAddress)==FALSE) contact_obj$setEmail(the_contact$contactInfo$address$electronicMailAddress) else contact_obj$setEmail("-")
  if(is.null(the_contact$individualName)==FALSE) contact_obj$setIndividualName(the_contact$individualName) else contact_obj$setIndividualName("-")
  if(is.null(the_contact$organisationName)==FALSE) contact_obj$setOrganizationName(the_contact$organisationName) else contact_obj$setOrganizationName("-")
  if(is.null(the_contact$positionName)==FALSE) contact_obj$setPositionName(the_contact$positionName) else contact_obj$setPositionName("-")
  if(is.null(the_contact$contactInfo$address$deliveryPoint)==FALSE) contact_obj$setPostalAddress(the_contact$contactInfo$address$deliveryPoint) else contact_obj$setPostalAddress("-")
  if(is.null(the_contact$contactInfo$address$city)==FALSE) contact_obj$setCity(the_contact$contactInfo$address$city) else contact_obj$setCity("-")
  if(is.null(the_contact$contactInfo$address$country)==FALSE) contact_obj$setCountry(the_contact$contactInfo$address$country) else contact_obj$setCountry("-")
  # if(is.null(the_contact$contactInfo$address$administrativeArea)==FALSE) contact_obj$setPositionName(the_contact$contactInfo$address$administrativeArea) else contact_obj$setPositionName("-")
  if(is.null(the_contact$contactInfo$address$postalCode)==FALSE) contact_obj$setPostalCode(the_contact$contactInfo$address$postalCode) else contact_obj$setPostalCode("-")
  if(is.null(the_contact$contactInfo$phone$voice)==FALSE) contact_obj$setVoice(the_contact$contactInfo$phone$voice) else contact_obj$setVoice("-")
  if(is.null(the_contact$contactInfo$phone$facsimile)==FALSE) contact_obj$setFacsimile(the_contact$contactInfo$phone$facsimile) else contact_obj$setFacsimile("-")
  if(is.null(the_contact$contactInfo$onlineResource$linkage$value)==FALSE) contact_obj$setWebsiteUrl(the_contact$contactInfo$onlineResource$linkage$value) else contact_obj$setWebsiteUrl("-")
  if(is.null(the_contact$contactInfo$onlineResource$name)==FALSE) contact_obj$setWebsiteName(the_contact$contactInfo$onlineResource$name) else contact_obj$setWebsiteName("-")
  
  return(contact_obj)
}

#         address$setDeliveryPoint(entity_contact$postalAddress)