# adapted from from https://github.com/juldebar/R_Metadata/blob/master/metadata_workflow_google_doc_Dublin_Core/scripts/write_metadata_EML.R

create_eml <- function(entity, config, options){
  
  if(!require("EML")){
    stop("This action requires the 'EML' package")
  }
  
  config$logger.info("Ay, Caramba")
  
  pubDate <- if(!is.null(entity$date)) as.character(entity$date) else as.character(Sys.Date())
  identifier <- entity$identifiers[["id"]]
  title <- entity$title
  abstract <- if(!is.null(entity$descriptions[["abstract"]])) entity$descriptions[["abstract"]] else "TO BE WRITTEN"
  # intellectualRights <- if(!is.null(entity$rights)) entity$rights[["accessconstraint"]] else entity$rights[sapply("TO BE DONE", "accessconstraint")]
  
  
  config$logger.info("Coverage metadata => TO BE DONE => geographicCoverage / temporalCoverage / taxonomicCoverage.")  
  
  config$logger.info("EML temporalCoverage")  
  if(!is.null(entity$temporal_extent)){
    
    if(!is.null(entity$temporal_extent$instant)){
      beginPosition = entity$temporal_extent$instant
      endPosition = entity$temporal_extent$instant
    }
    
    if(!is.null(entity$temporal_extent$start) & !is.null(entity$temporal_extent$end)){
      beginPosition = entity$temporal_extent$start
      endPosition = entity$temporal_extent$end
    }
    
  }
  
  config$logger.info("EML geographicCoverage")  
  if(!is.null(entity$spatial_extent)){
    sf_bbox <- attr(entity$spatial_extent, "bbox")
    west = sf_bbox$xmin
    east = sf_bbox$ymax
    north = sf_bbox$xmax
    south = sf_bbox$ymin
  }
  
  config$logger.info("set EML coverage")  
  coverage <- set_coverage(
    begin = as.character(beginPosition),
    end = as.character(endPosition),
    sci_names = "Sarracenia purpurea", # TO BE DONE => USE taxonomicCoverage if species or taxon !!!
    geographicDescription = "geographic_identifier",  # TO BE DONE REMOVE i
    west = west,
    east = east,
    north = north,
    south = south,
    altitudeMin = 0, # TO BE DONE
    altitudeMaximum = 0, # TO BE DONE
    altitudeUnits = "meter"
  )
  
  #add keywords
  if(!is.null(entity$subjects)){
    keywords_metadata <- entity$getSubjects(keywords = TRUE, pretty = TRUE)
    different_thesaurus <- unique(keywords_metadata$subject_name)
    number_thesaurus<-length(unique(different_thesaurus))
    all_thesaurus <- vector("list",number_thesaurus)
    # all_thesaurus <- c()
    keywordSet <- c()
    
    for(t in 1:number_thesaurus){
      if(is.null(keywords_metadata)==FALSE){
        number_row_kw<-nrow(keywords_metadata)
        vector <- character(0)
        for (i in 1:number_row_kw) {
          if(keywords_metadata$subject_name[i]==different_thesaurus[t] & !is.na(keywords_metadata$keyword_name[i])){
            vector[[length(vector)+1]] <- keywords_metadata$keyword_name[i]
          }
        }
      }
      all_thesaurus <- new("keywordSet",
                           keywordThesaurus = different_thesaurus[t],
                           keyword = vector)
      keywordSet[[t]]  <-  all_thesaurus
      class(all_thesaurus)
    }
  }
  
  #add contacts
  new_eml_contact=NULL
  for(entity_contact in entity$contacts){
    eml_contact_role <- NULL
    eml_contact_role <- switch(entity_contact$role,
                               "metadata" = "associatedParty",
                               "pointOfContact" = "contact",
                               "principalInvestigator" = "contact",
                               "publisher" = "contact",
                               "owner" = "contact",
                               "originator" = "contact"
    )
    if(is.null(eml_contact_role)){
      config$logger.info("No mapping has been found for the role of the conctact !")  
      #         the_contact <- contacts[contacts$electronicMailAddress%in%contacts_metadata$contacts_roles$contact[i],]
      #         cat(the_contact$electronicMailAddress)
      #         cat(contacts_metadata$contacts_roles$RoleCode[i])
    }
    
    HF_address <- new("address",
                      deliveryPoint = entity_contact$postalAddress,
                      city = entity_contact$city,
                      #administrativeArea = entity_contact$administrativeArea,
                      #postalCode = entity_contact$postalCode,
                      country = entity_contact$country
    )
    
    new_eml_contact <-  new(
      eml_contact_role,
      individualName = entity_contact$individualName,
      electronicMail = entity_contact$email,
      address = HF_address,
      organizationName = entity_contact$organizationName,
      phone = entity_contact$voice
    )
    
  }
  #   else {
  #     new_eml_contact <-  new(
  #       eml_contact_role= "contact",
  #       individualName = "John, Snow",
  #       electronicMail = "john.snow@toto.org",
  #       address = "in the North",
  #       organizationName = "xx"
  #       )  
  #     }
  
  
  
  
  #  config$logger.info("WRITE EML METADATA")  
  dataset <- new("dataset",
                 title = title,
                 creator = new_eml_contact,
                 pubDate = pubDate,
                 intellectualRights = "IPR",
                 abstract = abstract,
                 associatedParty = new_eml_contact,#@julien => select new_eml_contact where role=associatedParty
                 # associatedParty = new_eml_contact[new_eml_contact$eml_contact_role%in%"associatedParty",],
                 keywordSet = keywordSet,
                 coverage = coverage,
                 contact = new_eml_contact,
                 # methods = methods,
                 dataTable = NULL)
  
  eml <- new("eml",
             packageId = entity$identifiers[["id"]],  # from uuid::UUIDgenerate(),
             system = "uuid", # type of identifier
             dataset = dataset)
  
  
  #we save the metadata
  saveRDS(eml, file.path(getwd(), "metadata", paste0(entity$identifiers[["id"]], "_eml.rds")))
  filename <-paste0(entity$identifiers[["id"]], "_eml.xml")
  this_wd <- getwd()
  setwd(file.path(this_wd, "metadata"))
  write_eml(eml, filename)
  eml_validate(filename)
  setwd(this_wd)
  
  return(eml)
}
