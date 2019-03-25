create_Rmd <- function(entity, config, options){
  
  if(!require("knitr")){
    stop("This action requires the 'knitr' package")
  }
  
  output="Rmd"
  
  config$logger.info("Ay, Pepito")
  config$logger.info(getwd())
  
  template_path <-  file.path(getwd(), "template_datapaper.Rmd")
  system(paste0("cp ", file.path(config$wd, "R", "template_datapaper.Rmd"), " ", getwd()))
  template_file <- readLines(template_path,encoding="UTF-8")
  
  #template_file <- sub(pattern = "metadata$logo", replacement = metadata$logo, x = template_file,fixed = TRUE)

  if(!is.null(entity$identifiers)){
    template_file <- sub(pattern = "trash_db_rttp_19115_new", replacement = entity$identifiers[["id"]], x = template_file,fixed = TRUE)
    }
  if(!is.null(entity$title)){
    template_file <- sub(pattern = "metadata$Title", replacement = entity$title, x = template_file,fixed = TRUE)
  }
  
  if(!is.null(entity$Description)){
    template_file <- sub(pattern = "metadata$Description", replacement = entity$descriptions[["abstract"]], x = template_file,fixed = TRUE)
  }
 
  config$logger.info("Calculate the temporal coverage")  
  if(!is.null(entity$temporal_extent)){
    
    if(!is.null(entity$temporal_extent$instant)){
      beginPosition = entity$temporal_extent$instant
      endPosition = entity$temporal_extent$instant
    }
    if(!is.null(entity$temporal_extent$start) & !is.null(entity$temporal_extent$end)){
      beginPosition = entity$temporal_extent$start
      endPosition = entity$temporal_extent$end
    }
    template_file <- sub(pattern = "metadata$temporal_extent", replacement = paste0("from **", beginPosition, "** to **", endPosition,"**"), x = template_file,fixed = TRUE)
  }
  
  config$logger.info("Calculate the spatial coverage")  
  # config$logger.info(entity$spatial_extent)  
  if(!is.null(entity$spatial_extent)){
    geometry=st_geometry(entity$spatial_extent)
    centroid=st_centroid(st_geometry(geometry))
    lon_centroid= st_coordinates(centroid)[1]
    lat_centroid= st_coordinates(centroid)[2]
    
    template_file <- sub(pattern = "metadata$spatial_extent", replacement = geometry, x = template_file,fixed = TRUE)
    template_file <- sub(pattern = "-28.5", replacement = lon_centroid, x = template_file,fixed = TRUE)
    template_file <- sub(pattern = "3.5", replacement = lat_centroid, x = template_file,fixed = TRUE)
  }
  
  
  
  # template_file <- sub(pattern = "metadata$acknoledgment", replacement = "toto", x = template_file,fixed = TRUE)
  # template_file <- sub(pattern = "urls_metadata", replacement = data_paper_add_urls(urls_metadata), x = template_file,fixed = TRUE)
  
  
  
  # add keywords
  keywords_metadata <- entity$getSubjects(keywords = TRUE, pretty = TRUE)
  if(is.null(keywords_metadata)==FALSE){
    different_keywords <- keywords_metadata$keyword_name
    concatenated_keywords <- keywords_metadata$keyword_name[1]
    for (kw in different_keywords[2:length(different_keywords)]){
      concatenated_keywords <- paste0(concatenated_keywords,",",kw)
    }
  }
  template_file <- sub(pattern = "metadata$Subject", replacement = concatenated_keywords, x = template_file,fixed = TRUE)
  
  
  
  # template_file <- sub(pattern = "contacts_metadata$authors", replacement = data_paper_add_contacts_and_roles(config, metadata$Identifier, contacts_metadata$contacts_roles,type="authors"), x = template_file,fixed = TRUE)
  # template_file <- sub(pattern = "contacts_metadata$contacts_roles", replacement = data_paper_add_contacts_and_roles(config, metadata$Identifier, contacts_metadata$contacts_roles,type="contacts"), x = template_file,fixed = TRUE)
  
  
  this_wd <- getwd()
  setwd(file.path(this_wd, "metadata"))
  
  data_paper_namefile <- paste0("data_paper",entity$identifiers[["id"]])
  if(output=="Rnw"){
    data_paper_Rnw_file <- paste0(data_paper_namefile,".Rnw")
    Sweave2knitr(data_paper_Rnw_file)
    write(template_file, file = data_paper_Rnw_file,ncolumns=1)
    knit(paste0(data_paper_namefile,"-knitr.Rnw"))
    # system(paste("pdflatex ",paste0(data_paper_namefile,"-knitr.tex"), sep=""))
    # system(paste("pdflatex ",paste0(data_paper_namefile,"-knitr.tex"), sep=""))
    # system(paste("pdflatex ",data_paper_tex_file, sep=""))
  } else {
    data_paper_Rmd_file <- paste0(data_paper_namefile,".Rmd")
    write(template_file, file = data_paper_Rmd_file,ncolumns=1)
    render(data_paper_Rmd_file, "html_document")
    # render(data_paper_Rmd_file, "pdf_document")
  }
  
  setwd(this_wd)
  
  cat("Rnw template created")
  return(data_paper_namefile)
}
  

