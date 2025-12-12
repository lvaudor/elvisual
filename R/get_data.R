#' Extracts date from formatted ISO date strings
#' @param chain A vector ISO formatted date strings.
#' @return A vector of dates in "YYYY-MM-DD" format.
fdate=function(chain){
  result=rep(NA,length(chain))
  for (i in 1:length(chain)){
    result[i]=strsplit(as.vector(chain[i]),"T")[[1]][1]
  }
  return(result)
}

#' Converts a zero-length vector to NA
#' @param x A vector.
#' @return NA if the vector has length 0, otherwise returns the vector.
NAise=function(x){
  if(length(x)==0)return(NA) else return(x)
}

#' Gets the XML object from a metadata folder
#' @param xmlpath Path to the folder containing the metadata.
#' @return An XML object read from the metadata.xml file.
el_get_xml=function(xmlpath){
  file=paste0(xmlpath,
              "/metadata/metadata.xml")
  myxml=xml2::read_xml(file,encoding="iso-8889-1")
  return(myxml)
}

#' Extract metadata information from an XML object
#' @param myxml An XML object.
#' @return A data.frame containing ID_fiche, debut, fin, xmin, xmax, ymin, ymax, supp, nlinks, ntables.
el_get_info=function(myxml){
  ID_fiche=myxml %>%
    xml2::xml_find_first(".//gco:CharacterString") %>%
    xml2::xml_text()
  debut=myxml %>%
    xml2::xml_find_first(".//gml:beginPosition") %>%
    xml2::xml_text() %>%
    fdate() %>%
    lubridate::ymd()
  fin=myxml %>%
    xml2::xml_find_first(".//gml:endPosition") %>%
    xml2::xml_text() %>%
    fdate() %>%
    lubridate::ymd()
  xmin=myxml %>%
    xml2::xml_find_first(".//gmd:westBoundLongitude") %>%
    xml2::xml_text() %>%
    as.numeric()
  xmax=myxml %>%
    xml2::xml_find_first(".//gmd:eastBoundLongitude") %>%
    xml2::xml_text() %>%
    as.numeric()
  ymin=myxml %>%
    xml2::xml_find_first(".//gmd:southBoundLatitude") %>%
    xml2::xml_text() %>%
    as.numeric()
  ymax=myxml %>%
    xml2::xml_find_first(".//gmd:northBoundLatitude") %>%
    xml2::xml_text() %>%
    as.numeric()
  supp=myxml %>%
    xml2::xml_find_first(".//gmd:supplementalInformation") %>%
    xml2::xml_text()
  links=myxml %>%
    xml2::xml_find_all(".//gmd:CI_OnlineResource") %>%
    xml2::xml_find_all(".//gmd:URL") %>%
    xml2::xml_text() %>%
    unlist()
  nlinks=length(links)
  ntables=links %>%
    stringr::str_detect("(//.xls)|(//.txt)|(//.csv)$") %>%
    which() %>%
    length()
  info=data.frame(ID_fiche,debut,fin,xmin,xmax,ymin,ymax,supp,nlinks,ntables)
  return(info)
}


#' Extracts authors from a metadata XML object
#' @param myxml An XML object.
#' @return A data.frame with ID_fiche, R_personnes, and R_orgs.
el_get_auteurs=function(myxml){
  ID_fiche=myxml %>%
    xml2::xml_find_first(".//gco:CharacterString") %>%
    xml2::xml_text()
  Ressource=myxml %>%
    xml2::xml_find_all(".//gmd:pointOfContact")
  R_personnes=Ressource %>%
    xml2::xml_find_all(".//gmd:individualName") %>%
    xml2::xml_text() %>%
    NAise()
  R_orgs=Ressource %>%
    xml2::xml_find_all(".//gmd:organisationName") %>%
    xml2::xml_text() %>%
    NAise()
  auteurs=data.frame(ID_fiche,R_personnes,R_orgs)
  return(auteurs)
}

#' Extracts keywords from a metadata XML object
#' @param myxml An XML object.
#' @return A data.frame with ID_fiche, keywords, and type.
el_get_keywords=function(myxml){
  ID_fiche=myxml %>%
    xml2::xml_find_first(".//gco:CharacterString") %>%
    xml2::xml_text()
  all_keywords=myxml %>%
    xml2::xml_find_all(".//gmd:MD_Keywords")
  keywords=c()
  type=c()
  for (k in 1:length(all_keywords)){
    tkeywords=all_keywords[k] %>%
      xml2::xml_find_all(".//gmd:keyword") %>%
      xml2::xml_text()
    ttype=rep("FREE",length(tkeywords))
    thesaurus_mentioned=all_keywords[k] %>%
      xml2::xml_find_all(".//gmd:thesaurusName") %>%
      length()
    if(thesaurus_mentioned>0){
      ttype=rep("INSPIRE",length(tkeywords))
    }

    keywords=c(keywords,tkeywords)
    type=c(type,ttype)
  }
  iso_keywords=myxml %>%
    xml2::xml_find_all(".//gmd:MD_TopicCategoryCode") %>%
    xml2::xml_text()
  keywords=c(keywords,iso_keywords)
  type=c(type,rep("ISO",length(iso_keywords)))
  #####
  dat_keywords=data.frame(ID_fiche,keywords,type)
  return(dat_keywords)
}

#' Extract all metadata from a directory of flashcards
#' @param data_dir Directory containing the flashcard folders.
#' @return A data.frame with all extracted metadata.
#' @export
el_data=function(data_dir){
  fiches=list.files(data_dir)
  fiches=paste0(data_dir,"/",fiches)
  all_data=c()
  for (i in 1:length(fiches)){
    myxml=el_get_xml(fiches[i])
    info=el_get_info(myxml)
    auteurs=el_get_auteurs(myxml)
    keywords=el_get_keywords(myxml)

    dat_tmp=merge(info,auteurs,by="ID_fiche",all=T)
    dat_tmp=merge(dat_tmp,keywords,by="ID_fiche",all=T)
    all_data=rbind(all_data,dat_tmp)
  }
  return(all_data)
}

#' Extract categories from metadata files in a directory
#' @param data_dir Directory containing the metadata folders.
#' @return A data.frame with ID_fiche and categories.
#' @export
el_categories=function(data_dir){
  files=list.files(data_dir)
  dat_categories=c()
  for(i in 1:length(files)){
    file=paste0(data_dir,"/",files[i],"/",
                "info.xml")
    myxml=file %>%
      xml2::read_xml(encoding="iso-8889-1")
    categories=myxml %>%
      xml2::xml_find_all(".//category") %>%
      xml2::xml_attr("name")
    result=data.frame(ID_fiche=rep(files[i],length(categories)),
                      categories=categories)
    dat_categories=rbind(dat_categories,result)
  }
  return(dat_categories)
}
