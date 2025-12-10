#' Extraire la date d'un vecteur de chaînes ISO
#' Sépare une chaîne de type "YYYY-MM-DDTHH:MM:SS" et renvoie uniquement la date.
#' @param chain Un vecteur de chaînes contenant des dates ISO avec un T.
#' @return Un vecteur de dates sous forme de chaînes "YYYY-MM-DD".
fdate=function(chain){
  result=rep(NA,length(chain))
  for (i in 1:length(chain)){
    result[i]=strsplit(as.vector(chain[i]),"T")[[1]][1]
  }
  return(result)
}

#' Convertir un vecteur vide en NA
#' Renvoie NA si le vecteur est de longueur zéro, sinon renvoie le vecteur tel quel.
#' @param x Un vecteur quelconque.
#' @return NA ou le vecteur d'origine.
NAise=function(x){
  if(length(x)==0)return(NA) else return(x)
}

#' Charger un fichier metadata.xml
#' Charge le fichier XML principal d'une fiche metadata dans son dossier.
#' @param xmlpath Chemin du dossier de la fiche metadata.
#'
#' @return Un objet XML.
#' @export
el_get_xml=function(xmlpath){
  file=paste0(xmlpath,
              "/metadata/metadata.xml")
  myxml=xml2::read_xml(file,encoding="iso-8889-1")
  return(myxml)
}

#' Extraire les informations clés d'un fichier metadata XML
#' Récupère l'identifiant, les dates de début et fin, l'emprise spatiale, le nombre d'OSR,
#' les liens en ligne et le nombre de tables disponibles.
#' @param myxml Un objet XML tel que renvoyé par get_xml.
#' @return Un data.frame avec les champs ID_fiche, debut, fin, xmin, xmax, ymin, ymax,
#' nOSR, nlinks, ntables.
#' @export
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

#' Extraire les auteurs d'une fiche metadata
#'
#' Récupère les noms des personnes et des organisations associées à la ressource.
#' @param myxml Un objet XML.
#' @return Un data.frame contenant ID_fiche, R_personnes et R_orgs.
#' @export
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

#' Extraire les mots clés d'une fiche metadata
#' Récupère les mots clés, leur type (FREE, INSPIRE ou ISO) et l'identifiant associé.
#' @param myxml Un objet XML.
#' @return Un data.frame avec ID_fiche, keywords et types.
#' @export
el_get_keywords=function(myxml){
  ID_fiche=myxml %>%
    xml2::xml_find_first(".//gco:CharacterString") %>%
    xml2::xml_text()
  all_keywords=myxml %>%
    xml2::xml_find_all(".//gmd:MD_Keywords")
  keywords=c()
  types=c()
  for (k in 1:length(all_keywords)){
    tkeywords=all_keywords[k] %>%
      xml2::xml_find_all(".//gmd:keyword") %>%
      xml2::xml_text()
    ttypes=rep("FREE",length(tkeywords))
    thesaurus_mentioned=all_keywords[k] %>%
      xml2::xml_find_all(".//gmd:thesaurusName") %>%
      length()
    if(thesaurus_mentioned>0){
      ttypes=rep("INSPIRE",length(tkeywords))
    }

    keywords=c(keywords,tkeywords)
    types=c(types,ttypes)
  }
  iso_keywords=myxml %>%
    xml2::xml_find_all(".//gmd:MD_TopicCategoryCode") %>%
    xml2::xml_text()
  keywords=c(keywords,iso_keywords)
  types=c(types,rep("ISO",length(iso_keywords)))
  #####
  dat_keywords=data.frame(ID_fiche,keywords,types)
  return(dat_keywords)
}

#' Extraire toutes les métadonnées d'un répertoire
#' Parcourt toutes les fiches dans un dossier et assemble les informations, auteurs et mots clés.
#' @param metadata_dir Chemin du dossier contenant les fiches metadata.
#' @return Un data.frame complet avec toutes les métadonnées fusionnées.
#' @export
el_get_metadata=function(metadata_dir){
  fiches=list.files(metadata_dir)
  fiches=paste0(metadata_dir,"/",fiches)
  all_metadata=c()
  for (i in 1:length(fiches)){
    myxml=el_get_xml(fiches[i])
    info=el_get_info(myxml)
    auteurs=el_get_auteurs(myxml)
    keywords=el_get_keywords(myxml)

    dat_tmp=merge(info,auteurs,by="ID_fiche",all=T)
    dat_tmp=merge(dat_tmp,keywords,by="ID_fiche",all=T)
    all_metadata=rbind(all_metadata,dat_tmp)
  }
  return(all_metadata)
}

#' Extraire les catégories d'un ensemble de fiches metadata#'
#' Lit les fichiers info.xml et récupère les catégories associées à chaque fiche.
#' @param metadata_dir Dossier où se trouvent les sous-dossiers contenant info.xml.
#' @return Un data.frame avec les colonnes ID_fiche et categories.
#' @export
el_get_categories=function(metadata_dir){
  files=list.files(metadata_dir)
  dat_categories=c()
  for(i in 1:length(files)){
    file=paste0(metadata_dir,"/",files[i],"/",
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
