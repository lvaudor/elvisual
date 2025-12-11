#' Fournir les comptages de mots-clés
#' @param metadata les métadonnées, obtenues via el_get_metadata()
#' @return Un data.frame avec les mots-clés, leur type, leur fréquence
#' @export
el_table_keywords=function(metadata){
  keywords_data=metadata %>%
    dplyr::select(ID_fiche, keywords,type) %>%
    unique() %>%
    group_by(keywords,type) %>%
    summarise(freq=n()) %>%
    ungroup() %>%
    arrange(desc(freq),keywords)
  return(keywords_data)
}

#' Fournir un tableau avec une ligne=une fiche
#' @param metadata les métadonnées, obtenues via el_get_metadata()
#' @return Un data.frame avec un comptage des auteurs, organisations, mots-clés
#' @export
el_metadata_summary=function(metadata){
  nkeywords=metadata %>%
    dplyr::select(ID_fiche,keywords) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(nkeywords=n())
  npersonnes=metadata %>%
    dplyr::select(ID_fiche,R_personnes) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(npersonnes=n())
  norganisations=metadata %>%
    dplyr::select(ID_fiche,R_orgs) %>%
    unique() %>%
    dplyr::group_by(ID_fiche) %>%
    dplyr::summarise(norganisations=n())
  metadata_summary=metadata %>%
    dplyr::select(-R_orgs,-R_personnes,-keywords,-type) %>%
    unique() %>%
    dplyr::left_join(nkeywords, by="ID_fiche") %>%
    dplyr::left_join(npersonnes,by="ID_fiche") %>%
    dplyr::left_join(norganisations,by="ID_fiche")
  return(metadata_summary)


}
