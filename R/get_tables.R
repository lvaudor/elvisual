#' Fournir les comptages de mots-clés
#' @param metadata les métadonnées, obtenues via el_get_metadata()
#' @return Un data.frame avec les mots-clés, leur type, leur fréquence
#' @export
el_table_keywords=function(metadata){
  keywords_data=all_metadata %>%
    dplyr::select(ID_fiche, keywords,types) %>%
    unique() %>%
    group_by(keywords,types) %>%
    summarise(freq=n()) %>%
    ungroup() %>%
    arrange(desc(freq),keywords)
  return(keywords_data)
}
